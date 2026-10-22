#include "multimod/block.h"

#include <chrono>
#include <cmath>
#include <stdexcept>

namespace multimod {

namespace {
struct Timer {
  std::map<std::string, double>& stats;
  std::string key;
  std::chrono::steady_clock::time_point t0;
  Timer(std::map<std::string, double>& s, std::string k) : stats(s), key(std::move(k)), t0(std::chrono::steady_clock::now()) {}
  ~Timer() { stats[key] += std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count(); }
};
}  // namespace

void Executor::set_table(const std::string& name, Table t) {
  tables_[name] = std::move(t);
  // indexes built on the replaced table are stale
  for (auto it = index_cache_.begin(); it != index_cache_.end();) {
    const std::string& key = it->first;
    if (key.compare(0, name.size(), name) == 0 && (key.size() == name.size() || key[name.size()] == '\x1f')) {
      it = index_cache_.erase(it);
    } else {
      ++it;
    }
  }
}

int Env::col(const std::string& name) const {
  for (size_t c = 0; c < names.size(); ++c) if (names[c] == name) return static_cast<int>(c);
  return -1;
}

const Table& Executor::table(const std::string& name) const {
  auto it = tables_.find(name);
  if (it == tables_.end()) throw std::runtime_error("block plan refers to a table that was not supplied: " + name);
  return it->second;
}

const KeyIndex& Executor::index(const std::string& tname, const std::vector<std::string>& cols) {
  std::string key = tname;
  for (const auto& c : cols) { key += '\x1f'; key += c; }
  auto it = index_cache_.find(key);
  if (it != index_cache_.end()) return *it->second;
  Timer tm(stats_, "index");
  const Table& t = table(tname);
  KeyCols kc;
  for (const auto& c : cols) {
    auto ic = t.icols.find(c);
    if (ic == t.icols.end()) throw std::runtime_error("table " + tname + " has no integer column " + c);
    kc.push_back(ic->second);
  }
  auto ix = std::make_unique<KeyIndex>();
  ix->build(kc, t.n);
  const KeyIndex& ref = *ix;
  index_cache_[key] = std::move(ix);
  return ref;
}

KeyCols Executor::probe_cols(const Env& env, const OnPairs& on) const {
  KeyCols pc;
  for (const auto& p : on) {
    int c = env.col(p.first);
    if (c < 0) throw std::runtime_error("block plan joins on an unbound index: " + p.first);
    pc.push_back(env.cols[c].data());
  }
  return pc;
}

static std::vector<std::string> table_side(const OnPairs& on) {
  std::vector<std::string> out;
  for (const auto& p : on) out.push_back(p.second);
  return out;
}

// keep the listed rows, compacting every column
static void keep_rows(Env& env, const std::vector<int64_t>& keep) {
  int64_t m = static_cast<int64_t>(keep.size());
  std::vector<int> k(m); std::vector<double> coef(m);
  for (int64_t r = 0; r < m; ++r) { k[r] = env.k[keep[r]]; coef[r] = env.coef[keep[r]]; }
  env.k.swap(k); env.coef.swap(coef);
  for (auto& col : env.cols) {
    std::vector<int> v(m);
    for (int64_t r = 0; r < m; ++r) v[r] = col[keep[r]];
    col.swap(v);
  }
  if (env.has_j) {
    std::vector<int> v(m);
    for (int64_t r = 0; r < m; ++r) v[r] = env.j[keep[r]];
    env.j.swap(v);
  }
  if (!env.origin.empty()) {
    std::vector<int64_t> v(m);
    for (int64_t r = 0; r < m; ++r) v[r] = env.origin[keep[r]];
    env.origin.swap(v);
  }
  env.n = m;
}

void Executor::gate(Env& env, const Step& s) {
  const KeyIndex& ix = index(s.table, table_side(s.on));
  KeyCols pc = probe_cols(env, s.on);
  std::vector<int64_t> keep;
  keep.reserve(env.n);
  for (int64_t r = 0; r < env.n; ++r) if (ix.find(pc, r) >= 0) keep.push_back(r);
  keep_rows(env, keep);
}

void Executor::expand(Env& env, const Step& s) {
  const Table& t = table(s.table);
  std::vector<int64_t> src, hit;
  if (s.on.empty()) {
    // no shared key: every row pairs with every table row (a dense cross)
    src.reserve(env.n * t.n); hit.reserve(env.n * t.n);
    for (int64_t r = 0; r < env.n; ++r)
      for (int64_t q = 0; q < t.n; ++q) { src.push_back(r); hit.push_back(q); }
  } else {
    const KeyIndex& ix = index(s.table, table_side(s.on));
    KeyCols pc = probe_cols(env, s.on);
    src.reserve(env.n); hit.reserve(env.n);
    for (int64_t r = 0; r < env.n; ++r)
      for (int64_t q = ix.find(pc, r); q >= 0; q = ix.next[q]) { src.push_back(r); hit.push_back(q); }
  }
  keep_rows(env, src);
  for (const auto& b : s.bind) {
    auto ic = t.icols.find(b.second);
    if (ic == t.icols.end()) throw std::runtime_error("table " + s.table + " has no integer column " + b.second);
    std::vector<int> v(env.n);
    for (int64_t r = 0; r < env.n; ++r) v[r] = ic->second[hit[r]];
    env.names.push_back(b.first);
    env.cols.push_back(std::move(v));
  }
}

std::vector<double> Executor::eval_num(const NumExpr& e, const Env& env) {
  std::vector<double> out(env.n);
  switch (e.kind) {
    case NumExpr::Kind::Const:
      for (int64_t r = 0; r < env.n; ++r) out[r] = e.value;
      break;
    case NumExpr::Kind::Param: {
      const Table& t = table(e.table);
      auto dc = t.dcols.find(e.column);
      if (dc == t.dcols.end()) throw std::runtime_error("table " + e.table + " has no value column " + e.column);
      if (e.on.empty()) {              // a scalar parameter: its one stored value
        double v = t.n ? dc->second[0] : e.value;
        for (int64_t r = 0; r < env.n; ++r) out[r] = v;
        break;
      }
      const KeyIndex& ix = index(e.table, table_side(e.on));
      KeyCols pc = probe_cols(env, e.on);
      for (int64_t r = 0; r < env.n; ++r) {
        int64_t q = ix.find(pc, r);
        out[r] = q < 0 ? e.value : dc->second[q];
      }
      break;
    }
    case NumExpr::Kind::Binary: {
      std::vector<double> a = eval_num(*e.a, env), b = eval_num(*e.b, env);
      switch (e.op) {
        case '+': for (int64_t r = 0; r < env.n; ++r) out[r] = a[r] + b[r]; break;
        case '-': for (int64_t r = 0; r < env.n; ++r) out[r] = a[r] - b[r]; break;
        case '*': for (int64_t r = 0; r < env.n; ++r) out[r] = a[r] * b[r]; break;
        case '/': for (int64_t r = 0; r < env.n; ++r) out[r] = a[r] / b[r]; break;
        case '^': for (int64_t r = 0; r < env.n; ++r) out[r] = std::pow(a[r], b[r]); break;
        default: throw std::runtime_error(std::string("unknown operator ") + e.op);
      }
      break;
    }
    case NumExpr::Kind::When: {
      std::vector<char> c = eval_cond(*e.cond, env);
      std::vector<double> a = eval_num(*e.a, env);
      std::vector<double> b = e.b ? eval_num(*e.b, env) : std::vector<double>(env.n, 0.0);
      for (int64_t r = 0; r < env.n; ++r) out[r] = c[r] ? a[r] : b[r];
      break;
    }
    case NumExpr::Kind::Agg: {
      // the body is evaluated over an expansion of this environment and
      // folded back onto the originating rows; an empty sum is 0, an empty
      // product is 1
      Env sub;
      sub.n = env.n;
      sub.k = env.k;
      sub.coef.assign(env.n, 1.0);
      sub.names = env.names;
      sub.cols = env.cols;
      sub.origin.resize(env.n);
      for (int64_t r = 0; r < env.n; ++r) sub.origin[r] = r;
      for (const Step& s : e.steps) {
        if (sub.n == 0) break;
        step(sub, s);
      }
      double unit = e.op == '*' ? 1.0 : 0.0;
      for (int64_t r = 0; r < env.n; ++r) out[r] = unit;
      if (sub.n > 0) {
        std::vector<double> v = eval_num(*e.a, sub);
        if (e.op == '*') for (int64_t r = 0; r < sub.n; ++r) out[sub.origin[r]] *= v[r];
        else for (int64_t r = 0; r < sub.n; ++r) out[sub.origin[r]] += v[r];
      }
      break;
    }
  }
  return out;
}

void Executor::step(Env& env, const Step& s) {
  switch (s.op) {
    case StepOp::Gate:   { Timer t(stats_, "gate");   gate(env, s); break; }
    case StepOp::Expand: { Timer t(stats_, "expand"); expand(env, s); break; }
    case StepOp::Filter: { Timer t(stats_, "filter"); filter(env, s); break; }
    case StepOp::Factor: { Timer t(stats_, "factor"); factor(env, s); break; }
    case StepOp::Var:    { Timer t(stats_, "var");    var(env, s); break; }
  }
}

std::vector<char> Executor::eval_cond(const Cond& c, const Env& env) {
  std::vector<char> out(env.n);
  switch (c.kind) {
    case Cond::Kind::And: {
      std::vector<char> a = eval_cond(*c.a, env), b = eval_cond(*c.b, env);
      for (int64_t r = 0; r < env.n; ++r) out[r] = a[r] && b[r];
      break;
    }
    case Cond::Kind::Or: {
      std::vector<char> a = eval_cond(*c.a, env), b = eval_cond(*c.b, env);
      for (int64_t r = 0; r < env.n; ++r) out[r] = a[r] || b[r];
      break;
    }
    case Cond::Kind::Not: {
      std::vector<char> a = eval_cond(*c.a, env);
      for (int64_t r = 0; r < env.n; ++r) out[r] = !a[r];
      break;
    }
    case Cond::Kind::Cmp: {
      std::vector<double> x = eval_num(*c.x, env), y = eval_num(*c.y, env);
      const std::string& op = c.op;
      for (int64_t r = 0; r < env.n; ++r) {
        bool v;
        if (op == "<") v = x[r] < y[r];
        else if (op == "<=") v = x[r] <= y[r];
        else if (op == ">") v = x[r] > y[r];
        else if (op == ">=") v = x[r] >= y[r];
        else if (op == "==") v = x[r] == y[r];
        else if (op == "!=") v = x[r] != y[r];
        else throw std::runtime_error("unknown comparison " + op);
        out[r] = v;
      }
      break;
    }
    case Cond::Kind::Member: {
      const KeyIndex& ix = index(c.table, table_side(c.on));
      KeyCols pc = probe_cols(env, c.on);
      for (int64_t r = 0; r < env.n; ++r) out[r] = ix.find(pc, r) >= 0;
      break;
    }
  }
  return out;
}

void Executor::filter(Env& env, const Step& s) {
  std::vector<char> keep = eval_cond(*s.cond, env);
  std::vector<int64_t> rows;
  rows.reserve(env.n);
  for (int64_t r = 0; r < env.n; ++r) if (keep[r]) rows.push_back(r);
  keep_rows(env, rows);
}

void Executor::factor(Env& env, const Step& s) {
  std::vector<double> v = eval_num(*s.expr, env);
  if (s.invert) for (int64_t r = 0; r < env.n; ++r) env.coef[r] /= v[r];
  else for (int64_t r = 0; r < env.n; ++r) env.coef[r] *= v[r];
}

void Executor::var(Env& env, const Step& s) {
  if (env.has_j) throw std::runtime_error("a term refers to two variables; the model is not linear");
  const Table& t = table(s.table);
  auto jc = t.icols.find(s.column);
  if (jc == t.icols.end()) throw std::runtime_error("table " + s.table + " has no column " + s.column);
  env.has_j = true;
  if (s.on.empty()) {               // scalar variable: one column for every row
    env.j.assign(env.n, t.n ? jc->second[0] : INT_MIN);
    return;
  }
  const KeyIndex& ix = index(s.table, table_side(s.on));
  KeyCols pc = probe_cols(env, s.on);
  std::vector<int64_t> keep;
  keep.reserve(env.n);
  env.j.assign(env.n, INT_MIN);
  for (int64_t r = 0; r < env.n; ++r) {
    int64_t q = ix.find(pc, r);
    if (q >= 0) { env.j[r] = jc->second[q]; keep.push_back(r); }
  }
  keep_rows(env, keep);             // no column there -> no term
}

template <class Acc>
static void group_sum(const std::vector<int>& a, const std::vector<int>& b, const std::vector<double>& w, Acc emit) {
  KeyCols g = b.empty() ? KeyCols{a.data()} : KeyCols{a.data(), b.data()};
  KeyIndex ix; ix.na_matches = true; ix.build(g, static_cast<int64_t>(a.size()));
  std::vector<int64_t> rep; std::vector<double> sum;
  std::vector<int64_t> group_of(a.size(), -1);
  for (int64_t r = 0; r < static_cast<int64_t>(a.size()); ++r) {
    int64_t s = ix.find(g, r);
    if (group_of[s] < 0) { group_of[s] = static_cast<int64_t>(rep.size()); rep.push_back(s); sum.push_back(0.0); }
    sum[group_of[s]] += w[r];
  }
  for (size_t gi = 0; gi < rep.size(); ++gi) emit(rep[gi], sum[gi]);
}

Triplets Executor::run(const Plan& plan) {
  Triplets out;
  {
    Timer trun(stats_, "total");
    out = run_terms(plan);
  }
  for (const auto& kv : stats_) out.stats.emplace_back(kv.first, kv.second);
  stats_.clear();
  return out;
}

Triplets Executor::run_terms(const Plan& plan) {
  const Table& rows = table(plan.rows);
  auto kc = rows.icols.find("k");
  if (kc == rows.icols.end()) throw std::runtime_error("row table has no k column");
  if (rows.n == 0) return Triplets();

  std::vector<int> ai, aj; std::vector<double> ac;   // matrix entries
  std::vector<int> ci; std::vector<double> cc;       // constant terms

  for (const Term& term : plan.terms) {
    Env env;
    env.n = rows.n;
    env.k.assign(kc->second, kc->second + rows.n);
    env.coef.assign(rows.n, term.sign);
    for (const auto& name : plan.row_keys) {
      auto ic = rows.icols.find(name);
      if (ic == rows.icols.end()) throw std::runtime_error("row table has no key column " + name);
      env.names.push_back(name);
      env.cols.emplace_back(ic->second, ic->second + rows.n);
    }
    for (const Step& s : term.steps) {
      if (env.n == 0) break;
      step(env, s);
    }
    if (env.n == 0) continue;
    if (env.has_j) {
      ai.insert(ai.end(), env.k.begin(), env.k.end());
      aj.insert(aj.end(), env.j.begin(), env.j.end());
      ac.insert(ac.end(), env.coef.begin(), env.coef.end());
    } else {
      ci.insert(ci.end(), env.k.begin(), env.k.end());
      cc.insert(cc.end(), env.coef.begin(), env.coef.end());
    }
  }

  Triplets out;
  Timer tg(stats_, "groupsum");
  // sum per (row, column); exact zeros are dropped, as the R engine does
  if (!ai.empty()) {
    group_sum(ai, aj, ac, [&](int64_t rep, double sum) {
      if (sum == 0.0) return;
      out.i.push_back(ai[rep]); out.j.push_back(aj[rep]); out.coef.push_back(sum);
    });
  }
  if (!ci.empty()) {
    std::vector<int> none;
    group_sum(ci, none, cc, [&](int64_t rep, double sum) {
      out.rhs_i.push_back(ci[rep]);
      out.rhs.push_back(-sum);   // constants move to the right-hand side
    });
  }
  return out;
}

}  // namespace multimod
