#include "multimod/block.h"

#include <chrono>
#include <cmath>
#include <stdexcept>

#include "multimod/parallel.h"

namespace multimod {

namespace {
struct Timer {
  std::map<std::string, double>& stats;
  std::string key;
  std::chrono::steady_clock::time_point t0;
  Timer(std::map<std::string, double>& s, std::string k) : stats(s), key(std::move(k)), t0(std::chrono::steady_clock::now()) {}
  ~Timer() { stats[key] += std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count(); }
};

std::vector<std::string> table_side(const OnPairs& on) {
  std::vector<std::string> out;
  for (const auto& p : on) out.push_back(p.second);
  return out;
}

// positions of the set entries of a mask, by a count pass and a fill pass
std::vector<int64_t> mask_positions(const std::vector<char>& keep, int threads) {
  int64_t n = static_cast<int64_t>(keep.size());
  std::vector<int64_t> bounds = chunk_bounds(n, threads);
  int t = static_cast<int>(bounds.size()) - 1;
  std::vector<int64_t> counts(t, 0);
  parallel_chunks(bounds, [&](int w, int64_t b, int64_t e) {
    int64_t c = 0;
    for (int64_t r = b; r < e; ++r) c += keep[r] != 0;
    counts[w] = c;
  });
  std::vector<int64_t> offset(t + 1, 0);
  for (int w = 0; w < t; ++w) offset[w + 1] = offset[w] + counts[w];
  std::vector<int64_t> pos(offset[t]);
  parallel_chunks(bounds, [&](int w, int64_t b, int64_t e) {
    int64_t o = offset[w];
    for (int64_t r = b; r < e; ++r) if (keep[r]) pos[o++] = r;
  });
  return pos;
}

template <class T>
void gather(std::vector<T>& v, const std::vector<int64_t>& pos, int threads) {
  int64_t m = static_cast<int64_t>(pos.size());
  std::vector<T> out(m);
  parallel_for(m, threads, [&](int64_t b, int64_t e, int) {
    for (int64_t r = b; r < e; ++r) out[r] = v[pos[r]];
  });
  v.swap(out);
}
}  // namespace

int Env::col(const std::string& name) const {
  for (size_t c = 0; c < names.size(); ++c) if (names[c] == name) return static_cast<int>(c);
  return -1;
}

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

bool Executor::add_table(const std::string& name, Table t) {
  if (tables_.count(name)) return false;
  tables_[name] = std::move(t);
  return true;
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

// keep the rows flagged in the mask, compacting every column in parallel
void Executor::keep_mask(Env& env, const std::vector<char>& keep) {
  std::vector<int64_t> pos = mask_positions(keep, threads_);
  gather(env.k, pos, threads_);
  gather(env.coef, pos, threads_);
  for (auto& col : env.cols) gather(col, pos, threads_);
  if (env.has_j) gather(env.j, pos, threads_);
  if (!env.origin.empty()) gather(env.origin, pos, threads_);
  env.n = static_cast<int64_t>(pos.size());
}

void Executor::gate(Env& env, const Step& s) {
  const KeyIndex& ix = index(s.table, table_side(s.on));
  KeyCols pc = probe_cols(env, s.on);
  std::vector<char> keep(env.n);
  parallel_for(env.n, threads_, [&](int64_t b, int64_t e, int) {
    for (int64_t r = b; r < e; ++r) keep[r] = ix.find(pc, r) >= 0;
  });
  keep_mask(env, keep);
}

void Executor::expand(Env& env, const Step& s) {
  const Table& t = table(s.table);
  int64_t n = env.n;
  // pass 1: matches per row; pass 2: fill at prefix-sum offsets
  std::vector<int64_t> count(n + 1, 0);
  const KeyIndex* ix = nullptr;
  KeyCols pc;
  if (s.on.empty()) {
    for (int64_t r = 0; r < n; ++r) count[r + 1] = t.n;
  } else {
    ix = &index(s.table, table_side(s.on));
    pc = probe_cols(env, s.on);
    parallel_for(n, threads_, [&](int64_t b, int64_t e, int) {
      for (int64_t r = b; r < e; ++r) {
        int64_t c = 0;
        for (int64_t q = ix->find(pc, r); q >= 0; q = ix->next[q]) ++c;
        count[r + 1] = c;
      }
    });
  }
  for (int64_t r = 0; r < n; ++r) count[r + 1] += count[r];
  int64_t m = count[n];
  std::vector<int64_t> src(m), hit(m);
  parallel_for(n, threads_, [&](int64_t b, int64_t e, int) {
    for (int64_t r = b; r < e; ++r) {
      int64_t o = count[r];
      if (s.on.empty()) {
        for (int64_t q = 0; q < t.n; ++q) { src[o] = r; hit[o] = q; ++o; }
      } else {
        for (int64_t q = ix->find(pc, r); q >= 0; q = ix->next[q]) { src[o] = r; hit[o] = q; ++o; }
      }
    }
  });
  gather(env.k, src, threads_);
  gather(env.coef, src, threads_);
  for (auto& col : env.cols) gather(col, src, threads_);
  if (env.has_j) gather(env.j, src, threads_);
  if (!env.origin.empty()) gather(env.origin, src, threads_);
  env.n = m;
  for (const auto& b : s.bind) {
    auto ic = t.icols.find(b.second);
    if (ic == t.icols.end()) throw std::runtime_error("table " + s.table + " has no integer column " + b.second);
    std::vector<int> v(m);
    const int* col = ic->second;
    parallel_for(m, threads_, [&](int64_t bb, int64_t ee, int) {
      for (int64_t r = bb; r < ee; ++r) v[r] = col[hit[r]];
    });
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
      const double* val = dc->second;
      if (e.on.empty()) {              // a scalar parameter: its one stored value
        double v = t.n ? val[0] : e.value;
        for (int64_t r = 0; r < env.n; ++r) out[r] = v;
        break;
      }
      const KeyIndex& ix = index(e.table, table_side(e.on));
      KeyCols pc = probe_cols(env, e.on);
      double dflt = e.value;
      parallel_for(env.n, threads_, [&](int64_t b, int64_t en, int) {
        for (int64_t r = b; r < en; ++r) {
          int64_t q = ix.find(pc, r);
          out[r] = q < 0 ? dflt : val[q];
        }
      });
      break;
    }
    case NumExpr::Kind::Binary: {
      std::vector<double> a = eval_num(*e.a, env), b = eval_num(*e.b, env);
      char op = e.op;
      parallel_for(env.n, threads_, [&](int64_t bb, int64_t ee, int) {
        switch (op) {
          case '+': for (int64_t r = bb; r < ee; ++r) out[r] = a[r] + b[r]; break;
          case '-': for (int64_t r = bb; r < ee; ++r) out[r] = a[r] - b[r]; break;
          case '*': for (int64_t r = bb; r < ee; ++r) out[r] = a[r] * b[r]; break;
          case '/': for (int64_t r = bb; r < ee; ++r) out[r] = a[r] / b[r]; break;
          case '^': for (int64_t r = bb; r < ee; ++r) out[r] = std::pow(a[r], b[r]); break;
          default: throw std::runtime_error(std::string("unknown operator ") + op);
        }
      });
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
      int code = op == "<" ? 0 : op == "<=" ? 1 : op == ">" ? 2 : op == ">=" ? 3 : op == "==" ? 4 : op == "!=" ? 5 : -1;
      if (code < 0) throw std::runtime_error("unknown comparison " + op);
      for (int64_t r = 0; r < env.n; ++r) {
        bool v = false;
        switch (code) {
          case 0: v = x[r] < y[r]; break;
          case 1: v = x[r] <= y[r]; break;
          case 2: v = x[r] > y[r]; break;
          case 3: v = x[r] >= y[r]; break;
          case 4: v = x[r] == y[r]; break;
          case 5: v = x[r] != y[r]; break;
        }
        out[r] = v;
      }
      break;
    }
    case Cond::Kind::Member: {
      const KeyIndex& ix = index(c.table, table_side(c.on));
      KeyCols pc = probe_cols(env, c.on);
      parallel_for(env.n, threads_, [&](int64_t b, int64_t e, int) {
        for (int64_t r = b; r < e; ++r) out[r] = ix.find(pc, r) >= 0;
      });
      break;
    }
  }
  return out;
}

void Executor::filter(Env& env, const Step& s) {
  std::vector<char> keep = eval_cond(*s.cond, env);
  keep_mask(env, keep);
}

void Executor::factor(Env& env, const Step& s) {
  std::vector<double> v = eval_num(*s.expr, env);
  bool inv = s.invert;
  parallel_for(env.n, threads_, [&](int64_t b, int64_t e, int) {
    if (inv) for (int64_t r = b; r < e; ++r) env.coef[r] /= v[r];
    else for (int64_t r = b; r < e; ++r) env.coef[r] *= v[r];
  });
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
  const int* jcol = jc->second;
  env.j.assign(env.n, INT_MIN);
  std::vector<char> keep(env.n);
  parallel_for(env.n, threads_, [&](int64_t b, int64_t e, int) {
    for (int64_t r = b; r < e; ++r) {
      int64_t q = ix.find(pc, r);
      keep[r] = q >= 0;
      if (q >= 0) env.j[r] = jcol[q];
    }
  });
  keep_mask(env, keep);             // no column there -> no term
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

// Sum of w by (a, b) (or by a alone when b is empty), partitioned by key
// hash so that every partition is grouped on its own thread. Output order
// is by partition, then first occurrence.
void Executor::group_sum(const std::vector<int>& a, const std::vector<int>& b, const std::vector<double>& w,
                         std::vector<int>& oa, std::vector<int>& ob, std::vector<double>& os, bool drop_zero) {
  int64_t n = static_cast<int64_t>(a.size());
  bool two = !b.empty();
  int parts = std::max(1, std::min(threads_, 64));
  if (n < 65536) parts = 1;
  // partition of each entry
  std::vector<int> part(n);
  std::vector<int64_t> bounds = chunk_bounds(n, threads_);
  int nchunks = static_cast<int>(bounds.size()) - 1;
  std::vector<int64_t> counts(static_cast<size_t>(nchunks) * parts, 0);
  parallel_chunks(bounds, [&](int c, int bb, int64_t ee) {
    (void)bb;
    for (int64_t r = bounds[c]; r < ee; ++r) {
      uint64_t k = static_cast<uint32_t>(a[r]);
      if (two) k = (k << 32) | static_cast<uint32_t>(b[r]);
      int p = parts == 1 ? 0 : static_cast<int>(mix64(k) % static_cast<uint64_t>(parts));
      part[r] = p;
      counts[static_cast<size_t>(c) * parts + p]++;
    }
  });
  // offsets: by partition, then by chunk
  std::vector<int64_t> pstart(parts + 1, 0);
  for (int p = 0; p < parts; ++p) {
    int64_t s = 0;
    for (int c = 0; c < nchunks; ++c) s += counts[static_cast<size_t>(c) * parts + p];
    pstart[p + 1] = pstart[p] + s;
  }
  std::vector<int64_t> off(static_cast<size_t>(nchunks) * parts, 0);
  for (int p = 0; p < parts; ++p) {
    int64_t o = pstart[p];
    for (int c = 0; c < nchunks; ++c) { off[static_cast<size_t>(c) * parts + p] = o; o += counts[static_cast<size_t>(c) * parts + p]; }
  }
  std::vector<int> pa(n), pb(two ? n : 0); std::vector<double> pw(n);
  parallel_chunks(bounds, [&](int c, int64_t bb, int64_t ee) {
    std::vector<int64_t> o(off.begin() + static_cast<int64_t>(c) * parts, off.begin() + static_cast<int64_t>(c + 1) * parts);
    for (int64_t r = bb; r < ee; ++r) {
      int64_t d = o[part[r]]++;
      pa[d] = a[r]; if (two) pb[d] = b[r]; pw[d] = w[r];
    }
  });
  // group within each partition
  std::vector<std::vector<int>> ra(parts), rb(parts);
  std::vector<std::vector<double>> rs(parts);
  parallel_for(parts, parts, [&](int64_t pb0, int64_t pe, int) {
    for (int64_t p = pb0; p < pe; ++p) {
      int64_t lo = pstart[p], hi = pstart[p + 1];
      int64_t m = hi - lo;
      if (m == 0) continue;
      KeyCols g = two ? KeyCols{pa.data() + lo, pb.data() + lo} : KeyCols{pa.data() + lo};
      KeyIndex ix; ix.na_matches = true; ix.build(g, m);
      std::vector<int64_t> group_of(m, -1);
      std::vector<int64_t> rep; std::vector<double> sum;
      for (int64_t r = 0; r < m; ++r) {
        int64_t s = ix.find(g, r);
        if (group_of[s] < 0) { group_of[s] = static_cast<int64_t>(rep.size()); rep.push_back(s); sum.push_back(0.0); }
        sum[group_of[s]] += pw[lo + r];
      }
      for (size_t gi = 0; gi < rep.size(); ++gi) {
        if (drop_zero && sum[gi] == 0.0) continue;
        ra[p].push_back(pa[lo + rep[gi]]);
        if (two) rb[p].push_back(pb[lo + rep[gi]]);
        rs[p].push_back(sum[gi]);
      }
    }
  });
  size_t total = 0;
  for (int p = 0; p < parts; ++p) total += rs[p].size();
  oa.reserve(total); os.reserve(total); if (two) ob.reserve(total);
  for (int p = 0; p < parts; ++p) {
    oa.insert(oa.end(), ra[p].begin(), ra[p].end());
    if (two) ob.insert(ob.end(), rb[p].begin(), rb[p].end());
    os.insert(os.end(), rs[p].begin(), rs[p].end());
  }
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
  if (!ai.empty()) group_sum(ai, aj, ac, out.i, out.j, out.coef, true);
  if (!ci.empty()) {
    std::vector<int> none, nb;
    std::vector<double> sums;
    group_sum(ci, none, cc, out.rhs_i, nb, sums, false);
    out.rhs.resize(sums.size());
    for (size_t k = 0; k < sums.size(); ++k) out.rhs[k] = -sums[k];   // constants move to the right-hand side
  }
  return out;
}

}  // namespace multimod
