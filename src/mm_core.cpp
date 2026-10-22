// R binding of the native core: a block plan executed over R vectors.
//
// Tables arrive as a named list of named lists of columns; integer columns are
// join keys or column ids, double columns are values. The vectors are read in
// place, never copied. The plan arrives as nested lists mirroring
// multimod::Plan (see R/mx_plan.R for the lowering that writes them).
#include <cpp11.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/external_pointer.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/list.hpp>
#include <cpp11/strings.hpp>

#include <cstring>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "multimod/block.h"

using namespace cpp11;

namespace {

multimod::OnPairs pairs_of(sexp x) {
  multimod::OnPairs out;
  if (x == R_NilValue) return out;
  strings v(x);
  if (v.size() == 0) return out;
  strings nm(v.names());
  for (R_xlen_t i = 0; i < v.size(); ++i) out.emplace_back(std::string(nm[i]), std::string(v[i]));
  return out;
}

std::string str_of(sexp x) {
  if (x == R_NilValue) return "";
  return std::string(strings(x)[0]);
}

double num_of(sexp x, double dflt) {
  if (x == R_NilValue) return dflt;
  return doubles(x)[0];
}

std::vector<std::string> strs_of(sexp x) {
  std::vector<std::string> out;
  if (x == R_NilValue) return out;
  strings v(x);
  for (R_xlen_t i = 0; i < v.size(); ++i) out.emplace_back(v[i]);
  return out;
}

bool flag_of(sexp x) { return x != R_NilValue && Rf_asLogical(x) == TRUE; }

std::unique_ptr<multimod::Cond> cond_of(sexp x);
std::unique_ptr<multimod::NumExpr> num_expr_of(sexp x);

multimod::Step step_of(list st) {
  multimod::Step step;
  std::string op = str_of(st["op"]);
  if (op == "gate") step.op = multimod::StepOp::Gate;
  else if (op == "expand") step.op = multimod::StepOp::Expand;
  else if (op == "filter") step.op = multimod::StepOp::Filter;
  else if (op == "factor") step.op = multimod::StepOp::Factor;
  else if (op == "var") step.op = multimod::StepOp::Var;
  else stop("unknown plan step: %s", op.c_str());
  step.table = str_of(st["table"]);
  step.on = pairs_of(st["on"]);
  step.bind = pairs_of(st["bind"]);
  step.column = str_of(st["column"]);
  step.expr = num_expr_of(st["expr"]);
  step.cond = cond_of(st["cond"]);
  step.invert = flag_of(st["invert"]);
  return step;
}

std::vector<multimod::Step> steps_of(sexp x) {
  std::vector<multimod::Step> out;
  if (x == R_NilValue) return out;
  list steps(x);
  for (R_xlen_t s = 0; s < steps.size(); ++s) out.push_back(step_of(list(steps[s])));
  return out;
}

std::unique_ptr<multimod::NumExpr> num_expr_of(sexp x) {
  if (x == R_NilValue) return nullptr;
  list l(x);
  auto e = std::make_unique<multimod::NumExpr>();
  std::string kind = str_of(l["kind"]);
  if (kind == "const") {
    e->kind = multimod::NumExpr::Kind::Const;
    e->value = num_of(l["value"], 0.0);
  } else if (kind == "param") {
    e->kind = multimod::NumExpr::Kind::Param;
    e->table = str_of(l["table"]);
    e->on = pairs_of(l["on"]);
    e->column = str_of(l["column"]);
    e->value = num_of(l["default"], 0.0);
  } else if (kind == "bin") {
    e->kind = multimod::NumExpr::Kind::Binary;
    e->op = str_of(l["op"])[0];
    e->a = num_expr_of(l["a"]);
    e->b = num_expr_of(l["b"]);
  } else if (kind == "when") {
    e->kind = multimod::NumExpr::Kind::When;
    e->cond = cond_of(l["cond"]);
    e->a = num_expr_of(l["a"]);
    e->b = num_expr_of(l["b"]);
  } else if (kind == "agg") {
    e->kind = multimod::NumExpr::Kind::Agg;
    e->op = str_of(l["op"])[0];
    e->steps = steps_of(l["steps"]);
    e->a = num_expr_of(l["body"]);
  } else {
    stop("unknown numeric expression kind: %s", kind.c_str());
  }
  return e;
}

std::unique_ptr<multimod::Cond> cond_of(sexp x) {
  if (x == R_NilValue) return nullptr;
  list l(x);
  auto c = std::make_unique<multimod::Cond>();
  std::string kind = str_of(l["kind"]);
  if (kind == "and") { c->kind = multimod::Cond::Kind::And; c->a = cond_of(l["a"]); c->b = cond_of(l["b"]); }
  else if (kind == "or") { c->kind = multimod::Cond::Kind::Or; c->a = cond_of(l["a"]); c->b = cond_of(l["b"]); }
  else if (kind == "not") { c->kind = multimod::Cond::Kind::Not; c->a = cond_of(l["a"]); }
  else if (kind == "cmp") {
    c->kind = multimod::Cond::Kind::Cmp;
    c->op = str_of(l["op"]);
    c->x = num_expr_of(l["a"]);
    c->y = num_expr_of(l["b"]);
  } else if (kind == "member") {
    c->kind = multimod::Cond::Kind::Member;
    c->table = str_of(l["table"]);
    c->on = pairs_of(l["on"]);
  } else {
    stop("unknown condition kind: %s", kind.c_str());
  }
  return c;
}

multimod::Plan plan_of(list plan) {
  multimod::Plan p;
  p.rows = str_of(plan["rows"]);
  p.row_keys = strs_of(plan["row_keys"]);
  list terms(plan["terms"]);
  for (R_xlen_t t = 0; t < terms.size(); ++t) {
    list term(terms[t]);
    multimod::Term tm;
    tm.sign = num_of(term["sign"], 1.0);
    tm.steps = steps_of(term["steps"]);
    p.terms.push_back(std::move(tm));
  }
  return p;
}

multimod::Table table_of(list cols, const std::string& tname) {
  strings cnames(cols.names());
  multimod::Table tab;
  for (R_xlen_t c = 0; c < cols.size(); ++c) {
    sexp col = cols[c];
    std::string nm(cnames[c]);
    if (TYPEOF(col) == INTSXP) { tab.icols[nm] = INTEGER(col); tab.n = Rf_xlength(col); }
    else if (TYPEOF(col) == REALSXP) { tab.dcols[nm] = REAL(col); tab.n = Rf_xlength(col); }
    else stop("table %s: column %s is neither integer nor double", tname.c_str(), nm.c_str());
  }
  return tab;
}

sexp int_vector(const std::vector<int>& v) {
  sexp out = safe[Rf_allocVector](INTSXP, static_cast<R_xlen_t>(v.size()));
  if (!v.empty()) std::memcpy(INTEGER(out), v.data(), v.size() * sizeof(int));
  return out;
}

sexp dbl_vector(const std::vector<double>& v) {
  sexp out = safe[Rf_allocVector](REALSXP, static_cast<R_xlen_t>(v.size()));
  if (!v.empty()) std::memcpy(REAL(out), v.data(), v.size() * sizeof(double));
  return out;
}

list result_of(const multimod::Triplets& out) {
  sexp i = int_vector(out.i), j = int_vector(out.j), ri = int_vector(out.rhs_i);
  sexp coef = dbl_vector(out.coef), rhs = dbl_vector(out.rhs);
  writable::doubles st(out.stats.size());
  writable::strings stn(out.stats.size());
  for (size_t k = 0; k < out.stats.size(); ++k) { st[k] = out.stats[k].second; stn[k] = out.stats[k].first; }
  st.names() = stn;
  return writable::list({"i"_nm = i, "j"_nm = j, "coef"_nm = coef, "rhs_i"_nm = ri, "rhs"_nm = rhs,
                         "stats"_nm = st});
}

}  // namespace

// An executor over the tables of one lowered equation. The R vectors behind
// the tables are read in place: the caller keeps them alive while the
// executor is used.
[[cpp11::register]]
sexp mm_block_open(list tables, int threads) {
  std::map<std::string, multimod::Table> tbl;
  if (tables.size() > 0) {
    strings tnames(tables.names());
    for (R_xlen_t t = 0; t < tables.size(); ++t) {
      std::string nm(tnames[t]);
      tbl[nm] = table_of(list(tables[t]), nm);
    }
  }
  external_pointer<multimod::Executor> ptr(new multimod::Executor(std::move(tbl), threads));
  return sexp(static_cast<SEXP>(ptr));
}

// Register more tables on an open executor; names already present are kept.
// Returns the names that were added.
[[cpp11::register]]
strings mm_block_add(sexp ex, list tables) {
  external_pointer<multimod::Executor> p(ex);
  writable::strings added;
  if (tables.size() == 0) return added;
  strings tnames(tables.names());
  for (R_xlen_t t = 0; t < tables.size(); ++t) {
    std::string nm(tnames[t]);
    if (p->add_table(nm, table_of(list(tables[t]), nm))) added.push_back(nm);
  }
  return added;
}

// Run a plan over one chunk of rows on an open executor.
[[cpp11::register]]
list mm_block_run(sexp ex, list plan, list rows) {
  external_pointer<multimod::Executor> p(ex);
  p->set_table("rows", table_of(rows, "rows"));
  multimod::Plan pl = plan_of(plan);
  multimod::Triplets out;
  try {
    out = p->run(pl);
  } catch (const std::exception& e) {
    stop("native block executor: %s", e.what());
  }
  return result_of(out);
}

// One-shot form: open, run, discard.
[[cpp11::register]]
list mm_exec_block(list plan, list tables) {
  sexp ex = mm_block_open(tables, 1);
  list rows(tables["rows"]);
  return mm_block_run(ex, plan, rows);
}
