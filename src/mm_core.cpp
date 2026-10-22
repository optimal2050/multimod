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
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "multimod/block.h"
#include "multimod/mps.h"
#include "multimod/presolve.h"
#include "multimod/scan.h"

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

// --- streaming sink -----------------------------------------------------------

[[cpp11::register]]
sexp mm_sink_open(std::string dir, int buckets, double nrow, double ncol) {
  external_pointer<multimod::Sink> ptr(new multimod::Sink(dir, buckets, static_cast<int64_t>(nrow),
                                                          static_cast<int64_t>(ncol)));
  return sexp(static_cast<SEXP>(ptr));
}

[[cpp11::register]]
void mm_sink_write(sexp sink, integers i, integers j, doubles x) {
  external_pointer<multimod::Sink> p(sink);
  if (i.size() != j.size() || i.size() != x.size()) stop("i, j and x differ in length");
  try {
    p->write(INTEGER(i), INTEGER(j), REAL(x), i.size());
  } catch (const std::exception& e) {
    stop("sink: %s", e.what());
  }
}

// Run a plan over one chunk of rows and write the entries to the sink; only
// the constants (right-hand side) come back.
[[cpp11::register]]
list mm_block_run_sink(sexp ex, list plan, list rows, sexp sink) {
  external_pointer<multimod::Executor> p(ex);
  external_pointer<multimod::Sink> sk(sink);
  p->set_table("rows", table_of(rows, "rows"));
  multimod::Plan pl = plan_of(plan);
  multimod::Triplets out;
  try {
    out = p->run(pl);
    sk->write(out.i.data(), out.j.data(), out.coef.data(), static_cast<int64_t>(out.i.size()));
  } catch (const std::exception& e) {
    stop("native block executor: %s", e.what());
  }
  multimod::Triplets small;
  small.rhs_i = std::move(out.rhs_i);
  small.rhs = std::move(out.rhs);
  small.stats = std::move(out.stats);
  return result_of(small);
}

// The substitution the sink applies while writing. The R vectors are read in
// place and must outlive the sink.
[[cpp11::register]]
void mm_sink_map(sexp sink, integers ired, integers jred, integers ref, doubles coef, double tol) {
  external_pointer<multimod::Sink> p(sink);
  if (ref.size() != jred.size() || coef.size() != jred.size()) stop("ref and coef must match jred");
  p->set_map(INTEGER(ired), INTEGER(jred), INTEGER(ref), REAL(coef), ired.size(), jred.size(), tol);
}

// --- scan pass of the presolve --------------------------------------------

std::vector<uint8_t> mask_of(sexp x) {
  std::vector<uint8_t> out;
  if (x == R_NilValue) return out;
  if (TYPEOF(x) != LGLSXP) stop("a logical vector was expected");
  R_xlen_t n = Rf_xlength(x);
  const int* l = LOGICAL(x);
  out.resize(static_cast<size_t>(n));
  for (R_xlen_t k = 0; k < n; ++k) out[k] = (l[k] == TRUE) ? 1 : 0;
  return out;
}

// `candidate`: a logical over the rows, the rows whose entries are tracked
// (NULL: all); the others only count
[[cpp11::register]]
sexp mm_scan_open(double nrow, double ncol, int track, sexp candidate, int threads) {
  try {
    external_pointer<multimod::Scan> ptr(new multimod::Scan(static_cast<int64_t>(nrow), static_cast<int64_t>(ncol), track,
                                                            mask_of(candidate), threads));
    return sexp(static_cast<SEXP>(ptr));
  } catch (const std::exception& e) {
    stop("scan: %s", e.what());
  }
}

[[cpp11::register]]
list mm_scan_counts(sexp scan) {
  external_pointer<multimod::Scan> p(scan);
  return writable::list({
    "row_counts"_nm = int_vector(std::vector<int>(p->row_counts().begin(), p->row_counts().end())),
    "col_counts"_nm = int_vector(std::vector<int>(p->col_counts().begin(), p->col_counts().end()))});
}

// The doubleton substitution composed from the scan; the tracked entries are
// consumed. `candidate`: the rows that may be dropped; `is_int`: the columns
// that may not be eliminated (each a logical, or NULL).
[[cpp11::register]]
list mm_presolve_map(sexp scan, sexp candidate, sexp is_int, doubles col_lo, doubles col_up, int max_pass,
                     double tol, int threads, bool verbose) {
  external_pointer<multimod::Scan> p(scan);
  if (col_lo.size() != p->ncol() || col_up.size() != p->ncol()) stop("column bounds do not match the column count");
  multimod::PresolveMap m;
  std::function<void(const std::string&)> log;
  if (verbose) log = [](const std::string& s) { message(s.c_str()); };
  try {
    m = multimod::presolve_map(*p, mask_of(candidate), mask_of(is_int), REAL(col_lo), REAL(col_up), max_pass, tol, threads, log);
  } catch (const std::exception& e) {
    stop("presolve: %s", e.what());
  }
  std::vector<double> nnz(m.nnz.begin(), m.nnz.end());
  return writable::list({
    "col"_nm = int_vector(m.el_col), "ref"_nm = int_vector(m.el_ref), "coef"_nm = dbl_vector(m.el_coef),
    "pass"_nm = int_vector(m.el_pass),
    "jred"_nm = int_vector(m.jred), "ired"_nm = int_vector(m.ired),
    "final_ref"_nm = int_vector(m.ref), "final_coef"_nm = dbl_vector(m.coef),
    "col_lo"_nm = dbl_vector(m.col_lo), "col_up"_nm = dbl_vector(m.col_up),
    "nnz"_nm = dbl_vector(nnz),
    "crossing"_nm = as_sexp(static_cast<double>(m.crossing)),
    "passes"_nm = as_sexp(m.passes)});
}

[[cpp11::register]]
void mm_scan_add(sexp scan, integers i, integers j, doubles x) {
  external_pointer<multimod::Scan> p(scan);
  if (i.size() != j.size() || i.size() != x.size()) stop("i, j and x differ in length");
  try {
    p->add(INTEGER(i), INTEGER(j), REAL(x), i.size());
  } catch (const std::exception& e) {
    stop("scan: %s", e.what());
  }
}

[[cpp11::register]]
list mm_block_run_scan(sexp ex, list plan, list rows, sexp scan) {
  external_pointer<multimod::Executor> p(ex);
  external_pointer<multimod::Scan> sc(scan);
  p->set_table("rows", table_of(rows, "rows"));
  multimod::Plan pl = plan_of(plan);
  multimod::Triplets out;
  try {
    out = p->run(pl);
    sc->add(out.i.data(), out.j.data(), out.coef.data(), static_cast<int64_t>(out.i.size()));
  } catch (const std::exception& e) {
    stop("native block executor: %s", e.what());
  }
  multimod::Triplets small;
  small.rhs_i = std::move(out.rhs_i);
  small.rhs = std::move(out.rhs);
  small.stats = std::move(out.stats);
  return result_of(small);
}

[[cpp11::register]]
list mm_scan_close(sexp scan) {
  external_pointer<multimod::Scan> p(scan);
  return writable::list({
    "row_counts"_nm = int_vector(std::vector<int>(p->row_counts().begin(), p->row_counts().end())),
    "col_counts"_nm = int_vector(std::vector<int>(p->col_counts().begin(), p->col_counts().end())),
    "row"_nm = int_vector(std::vector<int>(p->s_row().begin(), p->s_row().end())),
    "col"_nm = int_vector(std::vector<int>(p->s_col().begin(), p->s_col().end())),
    "x"_nm = dbl_vector(p->s_x())});
}

[[cpp11::register]]
list mm_sink_close(sexp sink) {
  external_pointer<multimod::Sink> p(sink);
  try {
    p->close();
  } catch (const std::exception& e) {
    stop("sink: %s", e.what());
  }
  return writable::list({"nnz"_nm = as_sexp(static_cast<double>(p->nnz())),
                         "row_counts"_nm = int_vector(std::vector<int>(p->row_counts().begin(), p->row_counts().end()))});
}

[[cpp11::register]]
double mm_mps_write(std::string file, sexp sink, doubles row_lo, doubles row_up,
                    doubles col_lo, doubles col_up, doubles obj, bool maximum) {
  external_pointer<multimod::Sink> p(sink);
  if (row_lo.size() != p->nrow() || row_up.size() != p->nrow()) stop("row bounds do not match the sink's rows");
  if (col_lo.size() != p->ncol() || col_up.size() != p->ncol() || obj.size() != p->ncol()) stop("column vectors do not match the sink's columns");
  multimod::LpBounds lp{REAL(row_lo), REAL(row_up), REAL(col_lo), REAL(col_up), REAL(obj), maximum};
  try {
    return static_cast<double>(multimod::write_mps(file, *p, lp));
  } catch (const std::exception& e) {
    stop("mps: %s", e.what());
  }
}

// One-shot form: open, run, discard.
[[cpp11::register]]
list mm_exec_block(list plan, list tables) {
  sexp ex = mm_block_open(tables, 1);
  list rows(tables["rows"]);
  return mm_block_run(ex, plan, rows);
}
