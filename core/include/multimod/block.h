// A block plan and its execution: one equation's coefficient triplets from
// integer-coded tables.
//
// The plan is a list of terms. Each term is a sequence of steps over the
// equation's row table: gates (semi-joins), expansions (binding merges that
// introduce a summation index), filters (a condition evaluated per row),
// factors (a numeric expression of parameters and constants evaluated per
// row) and at most one variable lookup. A term with a variable yields matrix
// entries (row, column, coefficient); a term without one is a constant that
// goes to the right-hand side. Entries are summed per (row, column) at the
// end.
#pragma once
#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "keyhash.h"

namespace multimod {

// Columns of a table the plan refers to; the vectors are owned elsewhere.
struct Table {
  int64_t n = 0;
  std::map<std::string, const int*> icols;
  std::map<std::string, const double*> dcols;
};

using OnPairs = std::vector<std::pair<std::string, std::string>>;  // env col -> table col

struct Cond;
struct Step;

// A numeric expression over the environment: constants, parameter lookups
// (with a default), arithmetic, a conditional value, and an aggregate (sum
// or product) of a numeric body over an expansion of the environment.
struct NumExpr {
  enum class Kind { Const, Param, Binary, When, Agg };
  Kind kind = Kind::Const;
  double value = 0.0;          // Const; Param: the default
  std::string table;           // Param
  OnPairs on;                  // Param
  std::string column;          // Param: the value column
  char op = '+';               // Binary: + - * / ^ ; Agg: + or *
  std::unique_ptr<NumExpr> a, b;
  std::unique_ptr<Cond> cond;  // When: cond ? a : b (b absent -> 0)
  std::vector<Step> steps;     // Agg: gates, expansions and filters of the body's environment
};

// A condition over the environment.
struct Cond {
  enum class Kind { And, Or, Not, Cmp, Member };
  Kind kind = Kind::Member;
  std::unique_ptr<Cond> a, b;
  std::string op;              // Cmp: < <= > >= == !=
  std::unique_ptr<NumExpr> x, y;
  std::string table;           // Member
  OnPairs on;                  // Member
};

enum class StepOp { Gate, Expand, Filter, Factor, Var };

struct Step {
  StepOp op = StepOp::Factor;
  std::string table;           // gate, expand, var
  OnPairs on;                  // gate, expand (shared keys), var
  OnPairs bind;                // expand: new env column -> table column
  std::string column;          // var: the column-id column
  std::unique_ptr<NumExpr> expr;   // factor
  std::unique_ptr<Cond> cond;      // filter
  bool invert = false;         // factor: multiply by 1 / value
};

struct Term {
  double sign = 1.0;
  std::vector<Step> steps;
};

struct Plan {
  std::string rows;                  // table of the equation rows: k + key columns
  std::vector<std::string> row_keys; // env column names of the row keys, in table column order
  std::vector<Term> terms;
};

struct Triplets {
  std::vector<int> i, j;
  std::vector<double> coef;
  std::vector<int> rhs_i;
  std::vector<double> rhs;
  std::vector<std::pair<std::string, double>> stats;   // seconds per kind of work
};

struct Env {
  int64_t n = 0;
  std::vector<int> k;
  std::vector<double> coef;
  std::vector<std::string> names;
  std::vector<std::vector<int>> cols;
  std::vector<int> j;
  bool has_j = false;
  std::vector<int64_t> origin;   // for an aggregate: the row of the parent environment
  int col(const std::string& name) const;
};

// Executes plans over a fixed set of tables. The hash indexes built on the
// tables are kept between runs, so the chunks of one equation share them;
// the row table is replaced per chunk with set_table().
class Executor {
 public:
  explicit Executor(std::map<std::string, Table> tables, int threads = 1)
      : tables_(std::move(tables)), threads_(threads < 1 ? 1 : threads) {}
  void set_table(const std::string& name, Table t);
  // register a table once; a name already present is kept as it is
  bool add_table(const std::string& name, Table t);
  bool has_table(const std::string& name) const { return tables_.count(name) != 0; }
  Triplets run(const Plan& plan);

 private:
  Triplets run_terms(const Plan& plan);
  void keep_mask(Env& env, const std::vector<char>& keep);
  void group_sum(const std::vector<int>& a, const std::vector<int>& b, const std::vector<double>& w,
                 std::vector<int>& oa, std::vector<int>& ob, std::vector<double>& os, bool drop_zero);
  const Table& table(const std::string& name) const;
  const KeyIndex& index(const std::string& table, const std::vector<std::string>& cols);
  KeyCols probe_cols(const Env& env, const OnPairs& on) const;

  void gate(Env& env, const Step& s);
  void expand(Env& env, const Step& s);
  void filter(Env& env, const Step& s);
  void factor(Env& env, const Step& s);
  void var(Env& env, const Step& s);
  void step(Env& env, const Step& s);
  std::vector<double> eval_num(const NumExpr& e, const Env& env);
  std::vector<char> eval_cond(const Cond& c, const Env& env);

  std::map<std::string, Table> tables_;
  int threads_ = 1;
  std::map<std::string, std::unique_ptr<KeyIndex>> index_cache_;
  std::map<std::string, double> stats_;
};

}  // namespace multimod
