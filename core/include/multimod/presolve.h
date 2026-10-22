// The doubleton substitution of the build-time presolve, composed from the
// scan pass: rows `a*x + b*y = 0` eliminate x = -(b/a) y; the small rows are
// recomputed after each pass and the next pass runs on them, until no row
// qualifies. The rules are those of lp_presolve() in R/lp_reduce.R, so the
// reduced LP and the eliminated table are the same.
#pragma once
#include <cstdint>
#include <functional>
#include <string>
#include <vector>

#include "multimod/scan.h"

namespace multimod {

struct PresolveMap {
  // the eliminated columns, in elimination order: x = coef * ref, in `pass`
  std::vector<int32_t> el_col, el_ref, el_pass;
  std::vector<double> el_coef;
  // reduced position of each column / row, 0 when eliminated
  std::vector<int32_t> jred, ired;
  // every eliminated column resolved to a kept one; ref 0 / coef 1 when kept
  std::vector<int32_t> ref;
  std::vector<double> coef;
  // column bounds after tightening
  std::vector<double> col_lo, col_up;
  // column entry counts of the reduced LP
  std::vector<int64_t> nnz;
  // columns whose bounds cross after the substitution (infeasible)
  int64_t crossing = 0;
  int passes = 0;
};

// `candidate` (nrow) marks the rows that may be dropped: equalities with a
// zero right-hand side. `is_int` (ncol) marks the columns that may not be
// eliminated. The scan's tracked entries are taken over and consumed.
PresolveMap presolve_map(Scan& scan, const std::vector<uint8_t>& candidate, const std::vector<uint8_t>& is_int,
                         const double* col_lo, const double* col_up, int max_pass, double tol, int threads,
                         const std::function<void(const std::string&)>& log);

}  // namespace multimod
