#include "multimod/scan.h"

#include <algorithm>
#include <stdexcept>
#include <utility>

#include "multimod/parallel.h"

namespace multimod {

Scan::Scan(int64_t nrow, int64_t ncol, int track, std::vector<uint8_t> candidate, int threads)
    : nrow_(nrow), ncol_(ncol), track_(track < 2 ? 2 : track), threads_(threads < 1 ? 1 : threads),
      cand_(std::move(candidate)) {
  if (!cand_.empty() && static_cast<int64_t>(cand_.size()) != nrow) throw std::runtime_error("candidate mask does not match the row count");
  rowc_.assign(static_cast<size_t>(nrow), 0);
  colc_.assign(static_cast<size_t>(ncol), 0);
}

void Scan::add(const int* i, const int* j, const double* x, int64_t n) {
  if (n == 0) return;
  int32_t rmin = i[0], rmax = i[0];
  for (int64_t k = 0; k < n; ++k) {
    if (i[k] < 1 || i[k] > nrow_ || j[k] < 1 || j[k] > ncol_) throw std::runtime_error("entry outside the LP dimensions");
    rmin = std::min(rmin, i[k]); rmax = std::max(rmax, i[k]);
    rowc_[i[k] - 1]++;
    colc_[j[k] - 1]++;
  }
  // entries per row of this chunk; the small candidate rows are kept, placed
  // by a counting sort on the row and ordered by column within the row
  int64_t span = static_cast<int64_t>(rmax) - rmin + 1;
  std::vector<int32_t> local(static_cast<size_t>(span), 0);
  for (int64_t k = 0; k < n; ++k) local[i[k] - rmin]++;
  std::vector<int64_t> off(static_cast<size_t>(span) + 1, 0);
  for (int64_t r = 0; r < span; ++r) {
    bool keep = local[r] <= track_ && (cand_.empty() || cand_[rmin - 1 + r]);
    off[r + 1] = off[r] + (keep ? local[r] : 0);
    if (!keep) local[r] = 0;  // marks the row as not kept
  }
  int64_t kept = off[span];
  if (kept == 0) return;
  size_t base = srow_.size();
  srow_.resize(base + kept);
  scol_.resize(base + kept);
  sx_.resize(base + kept);
  std::vector<int64_t> cursor(off.begin(), off.end() - 1);
  for (int64_t k = 0; k < n; ++k) {
    int64_t r = i[k] - rmin;
    if (local[r] == 0) continue;
    size_t p = base + static_cast<size_t>(cursor[r]++);
    srow_[p] = i[k]; scol_[p] = j[k]; sx_[p] = x[k];
  }
  parallel_for(span, threads_, [&](int64_t b, int64_t e, int) {
    for (int64_t r = b; r < e; ++r) {
      int64_t a = off[r], z = off[r + 1];
      if (z - a < 2) continue;
      // insertion sort of at most `track` entries by column
      for (int64_t p = a + 1; p < z; ++p) {
        int32_t c = scol_[base + p]; double v = sx_[base + p];
        int64_t q = p;
        while (q > a && scol_[base + q - 1] > c) {
          scol_[base + q] = scol_[base + q - 1]; sx_[base + q] = sx_[base + q - 1];
          --q;
        }
        scol_[base + q] = c; sx_[base + q] = v;
      }
    }
  });
}

void Scan::release(std::vector<int32_t>& row, std::vector<int32_t>& col, std::vector<double>& x) {
  row.swap(srow_); col.swap(scol_); x.swap(sx_);
  std::vector<int32_t>().swap(srow_);
  std::vector<int32_t>().swap(scol_);
  std::vector<double>().swap(sx_);
}

}  // namespace multimod
