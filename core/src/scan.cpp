#include "multimod/scan.h"

#include <algorithm>
#include <numeric>
#include <stdexcept>

namespace multimod {

Scan::Scan(int64_t nrow, int64_t ncol, int track) : nrow_(nrow), ncol_(ncol), track_(track < 2 ? 2 : track) {
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
  // entries per row of this chunk, then the entries of the small rows,
  // gathered by sorting the chunk's entries by (row, column)
  int64_t span = static_cast<int64_t>(rmax) - rmin + 1;
  std::vector<int32_t> local(static_cast<size_t>(span), 0);
  for (int64_t k = 0; k < n; ++k) local[i[k] - rmin]++;
  std::vector<int64_t> keep;
  for (int64_t k = 0; k < n; ++k) if (local[i[k] - rmin] <= track_) keep.push_back(k);
  if (keep.empty()) return;
  std::sort(keep.begin(), keep.end(), [&](int64_t a, int64_t b) {
    return i[a] < i[b] || (i[a] == i[b] && j[a] < j[b]);
  });
  for (int64_t k : keep) { srow_.push_back(i[k]); scol_.push_back(j[k]); sx_.push_back(x[k]); }
}

}  // namespace multimod
