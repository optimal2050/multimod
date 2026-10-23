// The scan pass of the build-time presolve: per-row and per-column entry
// counts, and the entries of every candidate row with at most `track`
// entries, so that a substitution can be composed and the small rows
// recomputed exactly after each pass. The rows of one chunk are complete and
// contiguous, so a chunk is examined on its own.
#pragma once
#include <cstdint>
#include <vector>

namespace multimod {

class Sink;

class Scan {
 public:
  // `candidate`, when given, marks the rows worth tracking (1-based index
  // minus one); the others only count. Typically the equality rows.
  Scan(int64_t nrow, int64_t ncol, int track, std::vector<uint8_t> candidate = {}, int threads = 1);
  // every entry added is also written to `sink` (the raw LP, to be remapped
  // once the substitution is known); the sink must outlive the scan's use
  void attach(Sink* sink) { sink_ = sink; }
  // 1-based ids; the entries of a chunk of complete rows
  void add(const int* i, const int* j, const double* x, int64_t n);
  int64_t nrow() const { return nrow_; }
  int64_t ncol() const { return ncol_; }
  int track() const { return track_; }
  const std::vector<int32_t>& row_counts() const { return rowc_; }
  const std::vector<int32_t>& col_counts() const { return colc_; }
  // The tracked rows' entries, 1-based, each row's entries contiguous and in
  // ascending column order, kept in blocks of fixed size so that no block
  // is ever regrown; a row never spans two blocks.
  size_t blocks() const { return srow_.size(); }
  const std::vector<int32_t>& block_row(size_t k) const { return srow_[k]; }
  const std::vector<int32_t>& block_col(size_t k) const { return scol_[k]; }
  const std::vector<double>& block_x(size_t k) const { return sx_[k]; }
  void drop_block(size_t k);
  int64_t n_tracked() const;
  // the tracked entries concatenated (diagnostics and tests)
  std::vector<int32_t> s_row() const;
  std::vector<int32_t> s_col() const;
  std::vector<double> s_x() const;

 private:
  int64_t nrow_, ncol_;
  int track_;
  int threads_;
  std::vector<uint8_t> cand_;
  Sink* sink_ = nullptr;
  std::vector<int32_t> rowc_, colc_;
  std::vector<std::vector<int32_t>> srow_, scol_;
  std::vector<std::vector<double>> sx_;
  static constexpr int64_t kBlock = 1 << 22;   // entries per block
};

}  // namespace multimod
