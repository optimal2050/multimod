// Streaming output of an LP: matrix entries are spooled to disk as they are
// produced, bucketed by column range, and the MPS file is written at the
// end from one bucket at a time. Nothing of the matrix's size is held in
// memory.
#pragma once
#include <cstdint>
#include <cstdio>
#include <string>
#include <vector>

namespace multimod {

struct Entry {
  int32_t row;   // 0-based
  int32_t col;   // 0-based
  double x;
};

class Sink {
 public:
  // `dir` receives one binary file per bucket; it is created if absent.
  Sink(std::string dir, int buckets, int64_t nrow, int64_t ncol);
  ~Sink();
  // 1-based row and column ids, as the executor and R produce them
  void write(const int* i, const int* j, const double* x, int64_t n);
  // A substitution to apply while writing: `ired[r]` is the reduced position
  // of row r (0: the row is dropped), `jred[c]` the reduced position of a
  // kept column (0: eliminated), and an eliminated column c is replaced by
  // `coef[c]` times kept column `ref[c]` (1-based, full index). Entries of
  // one row that land on the same column are summed; a sum within `tol` of
  // zero is dropped. The sink's dimensions are the reduced ones.
  void set_map(const int* ired, const int* jred, const int* ref, const double* coef,
               int64_t nrow_full, int64_t ncol_full, double tol);
  void close();
  bool closed() const { return closed_; }
  int buckets() const { return buckets_; }
  int64_t nrow() const { return nrow_; }
  int64_t ncol() const { return ncol_; }
  int64_t nnz() const { return nnz_; }
  int64_t cols_per_bucket() const { return per_; }
  const std::vector<int32_t>& row_counts() const { return counts_; }
  std::string bucket_path(int b) const;
  // read one bucket back, sorted by (col, row)
  std::vector<Entry> read_bucket(int b) const;

 private:
  void flush(int b);
  std::string dir_;
  int buckets_;
  int64_t nrow_, ncol_, per_;
  std::vector<FILE*> files_;
  std::vector<std::vector<Entry>> buf_;
  size_t cap_;
  std::vector<int32_t> counts_;
  int64_t nnz_ = 0;
  bool closed_ = false;
  // substitution map, in full-index terms
  bool mapped_ = false;
  const int* ired_ = nullptr; const int* jred_ = nullptr;
  const int* ref_ = nullptr; const double* coef_ = nullptr;
  int64_t nrow_full_ = 0, ncol_full_ = 0;
  double tol_ = 0.0;
  void push(int32_t r, int32_t c, double x);
};

struct LpBounds {
  const double* row_lo; const double* row_up;
  const double* col_lo; const double* col_up;
  const double* obj;
  bool maximum;
};

// Write the LP whose entries the sink holds as a free-format MPS file with
// positional names c<j> and r<i>, every column present. Returns the number
// of matrix entries written.
int64_t write_mps(const std::string& file, const Sink& sink, const LpBounds& lp,
                  const std::string& name = "model");

}  // namespace multimod
