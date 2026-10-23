// Streaming output of an LP: matrix entries are spooled to disk as they are
// produced, bucketed by column range, and the MPS file is written at the
// end from one bucket at a time. Nothing of the matrix's size is held in
// memory.
#pragma once
#include <cstdint>
#include <cstdio>
#include <string>
#include <vector>

#include "multimod/presolve.h"

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
  // Apply a substitution to a closed, unmapped sink: the spooled entries are
  // read bucket by bucket, dropped rows skipped, eliminated columns moved to
  // `coef[c]` times `ref[c]` (see set_map), and re-spooled by reduced
  // column. The sink then holds the reduced LP, its dimensions the reduced
  // ones, and read_bucket() sums what lands on one (row, column), dropping
  // a sum within `tol` of zero.
  // `col_nnz`, when given, holds the entry count of each reduced column
  // (ncol_red values): the new buckets are then cut at equal entry counts
  // rather than equal column counts, so that no bucket is much larger than
  // the others however the entries are distributed over the columns.
  void remap(const int* ired, const int* jred, const int* ref, const double* coef,
             int64_t nrow_red, int64_t ncol_red, double tol, const int64_t* col_nnz = nullptr);
  // the same, from a composed map: the reduced dimensions and the entry
  // counts of the reduced columns come from it
  void remap(const PresolveMap& map, double tol);
  bool closed() const { return closed_; }
  int buckets() const { return buckets_; }
  int64_t nrow() const { return nrow_; }
  int64_t ncol() const { return ncol_; }
  int64_t nnz() const { return nnz_; }
  // the column range [begin, end) of a bucket
  int64_t bucket_begin(int b) const { return bounds_[b]; }
  int64_t bucket_end(int b) const { return bounds_[b + 1]; }
  const std::vector<int32_t>& row_counts() const { return counts_; }
  std::string bucket_path(int b) const;
  // read one bucket back, sorted by (col, row), duplicates merged after remap()
  std::vector<Entry> read_bucket(int b) const;
  std::vector<Entry> read_bucket_raw(int b) const;

 private:
  void flush(int b);
  std::string dir_;
  int buckets_;
  int64_t nrow_, ncol_;
  std::vector<int64_t> bounds_;   // bucket b holds columns [bounds_[b], bounds_[b + 1])
  int bucket_of(int64_t c) const;
  void uniform_bounds();
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
  bool merge_ = false;
  void push(int32_t r, int32_t c, double x);
  void open_files();
};

// The LP around the matrix, in full-index terms: one entry per row and
// column of the model as built. With a map, the writer reduces them itself:
// dropped rows are skipped, eliminated columns' objective entries move to
// their references, and the column bounds are the map's tightened ones.
struct LpInput {
  const int8_t* sense;            // per row: 0 '==', 1 '<=', 2 '>='
  const double* rhs;              // per row
  const double* col_lo; const double* col_up;   // per column (ignored with a map)
  const int* obj_j; const double* obj_x; int64_t obj_n;   // the objective, sparse, 1-based
  bool maximum;
  double tol;                     // |value| below it is written as 0
  int64_t nrow_full, ncol_full;
  const PresolveMap* map;         // nullptr: no reduction
};

// Write the LP whose entries the sink holds as a free-format MPS file with
// positional names c<j> and r<i>, every column present. Returns the number
// of matrix entries written.
int64_t write_mps(const std::string& file, const Sink& sink, const LpInput& lp,
                  const std::string& name = "model");

}  // namespace multimod
