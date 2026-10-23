#include "multimod/mps.h"

#include <algorithm>
#include <charconv>
#include <filesystem>
#include <cmath>
#include <cstring>
#include <limits>
#include <stdexcept>
#include <sys/stat.h>

#ifdef _WIN32
#include <direct.h>
#define MM_MKDIR(p) _mkdir(p)
#else
#define MM_MKDIR(p) mkdir(p, 0755)
#endif

namespace multimod {

namespace {

void ensure_dir(const std::string& dir) {
  struct stat st;
  if (stat(dir.c_str(), &st) == 0) return;
  if (MM_MKDIR(dir.c_str()) != 0) throw std::runtime_error("cannot create directory " + dir);
}

// shortest round-trip decimal of a double
void fmt_num(double v, std::string& out) {
  char buf[64];
  auto r = std::to_chars(buf, buf + sizeof(buf), v);
  out.append(buf, r.ptr);
}

bool is_inf(double v) { return std::isinf(v); }

}  // namespace

Sink::Sink(std::string dir, int buckets, int64_t nrow, int64_t ncol)
    : dir_(std::move(dir)), buckets_(std::max(1, buckets)), nrow_(nrow), ncol_(ncol) {
  ensure_dir(dir_);
  uniform_bounds();
  cap_ = 262144;   // entries per bucket buffer: 4 MB
  open_files();
  counts_.assign(static_cast<size_t>(nrow_), 0);
}

void Sink::uniform_bounds() {
  int64_t per = (ncol_ + buckets_ - 1) / buckets_;
  if (per < 1) per = 1;
  bounds_.assign(buckets_ + 1, ncol_);
  for (int b = 0; b <= buckets_; ++b) bounds_[b] = std::min<int64_t>(ncol_, static_cast<int64_t>(b) * per);
}

int Sink::bucket_of(int64_t c) const {
  // the last boundary not above c
  int b = static_cast<int>(std::upper_bound(bounds_.begin(), bounds_.end(), c) - bounds_.begin()) - 1;
  if (b < 0) b = 0;
  if (b >= buckets_) b = buckets_ - 1;
  return b;
}

void Sink::open_files() {
  files_.assign(buckets_, nullptr);
  buf_.assign(buckets_, {});
  for (int b = 0; b < buckets_; ++b) {
    files_[b] = std::fopen(bucket_path(b).c_str(), "wb");
    if (!files_[b]) throw std::runtime_error("cannot open bucket file " + bucket_path(b));
    buf_[b].reserve(cap_);
  }
}

void Sink::remap(const int* ired, const int* jred, const int* ref, const double* coef,
                 int64_t nrow_red, int64_t ncol_red, double tol, const int64_t* col_nnz) {
  if (!closed_) throw std::runtime_error("close the sink before remapping it");
  if (mapped_ || merge_) throw std::runtime_error("the sink is already mapped");
  int64_t nrow_full = nrow_, ncol_full = ncol_;
  // the raw buckets move aside; the reduced LP is spooled into fresh ones
  std::vector<std::string> raw(buckets_);
  for (int b = 0; b < buckets_; ++b) {
    raw[b] = bucket_path(b) + ".raw";
    std::remove(raw[b].c_str());
    if (std::rename(bucket_path(b).c_str(), raw[b].c_str()) != 0) throw std::runtime_error("cannot rename " + bucket_path(b));
  }
  int old_buckets = buckets_;
  nrow_ = nrow_red; ncol_ = ncol_red;
  if (col_nnz) {
    // cut at equal entry counts: a boundary moves on once the bucket holds
    // its share, so a bucket holds at least one column
    int64_t total = 0;
    for (int64_t c = 0; c < ncol_; ++c) total += col_nnz[c];
    int64_t share = total / buckets_ + 1;
    bounds_.assign(buckets_ + 1, ncol_);
    bounds_[0] = 0;
    int b = 1;
    int64_t acc = 0;
    for (int64_t c = 0; c < ncol_ && b < buckets_; ++c) {
      acc += col_nnz[c];
      if (acc >= share) { bounds_[b++] = c + 1; acc = 0; }
    }
  } else {
    uniform_bounds();
  }
  counts_.assign(static_cast<size_t>(nrow_), 0);
  nnz_ = 0;
  closed_ = false;
  open_files();
  std::vector<Entry> v;
  for (int b = 0; b < old_buckets; ++b) {
    FILE* f = std::fopen(raw[b].c_str(), "rb");
    if (!f) throw std::runtime_error("cannot read " + raw[b]);
    // the size from the file system: ftell() is 32-bit on some platforms
    v.resize(static_cast<size_t>(std::filesystem::file_size(raw[b]) / sizeof(Entry)));
    if (!v.empty() && std::fread(v.data(), sizeof(Entry), v.size(), f) != v.size()) {
      std::fclose(f);
      throw std::runtime_error("short read on " + raw[b]);
    }
    std::fclose(f);
    std::remove(raw[b].c_str());
    for (const Entry& e : v) {
      if (e.row < 0 || e.row >= nrow_full || e.col < 0 || e.col >= ncol_full) throw std::runtime_error("entry outside the LP dimensions");
      if (ired[e.row] == 0) continue;                 // an eliminated row
      int32_t c = e.col;
      double x = e.x;
      if (jred[c] == 0) {                             // an eliminated column
        x *= coef[c];
        c = ref[c] - 1;
        if (c < 0 || jred[c] == 0) throw std::runtime_error("substitution refers to an eliminated column");
      }
      push(ired[e.row] - 1, jred[c] - 1, x);
    }
  }
  close();
  merge_ = true;
  tol_ = tol;
}

Sink::~Sink() {
  for (FILE* f : files_) if (f) std::fclose(f);
}

std::string Sink::bucket_path(int b) const {
  return dir_ + "/b" + std::to_string(b) + ".bin";
}

void Sink::flush(int b) {
  if (buf_[b].empty()) return;
  size_t n = buf_[b].size();
  if (std::fwrite(buf_[b].data(), sizeof(Entry), n, files_[b]) != n) {
    throw std::runtime_error("write failed on " + bucket_path(b));
  }
  buf_[b].clear();
}

void Sink::push(int32_t r, int32_t c, double x) {
  if (r < 0 || r >= nrow_ || c < 0 || c >= ncol_) throw std::runtime_error("entry outside the LP dimensions");
  int b = bucket_of(c);
  buf_[b].push_back(Entry{r, c, x});
  counts_[r]++;
  nnz_++;
  if (buf_[b].size() >= cap_) flush(b);
}

void Sink::set_map(const int* ired, const int* jred, const int* ref, const double* coef,
                   int64_t nrow_full, int64_t ncol_full, double tol) {
  mapped_ = true;
  ired_ = ired; jred_ = jred; ref_ = ref; coef_ = coef;
  nrow_full_ = nrow_full; ncol_full_ = ncol_full; tol_ = tol;
}

void Sink::write(const int* i, const int* j, const double* x, int64_t n) {
  if (closed_) throw std::runtime_error("the sink is closed");
  if (!mapped_) {
    for (int64_t k = 0; k < n; ++k) push(i[k] - 1, j[k] - 1, x[k]);
    return;
  }
  // substitute, then sum what lands on the same (row, column) of one row
  std::vector<Entry> e;
  e.reserve(static_cast<size_t>(n));
  for (int64_t k = 0; k < n; ++k) {
    int32_t r = i[k], c = j[k];
    if (r < 1 || r > nrow_full_ || c < 1 || c > ncol_full_) throw std::runtime_error("entry outside the LP dimensions");
    if (ired_[r - 1] == 0) continue;                 // an eliminated row
    double v = x[k];
    if (jred_[c - 1] == 0) {                         // an eliminated column
      v *= coef_[c - 1];
      c = ref_[c - 1];
      if (c < 1 || jred_[c - 1] == 0) throw std::runtime_error("substitution refers to an eliminated column");
    }
    e.push_back(Entry{r, c, v});
  }
  std::sort(e.begin(), e.end(), [](const Entry& a, const Entry& b) {
    return a.row < b.row || (a.row == b.row && a.col < b.col);
  });
  size_t k = 0;
  while (k < e.size()) {
    size_t m = k + 1;
    double v = e[k].x;
    while (m < e.size() && e[m].row == e[k].row && e[m].col == e[k].col) { v += e[m].x; ++m; }
    if (std::fabs(v) > tol_) push(ired_[e[k].row - 1] - 1, jred_[e[k].col - 1] - 1, v);
    k = m;
  }
}

void Sink::close() {
  if (closed_) return;
  for (int b = 0; b < buckets_; ++b) {
    flush(b);
    std::fclose(files_[b]);
    files_[b] = nullptr;
  }
  closed_ = true;
}

std::vector<Entry> Sink::read_bucket_raw(int b) const {
  std::vector<Entry> v;
  FILE* f = std::fopen(bucket_path(b).c_str(), "rb");
  if (!f) throw std::runtime_error("cannot read bucket file " + bucket_path(b));
  v.resize(static_cast<size_t>(std::filesystem::file_size(bucket_path(b)) / sizeof(Entry)));
  if (!v.empty() && std::fread(v.data(), sizeof(Entry), v.size(), f) != v.size()) {
    std::fclose(f);
    throw std::runtime_error("short read on " + bucket_path(b));
  }
  std::fclose(f);
  return v;
}

std::vector<Entry> Sink::read_bucket(int b) const {
  std::vector<Entry> v = read_bucket_raw(b);
  std::sort(v.begin(), v.end(), [](const Entry& a, const Entry& c) {
    return a.col < c.col || (a.col == c.col && a.row < c.row);
  });
  if (!merge_ || v.empty()) return v;
  // entries of one (row, column) summed; a sum within the tolerance dropped
  size_t w = 0;
  for (size_t k = 0; k < v.size();) {
    size_t m = k + 1;
    double x = v[k].x;
    while (m < v.size() && v[m].col == v[k].col && v[m].row == v[k].row) { x += v[m].x; ++m; }
    if (std::fabs(x) > tol_) { v[w] = v[k]; v[w].x = x; ++w; }
    k = m;
  }
  v.resize(w);
  return v;
}

void Sink::remap(const PresolveMap& map, double tol) {
  int64_t nr = 0, nc = 0;
  for (int32_t v : map.ired) if (v > 0) ++nr;
  std::vector<int64_t> nnz;
  nnz.reserve(map.jred.size());
  for (size_t c = 0; c < map.jred.size(); ++c) if (map.jred[c] > 0) { ++nc; nnz.push_back(map.nnz[c]); }
  remap(map.ired.data(), map.jred.data(), map.ref.data(), map.coef.data(), nr, nc, tol, nnz.data());
}

int64_t write_mps(const std::string& file, const Sink& sink, const LpInput& lp, const std::string& name) {
  if (!sink.closed()) throw std::runtime_error("close the sink before writing the MPS");
  const PresolveMap* map = lp.map;
  int64_t nrow = sink.nrow(), ncol = sink.ncol();
  if (map) {
    if (static_cast<int64_t>(map->ired.size()) != lp.nrow_full || static_cast<int64_t>(map->jred.size()) != lp.ncol_full)
      throw std::runtime_error("the map does not match the LP dimensions");
  } else if (nrow != lp.nrow_full || ncol != lp.ncol_full) {
    throw std::runtime_error("the LP dimensions do not match the sink's");
  }
  auto small = [&](double v) { return std::fabs(v) < lp.tol ? 0.0 : v; };
  // the full column behind each reduced column, and the reduced objective
  // (an eliminated column's entry moves to its reference, scaled)
  std::vector<int32_t> full_of(static_cast<size_t>(ncol));
  if (map) {
    for (int64_t c = 0; c < lp.ncol_full; ++c) if (map->jred[c] > 0) full_of[map->jred[c] - 1] = static_cast<int32_t>(c);
  } else {
    for (int64_t c = 0; c < ncol; ++c) full_of[c] = static_cast<int32_t>(c);
  }
  std::vector<double> obj(static_cast<size_t>(ncol), 0.0);
  for (int64_t k = 0; k < lp.obj_n; ++k) {
    int64_t c = lp.obj_j[k] - 1;
    double v = lp.obj_x[k];
    if (c < 0 || c >= lp.ncol_full) throw std::runtime_error("objective column outside the LP");
    if (map && map->jred[c] == 0) { v *= map->coef[c]; c = map->ref[c] - 1; }
    int64_t cr = map ? map->jred[c] - 1 : c;
    if (cr < 0) throw std::runtime_error("objective refers to an eliminated column");
    obj[cr] += v;
  }

  FILE* out = std::fopen(file.c_str(), "wb");
  if (!out) throw std::runtime_error("cannot open " + file + " for writing");
  std::string s;
  s.reserve(1 << 24);
  auto emit = [&]() {
    if (s.size() < (1u << 23)) return;
    std::fwrite(s.data(), 1, s.size(), out);
    s.clear();
  };

  s += "NAME        " + name + "\n";
  if (lp.maximum) s += "OBJSENSE\n    MAX\n";

  // rows: the type from the sense; a ranged row carries its width in RANGES
  s += "ROWS\n N  Obj\n";
  std::vector<char> rtype(static_cast<size_t>(nrow));
  {
    int64_t r = 0;
    for (int64_t rf = 0; rf < lp.nrow_full; ++rf) {
      if (map && map->ired[rf] == 0) continue;
      char t;
      switch (lp.sense[rf]) {
        case 0: t = 'E'; break;
        case 1: t = 'L'; break;
        case 2: t = 'G'; break;
        default: throw std::runtime_error("row " + std::to_string(rf + 1) + " has an unknown sense");
      }
      rtype[r] = t;
      s += ' '; s += t; s += "  r"; s += std::to_string(r); s += '\n';
      ++r;
      emit();
    }
  }

  // columns: objective entry, then the matrix entries in row order; a column
  // without any entry is kept with an explicit zero objective
  s += "COLUMNS\n";
  int64_t written = 0;
  for (int b = 0; b < sink.buckets(); ++b) {
    std::vector<Entry> e = sink.read_bucket(b);
    int64_t c0 = sink.bucket_begin(b);
    int64_t c1 = sink.bucket_end(b);
    size_t k = 0;
    for (int64_t c = c0; c < c1; ++c) {
      std::string cname = "c" + std::to_string(c);
      bool any = false;
      if (obj[c] != 0.0) {
        s += "    "; s += cname; s += "  Obj  "; fmt_num(obj[c], s); s += '\n';
        any = true;
      }
      while (k < e.size() && e[k].col == c) {
        s += "    "; s += cname; s += "  r"; s += std::to_string(e[k].row); s += "  ";
        fmt_num(e[k].x, s); s += '\n';
        ++k; ++written; any = true;
        emit();
      }
      if (!any) { s += "    "; s += cname; s += "  Obj  0\n"; }
      emit();
    }
    if (k != e.size()) throw std::runtime_error("bucket " + std::to_string(b) + " holds entries outside its column range");
  }

  s += "RHS\n";
  {
    int64_t r = 0;
    for (int64_t rf = 0; rf < lp.nrow_full; ++rf) {
      if (map && map->ired[rf] == 0) continue;
      double v = small(lp.rhs[rf]);
      if (v != 0.0) { s += "    RHS  r"; s += std::to_string(r); s += "  "; fmt_num(v, s); s += '\n'; emit(); }
      ++r;
    }
  }
  // no ranged rows: every row is an equality or one-sided

  // bounds: the MPS default is [0, +inf); everything else is stated
  s += "BOUNDS\n";
  for (int64_t c = 0; c < ncol; ++c) {
    int64_t cf = full_of[c];
    double lo = map ? map->col_lo[cf] : lp.col_lo[cf];
    double up = map ? map->col_up[cf] : lp.col_up[cf];
    if (std::isfinite(lo)) lo = small(lo);
    if (std::isfinite(up)) up = small(up);
    std::string cn = "c" + std::to_string(c);
    if (lo == 0.0 && is_inf(up) && up > 0) continue;
    if (is_inf(lo) && is_inf(up)) { s += " FR BND  " + cn + "\n"; emit(); continue; }
    if (lo == up) { s += " FX BND  " + cn + "  "; fmt_num(lo, s); s += '\n'; emit(); continue; }
    if (is_inf(lo)) {
      s += " MI BND  " + cn + "\n";
      s += " UP BND  " + cn + "  "; fmt_num(up, s); s += '\n';
      emit(); continue;
    }
    // a finite lower bound is stated whenever it is not the default, and
    // before a negative upper bound, which some readers would otherwise read
    // as implying a lower bound of -infinity
    if (lo != 0.0 || up < 0.0) { s += " LO BND  " + cn + "  "; fmt_num(lo, s); s += '\n'; }
    if (!is_inf(up)) { s += " UP BND  " + cn + "  "; fmt_num(up, s); s += '\n'; }
    emit();
  }
  s += "ENDATA\n";
  std::fwrite(s.data(), 1, s.size(), out);
  std::fclose(out);
  return written;
}

}  // namespace multimod
