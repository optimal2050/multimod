#include "multimod/presolve.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <numeric>
#include <stdexcept>

namespace multimod {

namespace {

// per-column flags of one pass
constexpr uint8_t ENDPOINT1 = 1;  // appears once among the pass's endpoints
constexpr uint8_t ENDPOINT2 = 2;  // appears more than once
constexpr uint8_t SEEN_X = 4;     // already chosen as x in this pass
constexpr uint8_t IS_Y = 8;       // a reference of a kept candidate
constexpr uint8_t IS_X = 16;      // an eliminated column of a kept candidate
constexpr uint8_t THIS_PASS = 32; // eliminated in this pass (for the row update)

// per-record flags
constexpr uint8_t KEEP = 1;
constexpr uint8_t PICK_U = 2;

}  // namespace

PresolveMap presolve_map(Scan& scan, const std::vector<uint8_t>& candidate, const std::vector<uint8_t>& is_int,
                         const double* col_lo, const double* col_up, int max_pass, double tol, int threads,
                         const std::function<void(const std::string&)>& log) {
  (void)threads;
  const int64_t nrow = scan.nrow(), ncol = scan.ncol();
  if (!candidate.empty() && static_cast<int64_t>(candidate.size()) != nrow) throw std::runtime_error("candidate mask does not match the row count");
  if (!is_int.empty() && static_cast<int64_t>(is_int.size()) != ncol) throw std::runtime_error("integer mask does not match the column count");
  auto integer = [&](int32_t c) { return !is_int.empty() && is_int[c - 1]; };

  PresolveMap M;
  M.col_lo.assign(col_lo, col_lo + ncol);
  M.col_up.assign(col_up, col_up + ncol);
  M.nnz.assign(scan.col_counts().begin(), scan.col_counts().end());
  std::vector<double>& clo = M.col_lo;
  std::vector<double>& cup = M.col_up;
  std::vector<int64_t>& nnz = M.nnz;

  // the tracked entries of the candidate rows, each row a record over a
  // contiguous run of entries in ascending column order
  std::vector<int32_t> srow, scol;
  std::vector<double> sx;
  scan.release(srow, scol, sx);
  if (scol.size() > static_cast<size_t>(std::numeric_limits<uint32_t>::max())) throw std::runtime_error("too many tracked entries");
  std::vector<uint32_t> rstart;
  std::vector<int32_t> rid;
  std::vector<int8_t> rlen;
  size_t w = 0;
  bool sorted = true;
  for (size_t k = 0, ne = srow.size(); k < ne;) {
    int32_t r = srow[k];
    size_t e = k;
    while (e < ne && srow[e] == r) ++e;
    if (candidate.empty() || candidate[r - 1]) {
      if (e - k > 127) throw std::runtime_error("a tracked row has more than 127 entries");
      if (!rid.empty() && rid.back() > r) sorted = false;
      rstart.push_back(static_cast<uint32_t>(w));
      rid.push_back(r);
      rlen.push_back(static_cast<int8_t>(e - k));
      for (size_t p = k; p < e; ++p, ++w) { scol[w] = scol[p]; sx[w] = sx[p]; }
    }
    k = e;
  }
  std::vector<int32_t>().swap(srow);
  scol.resize(w); sx.resize(w);
  scol.shrink_to_fit(); sx.shrink_to_fit();
  const size_t nr = rid.size();
  if (!sorted) {
    // the chunks did not arrive in row order: order the records, the
    // entries stay where they are
    std::vector<uint32_t> perm(nr);
    std::iota(perm.begin(), perm.end(), 0u);
    std::stable_sort(perm.begin(), perm.end(), [&](uint32_t a, uint32_t b) { return rid[a] < rid[b]; });
    std::vector<uint32_t> s2(nr); std::vector<int32_t> i2(nr); std::vector<int8_t> l2(nr);
    for (size_t t = 0; t < nr; ++t) { s2[t] = rstart[perm[t]]; i2[t] = rid[perm[t]]; l2[t] = rlen[perm[t]]; }
    rstart.swap(s2); rid.swap(i2); rlen.swap(l2);
  }

  std::vector<uint8_t> cflag(static_cast<size_t>(ncol), 0);
  std::vector<uint8_t> rflag(nr, 0);
  std::vector<uint8_t> elim_col(static_cast<size_t>(ncol), 0), dropped_row(static_cast<size_t>(nrow), 0);
  std::vector<int32_t> step_ref(static_cast<size_t>(ncol), 0);
  std::vector<double> step_coef(static_cast<size_t>(ncol), 1.0);

  auto endpoints = [&](size_t t, int32_t& u, double& au, int32_t& v, double& av) {
    uint32_t a = rstart[t];
    u = scol[a]; au = sx[a]; v = scol[a + 1]; av = sx[a + 1];
  };
  auto pref = [&](int32_t x, int32_t other) {
    return (integer(x) ? 0 : 4) + ((cflag[x - 1] & ENDPOINT2) ? 0 : 2) + (nnz[x - 1] <= nnz[other - 1] ? 1 : 0);
  };

  int passes = 0;
  for (int pass = 1; pass <= max_pass; ++pass) {
    // the candidates: rows with exactly two live entries. How often each
    // column is an endpoint decides which end is eliminated.
    int64_t ncand = 0;
    for (size_t t = 0; t < nr; ++t) {
      if (rlen[t] != 2) continue;
      ++ncand;
      int32_t u, v; double au, av;
      endpoints(t, u, au, v, av);
      for (int32_t c : {u, v}) {
        uint8_t& f = cflag[c - 1];
        f = (f & ENDPOINT1) ? static_cast<uint8_t>(f | ENDPOINT2) : static_cast<uint8_t>(f | ENDPOINT1);
      }
    }
    if (ncand == 0) break;
    // choose x (eliminated) and y (kept) per candidate; keep the first
    // candidate of each x, then no x that is another's y, then no y that is
    // another's x, in row order
    for (size_t t = 0; t < nr; ++t) {
      if (rlen[t] != 2) continue;
      int32_t u, v; double au, av;
      endpoints(t, u, au, v, av);
      bool pick_u = pref(u, v) >= pref(v, u);
      int32_t x = pick_u ? u : v, y = pick_u ? v : u;
      double ax = pick_u ? au : av;
      uint8_t f = pick_u ? PICK_U : 0;
      if (!integer(x) && ax != 0 && !(cflag[x - 1] & SEEN_X)) {
        f |= KEEP;
        cflag[x - 1] |= SEEN_X;
        cflag[y - 1] |= IS_Y;
      }
      rflag[t] = f;
    }
    for (size_t t = 0; t < nr; ++t) {
      if (rlen[t] != 2 || !(rflag[t] & KEEP)) continue;
      int32_t u, v; double au, av;
      endpoints(t, u, au, v, av);
      int32_t x = (rflag[t] & PICK_U) ? u : v;
      if (cflag[x - 1] & IS_Y) rflag[t] &= static_cast<uint8_t>(~KEEP); else cflag[x - 1] |= IS_X;
    }
    for (size_t t = 0; t < nr; ++t) {
      if (rlen[t] != 2 || !(rflag[t] & KEEP)) continue;
      int32_t u, v; double au, av;
      endpoints(t, u, au, v, av);
      int32_t y = (rflag[t] & PICK_U) ? v : u;
      if (cflag[y - 1] & IS_X) rflag[t] &= static_cast<uint8_t>(~KEEP);
    }
    // apply: bounds of x carried onto y, the elimination recorded, the row
    // dropped, the column counts moved
    int64_t n_el = 0;
    for (size_t t = 0; t < nr; ++t) {
      if (rlen[t] != 2) continue;
      int32_t u, v; double au, av;
      endpoints(t, u, au, v, av);
      // the per-pass flags of the endpoints are cleared as they are visited
      cflag[u - 1] &= static_cast<uint8_t>(~(ENDPOINT1 | ENDPOINT2 | SEEN_X | IS_Y | IS_X));
      cflag[v - 1] &= static_cast<uint8_t>(~(ENDPOINT1 | ENDPOINT2 | SEEN_X | IS_Y | IS_X));
      if (!(rflag[t] & KEEP)) continue;
      bool pick_u = rflag[t] & PICK_U;
      int32_t x = pick_u ? u : v, y = pick_u ? v : u;
      double ax = pick_u ? au : av, ay = pick_u ? av : au;
      double cf = -ay / ax;
      double lo_y = cf > 0 ? clo[x - 1] / cf : cup[x - 1] / cf;
      double up_y = cf > 0 ? cup[x - 1] / cf : clo[x - 1] / cf;
      clo[y - 1] = std::max(clo[y - 1], lo_y);
      cup[y - 1] = std::min(cup[y - 1], up_y);
      M.el_col.push_back(x); M.el_ref.push_back(y); M.el_coef.push_back(cf); M.el_pass.push_back(pass);
      elim_col[x - 1] = 1;
      cflag[x - 1] |= THIS_PASS;
      dropped_row[rid[t] - 1] = 1;
      rlen[t] = -1;
      step_ref[x - 1] = y; step_coef[x - 1] = cf;
      // the eliminated column's other entries move to the target, and the
      // target loses its own entry in the dropped row
      nnz[y - 1] += nnz[x - 1] - 2;
      nnz[x - 1] = 0;
      ++n_el;
    }
    if (n_el == 0) break;
    passes = pass;
    if (log) log("doubletons pass " + std::to_string(pass) + ": " + std::to_string(n_el) + " columns and rows eliminated");
    // the tracked rows after this pass: eliminated columns moved to their
    // reference and scaled, entries on one column merged, entries within
    // the tolerance dropped; the merges reduce the column counts
    int64_t gone_total = 0;
    for (size_t t = 0; t < nr; ++t) {
      int len = rlen[t];
      if (len <= 0) continue;
      uint32_t a = rstart[t];
      bool remap = false;
      for (int p = 0; p < len; ++p) {
        int32_t c = scol[a + p];
        if (cflag[c - 1] & THIS_PASS) {
          sx[a + p] *= step_coef[c - 1];
          scol[a + p] = step_ref[c - 1];
          remap = true;
        }
      }
      if (remap) {
        for (int p = 1; p < len; ++p) {
          int32_t c = scol[a + p]; double v = sx[a + p];
          int q = p;
          while (q > 0 && scol[a + q - 1] > c) { scol[a + q] = scol[a + q - 1]; sx[a + q] = sx[a + q - 1]; --q; }
          scol[a + q] = c; sx[a + q] = v;
        }
      }
      int out = 0;
      for (int p = 0; p < len;) {
        int32_t c = scol[a + p]; double v = sx[a + p];
        int m = 1;
        while (p + m < len && scol[a + p + m] == c) { v += sx[a + p + m]; ++m; }
        if (std::fabs(v) > tol) {
          scol[a + out] = c; sx[a + out] = v; ++out;
          nnz[c - 1] -= m - 1; gone_total += m - 1;
        } else {
          nnz[c - 1] -= m; gone_total += m;
        }
        p += m;
      }
      rlen[t] = static_cast<int8_t>(out);
    }
    if (gone_total > 0 && log) log("  " + std::to_string(gone_total) + " entries merged or cancelled in the tracked rows");
    for (size_t k = M.el_col.size() - static_cast<size_t>(n_el); k < M.el_col.size(); ++k) cflag[M.el_col[k] - 1] &= static_cast<uint8_t>(~THIS_PASS);
  }
  M.passes = passes;

  // resolve every eliminated column to a kept one (chains over passes)
  std::vector<int32_t> final_ref(static_cast<size_t>(ncol));
  std::iota(final_ref.begin(), final_ref.end(), 1);
  std::vector<double> final_coef(static_cast<size_t>(ncol), 1.0);
  for (size_t k = M.el_col.size(); k-- > 0;) {
    int32_t x = M.el_col[k], y = M.el_ref[k];
    final_ref[x - 1] = final_ref[y - 1];
    final_coef[x - 1] = M.el_coef[k] * final_coef[y - 1];
  }
  M.ref.assign(static_cast<size_t>(ncol), 0);
  M.coef.assign(static_cast<size_t>(ncol), 1.0);
  M.jred.assign(static_cast<size_t>(ncol), 0);
  int32_t nj = 0;
  for (int64_t c = 0; c < ncol; ++c) {
    if (elim_col[c]) { M.ref[c] = final_ref[c]; M.coef[c] = final_coef[c]; }
    else M.jred[c] = ++nj;
    if (clo[c] > cup[c] + 1e-9 * std::max(1.0, std::fabs(clo[c]))) ++M.crossing;
  }
  M.ired.assign(static_cast<size_t>(nrow), 0);
  int32_t ni = 0;
  for (int64_t r = 0; r < nrow; ++r) if (!dropped_row[r]) M.ired[r] = ++ni;
  return M;
}

}  // namespace multimod
