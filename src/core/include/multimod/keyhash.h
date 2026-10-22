// Open-addressing hash index over composite integer keys.
//
// A table is k integer columns of n rows. The index maps each distinct key
// tuple to the chain of rows carrying it, so a lookup returns one position
// (the first row) and a merge enumerates every matching row. An NA key
// (INT_MIN) matches nothing in a join; a group key may be NA.
//
// When the key columns fit in 64 bits together (codes are small integers,
// which member dictionaries guarantee), each row's key is packed into one
// word: the hash is one multiply-shift and an equality test is one compare,
// instead of one scattered read per column.
#pragma once
#include <climits>
#include <cstdint>
#include <vector>

namespace multimod {

using KeyCols = std::vector<const int*>;

inline uint64_t mix64(uint64_t h) {
  h ^= h >> 33; h *= 0xff51afd7ed558ccdULL;
  h ^= h >> 33; h *= 0xc4ceb9fe1a85ec53ULL;
  h ^= h >> 33;
  return h;
}

inline uint64_t hash_row(const KeyCols& cols, int64_t row) {
  uint64_t h = 0x9e3779b97f4a7c15ULL;
  for (const int* c : cols) {
    h = mix64(h ^ (static_cast<uint64_t>(static_cast<uint32_t>(c[row])) * 0x9e3779b97f4a7c15ULL));
  }
  return h;
}

inline bool row_has_na(const KeyCols& cols, int64_t row) {
  for (const int* c : cols) if (c[row] == INT_MIN) return true;
  return false;
}

inline bool rows_equal(const KeyCols& a, int64_t i, const KeyCols& b, int64_t j) {
  for (size_t k = 0; k < a.size(); ++k) if (a[k][i] != b[k][j]) return false;
  return true;
}

struct KeyIndex {
  KeyCols cols;
  int64_t n = 0;
  bool na_matches = false;
  std::vector<int64_t> slot;   // first row of a chain, or -1
  std::vector<int64_t> next;   // next row with the same key, or -1
  uint64_t mask = 0;

  // packed mode: per-column bit widths and the packed key of every row
  bool packed = false;
  std::vector<int> shift;      // bit offset of each column
  std::vector<uint64_t> lim;   // exclusive upper bound of each column's code (+1 for NA)
  std::vector<uint64_t> key;   // packed key per row

  static const uint64_t NOKEY = ~0ULL;

  // Pack one row of `q` with this index's layout; NOKEY when a code does
  // not fit the layout (then it cannot occur in the table) or is NA and NA
  // does not match.
  uint64_t pack(const KeyCols& q, int64_t row) const {
    uint64_t k = 0;
    for (size_t c = 0; c < q.size(); ++c) {
      int v = q[c][row];
      uint64_t u;
      if (v == INT_MIN) { if (!na_matches) return NOKEY; u = 0; }
      else { if (v < 0) return NOKEY; u = static_cast<uint64_t>(v) + 1; }
      if (u >= lim[c]) return NOKEY;
      k |= u << shift[c];
    }
    return k;
  }

  void build(const KeyCols& c, int64_t nrow) {
    cols = c; n = nrow;
    // can the columns be packed? each needs bits for max code + 2
    packed = !cols.empty();
    shift.assign(cols.size(), 0); lim.assign(cols.size(), 0);
    int bits = 0;
    for (size_t k = 0; k < cols.size() && packed; ++k) {
      int mx = 0;
      for (int64_t r = 0; r < nrow; ++r) { int v = cols[k][r]; if (v != INT_MIN && v > mx) mx = v; }
      uint64_t need = static_cast<uint64_t>(mx) + 2;   // codes 0..mx map to 1..mx+1; 0 is NA
      int w = 1; while ((1ULL << w) < need) ++w;
      shift[k] = bits; lim[k] = 1ULL << w; bits += w;
      if (bits > 63) packed = false;
    }
    uint64_t cap = 16;
    while (cap < static_cast<uint64_t>(nrow) * 2) cap <<= 1;
    mask = cap - 1;
    slot.assign(cap, -1);
    next.assign(static_cast<size_t>(nrow), -1);
    if (packed) {
      key.resize(nrow);
      for (int64_t r = 0; r < nrow; ++r) key[r] = pack(cols, r);
      for (int64_t r = nrow - 1; r >= 0; --r) {
        if (key[r] == NOKEY) continue;
        uint64_t p = mix64(key[r]) & mask;
        for (;;) {
          int64_t s = slot[p];
          if (s < 0) { slot[p] = r; break; }
          if (key[s] == key[r]) { next[r] = s; slot[p] = r; break; }
          p = (p + 1) & mask;
        }
      }
      return;
    }
    // inserted in reverse so that a chain enumerates rows in ascending order
    for (int64_t r = nrow - 1; r >= 0; --r) {
      if (!na_matches && row_has_na(cols, r)) continue;
      uint64_t p = hash_row(cols, r) & mask;
      for (;;) {
        int64_t s = slot[p];
        if (s < 0) { slot[p] = r; break; }
        if (rows_equal(cols, s, cols, r)) { next[r] = s; slot[p] = r; break; }
        p = (p + 1) & mask;
      }
    }
  }

  // first row whose key equals probe row `i` of `q`, or -1
  int64_t find(const KeyCols& q, int64_t i) const {
    if (slot.empty()) return -1;
    if (packed) {
      uint64_t k = pack(q, i);
      if (k == NOKEY) return -1;
      uint64_t p = mix64(k) & mask;
      for (;;) {
        int64_t s = slot[p];
        if (s < 0) return -1;
        if (key[s] == k) return s;
        p = (p + 1) & mask;
      }
    }
    if (!na_matches && row_has_na(q, i)) return -1;
    uint64_t p = hash_row(q, i) & mask;
    for (;;) {
      int64_t s = slot[p];
      if (s < 0) return -1;
      if (rows_equal(cols, s, q, i)) return s;
      p = (p + 1) & mask;
    }
  }
};

}  // namespace multimod
