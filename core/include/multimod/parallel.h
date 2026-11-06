// A parallel loop over a row range on std::thread.
//
// The range [0, n) is split into `threads` contiguous chunks; the calling
// thread runs the last chunk. An exception thrown in a worker is rethrown on
// the calling thread after every worker has joined. A worker that cannot be
// started runs its chunk on the calling thread instead.
#pragma once
#include <algorithm>
#include <cstdint>
#include <exception>
#include <system_error>
#include <thread>
#include <vector>

namespace multimod {

template <class Fn>
void parallel_for(int64_t n, int threads, Fn fn) {
  if (n <= 0) return;
  int t = std::max(1, threads);
  // small ranges are not worth a thread each
  int64_t min_chunk = 16384;
  if (t > 1) t = static_cast<int>(std::min<int64_t>(t, std::max<int64_t>(1, n / min_chunk)));
  if (t == 1) { fn(0, n, 0); return; }
  int64_t chunk = (n + t - 1) / t;
  std::vector<std::thread> workers;
  workers.reserve(t - 1);
  std::vector<std::exception_ptr> errors(t);
  for (int w = 0; w < t - 1; ++w) {
    int64_t b = w * chunk, e = std::min(n, b + chunk);
    auto task = [&, w, b, e]() {
      try { fn(b, e, w); } catch (...) { errors[w] = std::current_exception(); }
    };
    try { workers.emplace_back(task); } catch (const std::system_error&) { task(); }
  }
  int64_t b = (t - 1) * chunk, e = n;
  try { if (b < e) fn(b, e, t - 1); } catch (...) { errors[t - 1] = std::current_exception(); }
  for (auto& th : workers) th.join();
  for (auto& ex : errors) if (ex) std::rethrow_exception(ex);
}

// The chunk boundaries parallel_for() uses for a range, so that a two-pass
// algorithm can count per chunk and fill at prefix-sum offsets.
inline std::vector<int64_t> chunk_bounds(int64_t n, int threads) {
  int t = std::max(1, threads);
  int64_t min_chunk = 16384;
  if (t > 1) t = static_cast<int>(std::min<int64_t>(t, std::max<int64_t>(1, n / min_chunk)));
  std::vector<int64_t> b;
  if (n <= 0) { b.push_back(0); return b; }
  int64_t chunk = (n + t - 1) / t;
  for (int w = 0; w < t; ++w) b.push_back(std::min(n, w * chunk));
  b.push_back(n);
  return b;
}

// Run `fn(chunk, begin, end)` over the chunks of `bounds` in parallel.
template <class Fn>
void parallel_chunks(const std::vector<int64_t>& bounds, Fn fn) {
  int t = static_cast<int>(bounds.size()) - 1;
  if (t <= 0) return;
  if (t == 1) { fn(0, bounds[0], bounds[1]); return; }
  std::vector<std::thread> workers;
  workers.reserve(t - 1);
  std::vector<std::exception_ptr> errors(t);
  for (int w = 0; w < t - 1; ++w) {
    auto task = [&, w]() {
      try { fn(w, bounds[w], bounds[w + 1]); } catch (...) { errors[w] = std::current_exception(); }
    };
    try { workers.emplace_back(task); } catch (const std::system_error&) { task(); }
  }
  try { fn(t - 1, bounds[t - 1], bounds[t]); } catch (...) { errors[t - 1] = std::current_exception(); }
  for (auto& th : workers) th.join();
  for (auto& ex : errors) if (ex) std::rethrow_exception(ex);
}

}  // namespace multimod
