# Map a solved MPS back onto model symbols

Map a solved MPS back onto model symbols

## Usage

``` r
read_mps_solution(
  sol,
  index_dir = NULL,
  col_index = NULL,
  row_index = NULL,
  base = NULL,
  format = c("parquet", "ipc", "csv"),
  solver = c("auto", "highs", "cuopt"),
  require_optimal = TRUE
)
```

## Arguments

- sol:

  Path to a HiGHS solution file, or the list returned by
  [`read_highs_solution()`](https://optimal2050.github.io/multimod/reference/read_highs_solution.md).

- index_dir:

  Directory holding the `*_col_index` / `*_row_index` tables written by
  [`write_mps()`](https://optimal2050.github.io/multimod/reference/write_mps.md).
  Ignored when both index tables are supplied.

- col_index, row_index:

  Index tables, if already in memory.

- base:

  Basename used when the index files were written; inferred from the
  directory when there is exactly one candidate.

- format:

  Storage format of the index tables.

- solver:

  Which solver wrote `sol`; `"auto"` sniffs the file.

- require_optimal:

  Passed to the reader.

## Value

A list with `objective`, `status`, `primal` (`j`, symbol, name, value)
and `dual` (`i`, symbol, name, value, marginal) - the same shape
[`solve_highs()`](https://optimal2050.github.io/multimod/reference/solve_highs.md)
returns, so the two paths are interchangeable.
