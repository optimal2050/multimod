# Write a solution in energyRt's `output/` layout

Reproduces what energyRt's generated `output.py` writes, so
[`energyRt::read_solution()`](https://energyRt.org/reference/read.html)
consumes it unchanged: the per-variable tables of
[`write_solution()`](https://optimal2050.github.io/multimod/r/reference/write_solution.md),
plus a `variable_list.csv` naming what was written, a set-membership
table and a run log.

## Usage

``` r
write_energyrt_output(
  solution,
  dir,
  format = c("arrow", "csv", "parquet"),
  variables = NULL
)
```

## Arguments

- solution:

  The list returned by
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md)
  or
  [`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.md).

- dir:

  Output directory; created if absent.

- format:

  `"arrow"` (feather, lz4 - what energyRt writes), `"csv"` or
  `"parquet"`.

- variables:

  Optional character vector limiting which variables to write.

## Value

Invisibly, the paths written.

## See also

[`write_solution()`](https://optimal2050.github.io/multimod/r/reference/write_solution.md)
