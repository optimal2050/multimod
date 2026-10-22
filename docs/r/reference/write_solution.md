# Write a solution as one table per variable

Writes each solved variable to its own file, with the index columns
under their real set names rather than positional `i1..iN`.

## Usage

``` r
write_solution(
  solution,
  dir,
  format = c("arrow", "csv", "parquet"),
  variables = NULL,
  nonzero_only = TRUE
)
```

## Arguments

- solution:

  A solution from
  [`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.md)
  or
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md).

- dir:

  Directory to write into; created if absent.

- format:

  One of `"arrow"`, `"csv"`, `"parquet"`.

- variables:

  Optional character vector; write only these variables.

- nonzero_only:

  Drop rows whose value is zero, and skip a variable that is entirely
  zero. `TRUE` by default, which is what solvers' readers generally
  expect.

## Value

The paths written, invisibly, with the kept variable names in the
`"variables"` attribute.

## Details

This is multimod's own layout and carries no assumptions about who reads
it.
[`write_energyrt_output()`](https://optimal2050.github.io/multimod/r/reference/write_energyrt_output.md)
builds on it, adding the extra files energyRt's `read_solution()`
expects.

## See also

[`write_energyrt_output()`](https://optimal2050.github.io/multimod/r/reference/write_energyrt_output.md),
[`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md)

## Examples

``` r
# \donttest{
# Solving takes a few seconds
if (requireNamespace("highs", quietly = TRUE)) {
  m <- example_models$energyRt$multimod
  r <- solve_highs(m, attach = FALSE)
  write_solution(r, file.path(tempdir(), "energyRt_solution"), format = "csv")
}
# }
```
