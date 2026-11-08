# Build the LP row index

One row per LP constraint. Rows are enumerated per equation from the
tuples of the mapping named by `equation$domain`; an equation with no
domain is treated as a scalar and contributes a single row.

## Usage

``` r
build_row_index(
  model,
  verbose = FALSE,
  names = TRUE,
  codes = FALSE,
  skip = NULL
)
```

## Arguments

- model:

  A multimod model with data attached.

- verbose:

  Logical; report per-equation row counts.

- names:

  Logical; add the readable `name` column — see
  [`build_col_index()`](https://optimal2050.github.io/multimod/r/reference/build_col_index.md).

- codes:

  Store the index members as integer codes of a dictionary — see
  [`build_col_index()`](https://optimal2050.github.io/multimod/r/reference/build_col_index.md).
  The row index of an LP is coded with the dictionary of its column
  index.

- skip:

  Internal: names of equations that get no rows (definitions substituted
  at build time), or a named list by equation of `TRUE` (no rows) or a
  coded tuple table (`i1..iN`, the equation's index order) of the rows
  left out.

## Value

A `data.table` with columns `i` (1-based row index), `symbol`, `name`
(when `names = TRUE`), `i1`..`iN`, `sense` (`"=="`, `"<="`, `">="`) and
`rhs` (`NA_real_` until coefficient extraction fills it).

## Details

Counting is **per equation**, which is what an LP needs. energyRt's
`model_size()` counts per *map* and therefore undercounts whenever one
mapping gates several equations.

## Examples

``` r
rows <- build_row_index(example_models$toy)
table(rows$symbol)
#> 
#> eqCapLim   eqCost eqDemand 
#>        6        1        2 
```
