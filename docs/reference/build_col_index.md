# Build the LP column index

One row per LP column. Columns are enumerated per variable from the
tuples of the mapping named by `variable$domain`; a variable with no
domain is treated as a scalar and contributes a single column.

## Usage

``` r
build_col_index(model, verbose = FALSE, names = TRUE)
```

## Arguments

- model:

  A multimod model with data attached.

- verbose:

  Logical; report per-variable column counts.

- names:

  Logical; add the readable `name` column (`symbol.i1.i2...`). `FALSE`
  skips it — on large models the names dominate index-building memory,
  and the MPS/solution path never reads them.

## Value

A `data.table` with columns `j` (1-based column index), `symbol`, `name`
(`symbol.i1.i2...`, when `names = TRUE`), `i1`..`iN` (index members,
`NA` beyond a symbol's arity), and bounds `lo`, `up`, `vtype`.
Per-symbol dimension names are kept in the `dim_names` attribute (a
named list).
