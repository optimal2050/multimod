# Build the LP column index

One row per LP column. Columns are enumerated per variable from the
tuples of the mapping named by `variable$domain`; a variable with no
domain is treated as a scalar and contributes a single column.

## Usage

``` r
build_col_index(
  model,
  verbose = FALSE,
  names = TRUE,
  codes = FALSE,
  exclude = NULL
)
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

- codes:

  `FALSE` (the default) stores the index members as character labels.
  `TRUE` stores them as integer codes of a dictionary made for this
  table and attached to it as the `codes` attribute; passing that
  attribute to another call codes the other table with the same
  dictionary. The streaming writer builds both tables this way, so that
  the members are never held as strings; the files it writes carry the
  labels.

- exclude:

  Internal: a named list (by variable) of coded tuple tables (`i1..iN`)
  whose columns are left out, as a build-time substitution removes them,
  or `TRUE` for a variable that gets no column at all (a scalar
  substituted). Each tuple must be a column of its variable. Needs a
  dictionary (`codes`).

## Value

A `data.table` with columns `j` (1-based column index), `symbol`, `name`
(`symbol.i1.i2...`, when `names = TRUE`), `i1`..`iN` (index members,
`NA` beyond a symbol's arity), and bounds `lo`, `up`, `vtype`.
Per-symbol dimension names are kept in the `dim_names` attribute (a
named list).

## Examples

``` r
cols <- build_col_index(example_models$toy)
table(cols$symbol)
#> 
#>  vCap vCost  vGen 
#>     6     1     6 
```
