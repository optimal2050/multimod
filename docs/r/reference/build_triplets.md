# Extract (row, column, coefficient) triplets from a model

Walks each equation's AST set-at-a-time and returns the LP matrix
entries together with the right-hand side implied by the equation's
constant terms.

## Usage

``` r
build_triplets(
  model,
  col_index = NULL,
  row_index = NULL,
  equations = NULL,
  on_duplicate = c("error", "last", "first"),
  verbose = FALSE,
  chunk_rows = 1000000L,
  sink = NULL,
  substitute = NULL
)
```

## Arguments

- model:

  A multimod model with data attached.

- col_index, row_index:

  Index tables from
  [`build_col_index()`](https://optimal2050.github.io/multimod/r/reference/build_col_index.md)
  /
  [`build_row_index()`](https://optimal2050.github.io/multimod/r/reference/build_row_index.md).
  Built on demand when not supplied.

- equations:

  Optional character vector limiting which equations to process; useful
  when bringing the backend up equation by equation.

- on_duplicate:

  What to do when a parameter stores the same index tuple with more than
  one distinct value. `"error"` (default) refuses, because the lookup is
  genuinely ambiguous; `"last"` takes the last, matching what GAMS and
  Pyomo do silently; `"first"` takes the first. Either non-default
  choice reports the parameter and the number of tuples affected.

- verbose:

  Logical; report per-equation term counts and timings.

- chunk_rows:

  Integer; evaluate each equation over at most this many LP rows at a
  time. Each LP row's terms are independent, so slicing by row is exact;
  what it bounds is the pre-aggregation intermediate of a
  [`sum()`](https://rdrr.io/r/base/sum.html) (outer rows x inner
  tuples), which on a large full-year model otherwise allocates tens of
  gigabytes in one piece. The aggregated per-chunk triplets concatenate
  without renumbering (`.k` carries the global row id).

- sink:

  Internal: an open streaming sink (see
  [`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.md)
  with `stream = TRUE`). The matrix entries are written to it as each
  chunk is built and `triplets` comes back empty; `rhs` is returned as
  usual.

- substitute:

  Internal: definition equations substituted at build time, as prepared
  for the streaming writer (see
  [`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.md)).
  The index tables must have been built without their rows and columns;
  the equations that refer to a substituted variable are lowered for the
  native executor, and the build's context is returned in the `ctx`
  attribute.

## Value

A list with `triplets` (`data.table(i, j, coef)`, aggregated), `rhs`
(`data.table(i, rhs)`), and `row_index` / `col_index` as used.

## Native engine

`options(multimod.engine = "native")` runs the equations that the
block-plan lowering covers in compiled code; the others stay on the R
engine. Further options: `multimod.threads` (threads of the executor,
default 2), `multimod.slice_rows` (rows a term's expansion may reach
before it runs in slices, default `16e6`) and `multimod.cache_gb` (cap
on the executor's index cache, default no cap).
