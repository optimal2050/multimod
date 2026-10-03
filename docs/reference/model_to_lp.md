# Assemble a model into LP arrays

Assemble a model into LP arrays

## Usage

``` r
model_to_lp(
  model,
  col_index = NULL,
  row_index = NULL,
  on_duplicate = c("error", "last", "first"),
  verbose = FALSE,
  index_names = FALSE,
  on_empty_row = c("user_constraints", "all", "warn", "ignore"),
  chunk_rows = 1000000L
)
```

## Arguments

- model:

  A multimod model with data attached.

- col_index, row_index:

  Optional index tables; built when not supplied.

- on_duplicate:

  Passed to
  [`build_triplets()`](https://optimal2050.github.io/multimod/reference/build_triplets.md);
  how to resolve a parameter that stores one index tuple with several
  distinct values.

- verbose:

  Logical; report progress.

- index_names:

  Logical; carry the readable `name` column in the index tables. Default
  `FALSE`: nothing on the solve/MPS/decode path reads the names, and on
  a large model they are the biggest single allocation of index building
  (one interned string per LP row and column).

- on_empty_row:

  What to do about equation rows that carry no coefficients. Such a row
  reads `0 <op> rhs`: it constrains nothing, yet the model still
  assembles and the solver still reports Optimal, so the answer is
  silently to a different question. `"user_constraints"` (the default)
  refuses when a USER constraint (`eqCns*`) is empty – a relaxed user
  constraint is never what the author meant – and warns about any other
  empty rows. `"all"` refuses on any empty row, `"warn"` only warns,
  `"ignore"` says nothing.

- chunk_rows:

  Passed to
  [`build_triplets()`](https://optimal2050.github.io/multimod/reference/build_triplets.md):
  evaluate each equation over at most this many LP rows at a time. It
  bounds the pre-aggregation intermediate of a
  [`sum()`](https://rdrr.io/r/base/sum.html) (outer rows x inner
  tuples), which is the transient that dominates peak memory on a large
  full-year build. Slicing by row is exact – each LP row's terms are
  independent, and the per-chunk triplets concatenate without
  renumbering.

## Value

A list with `A` (a `Matrix::dgCMatrix`), `obj`, `row_lo`, `row_up`,
`col_lo`, `col_up`, `maximum`, and the `row_index` / `col_index` used.
Row bounds encode the constraint sense: `==` gives equal bounds, `<=` an
upper bound only, `>=` a lower bound only.
