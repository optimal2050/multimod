# Presolve an assembled LP

Exact reductions applied before the matrix is handed to a solver. The
one step implemented, `"doubletons"`, removes every equality row of the
form `a * x + b * y = 0` together with column `x`, substituting
`x = c * y` (`c = -b / a`) into the rest of the matrix, the objective
and `y`'s bounds. Integer columns are never eliminated. The reduced LP
solves to the same optimum;
[`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md)
restores the eliminated columns from the table kept in the result.

## Usage

``` r
lp_presolve(
  lp,
  steps = "doubletons",
  max_pass = 50L,
  tol = 1e-12,
  verbose = FALSE
)
```

## Arguments

- lp:

  An LP from
  [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md).

- steps:

  Reductions to apply; `"doubletons"` is the only one so far.

- max_pass:

  Maximum number of passes; chains need one pass per link.

- tol:

  Entries smaller than this in absolute value after substitution are
  dropped (exact cancellations that floating point leaves behind).

- verbose:

  Logical; report each pass.

## Value

The LP with `A`, `obj` and the bounds reduced. `col_index` and
`row_index` stay complete and gain `jred` / `ired`, the position of each
kept column / row in the reduced LP (`NA` when eliminated); `eliminated`
lists the removed columns (`col`, `ref`, `coef`, `pass`, positions in
the full index) so that `value[col] = coef * value[ref]`, applied from
the last pass to the first.
