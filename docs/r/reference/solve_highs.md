# Solve a model in-process with HiGHS

Uses the solver-object interface rather than
[`highs::highs_solve()`](https://rdrr.io/pkg/highs/man/highs_solve.html),
because only that path returns dual values (constraint marginals) as
well as primal levels.

## Usage

``` r
solve_highs(model, lp = NULL, attach = TRUE, control = list(), verbose = FALSE)
```

## Arguments

- model:

  A multimod model with data attached.

- lp:

  Optional pre-built LP from
  [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md).

- attach:

  Logical; attach primal values back onto `model$variables` as a long
  data.frame with named index columns, matching what
  [`read_csv_results()`](https://optimal2050.github.io/multimod/r/reference/read_csv_results.md)
  produces so downstream readers keep working.

- control:

  Optional list of HiGHS options.

- verbose:

  Logical; report progress.

## Value

A list with `objective`, `status`, `status_message`, `primal` (a
`data.table` of `j`, symbol, index tuple and value), `dual` (per row),
the `lp`, and - when `attach` is `TRUE` - the updated `model`.

## Examples

``` r
if (requireNamespace("highs", quietly = TRUE)) {
  r <- solve_highs(example_models$toy, attach = FALSE)
  r$objective
}
#> [1] 198000
```
