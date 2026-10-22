# Create a solver session

Builds a `MultimodSession`: an R6 object holding the state the matrix
route otherwise threads through every call - the model, the assembled
LP, the solution and the shared options - and forwarding each step to
the ordinary exported function.

## Usage

``` r
mm_session(
  model = NULL,
  verbose = FALSE,
  on_duplicate = c("error", "last", "first"),
  chunk_rows = 1000000L,
  control = list()
)
```

## Arguments

- model:

  A multimod model with data attached.

- verbose:

  Logical; forwarded to every step.

- on_duplicate:

  Passed to
  [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md).

- chunk_rows:

  Passed to
  [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md).

- control:

  Solver options passed to
  [`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.md).

## Value

An R6 object of class `MultimodSession`.

## Details

Every method returns the session invisibly, so calls chain. Results are
reachable as fields (`$lp`, `$solution`) and are the same objects the
functional API returns. The session is a convenience layer: it does not
replace
[`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md),
[`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.md)
or
[`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.md),
which remain the supported interface.

Methods: `$build_lp()`, `$check_numbers()`, `$solve()`, `$write_mps()`,
`$read_solution()`, `$write_output()`, and the accessors `$numbers()`,
`$mps_paths()`, `$output_files()`.

## Copy semantics

This is an R6 class, so a session is a reference: `s2 <- s1` makes an
alias, not a copy. Use `s$clone(deep = TRUE)` for an independent
session; the deep clone here also copies `data.table` fields, which R6's
own deep clone does not.
