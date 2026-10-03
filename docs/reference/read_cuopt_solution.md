# Parse a cuOpt solution file

cuOpt writes a different shape from HiGHS: two comment lines carrying
the status and the objective, then one `name value` pair per column.

## Usage

``` r
read_cuopt_solution(file, require_optimal = TRUE)
```

## Arguments

- file:

  Path to the solution file; `.gz` is read transparently.

- require_optimal:

  Logical; error unless the status is optimal. A capped or interrupted
  solve returns a feasible but sub-optimal primal, and presenting that
  as the optimum is the failure this guards.

## Value

The same shape as
[`read_highs_solution()`](https://optimal2050.github.io/multimod/reference/read_highs_solution.md):
`status`, `objective`, `columns` (`name`, `primal`, `dual` = `NA`), and
`rows` = `NULL` to mark the absence of dual information.

## Details

    # Status: Optimal
    # Objective value: 36880.2354212843493
    c0 0
    c1 1.5

There is **no row section**: cuOpt's `--solution-file` writes primal
values only, so nothing on this path yields duals or marginals.
