# Read a solution file, whichever solver wrote it

Read a solution file, whichever solver wrote it

## Usage

``` r
read_solver_solution(
  file,
  solver = c("auto", "highs", "cuopt"),
  require_optimal = TRUE
)
```

## Arguments

- file:

  Path to the solution file.

- solver:

  `"auto"` sniffs the first comment line, or name the format.

- require_optimal:

  Passed through.

## Value

As
[`read_highs_solution()`](https://optimal2050.github.io/multimod/r/reference/read_highs_solution.md).
