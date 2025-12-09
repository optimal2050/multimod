# Compare solutions from different solver outputs

Directly compares variable solution files without loading full model.
Handles different data formats (e.g., CSV from GMPL, Arrow from JuMP).

## Usage

``` r
compare_solution(
  path1,
  path2,
  solver1 = NULL,
  solver2 = NULL,
  variables = NULL,
  tol = 1e-05,
  keep = c("differences", "all", "matches"),
  allowAll = TRUE,
  verbose = TRUE
)
```

## Arguments

- path1:

  Path to first solver output directory (reference/expected)

- path2:

  Path to second solver output directory (to compare)

- solver1:

  Solver name for path1 if it's a model root (e.g., "gmpl", "jump").
  NULL for direct solution paths.

- solver2:

  Solver name for path2 if it's a model root (e.g., "gmpl", "jump").
  NULL for direct solution paths.

- variables:

  Character vector of variable names to compare. If NULL, compares all
  common variables.

- tol:

  Numeric tolerance for comparing values (default 1e-5)

- keep:

  What to return: "all", "differences", "matches" (default
  "differences")

- allowAll:

  Passed to compare::compare() - allows all transformations (default
  TRUE)

- verbose:

  Print comparison details (default TRUE)

## Value

A list with components:

- summary: Overall comparison summary

- variables: List of comparison results per variable

- differences: Variables with differences (if any)

- objective: Objective value comparison

- paths: Paths to the compared models
