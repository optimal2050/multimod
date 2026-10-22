# Compare variable solutions between two models

Compare variable solutions between two models

## Usage

``` r
compare_variables(
  model1,
  model2,
  variables = NULL,
  tol = 1e-05,
  keep = c("differences", "all", "matches"),
  allowAll = TRUE,
  verbose = TRUE
)
```

## Arguments

- model1:

  First model object (reference/expected)

- model2:

  Second model object (to compare against model1)

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
