# Compare two MPS files and report differences

Reads two MPS files using glpkAPI, extracts all constraint and variable
information into data frames, and performs a deep comparison using the
compare package. Constraints and variables are sorted by name before
comparison, making the comparison order-independent.

## Usage

``` r
compare_mps(
  file1,
  file2,
  ignoreOrder = TRUE,
  ignoreNameCase = FALSE,
  verbose = FALSE,
  returnData = FALSE,
  compareMatrix = FALSE,
  matrixTolerance = 1e-09,
  matrixVerbose = FALSE,
  ...
)
```

## Arguments

- file1:

  Path to first MPS file (reference/expected)

- file2:

  Path to second MPS file (to compare)

- ignoreOrder:

  Ignore row order in comparison (default TRUE, passed to
  compare::compare)

- ignoreNameCase:

  Ignore case differences in names (default FALSE, passed to
  compare::compare)

- verbose:

  Logical; if TRUE, print detailed progress (default: FALSE)

- returnData:

  Logical; if TRUE, include the extracted constraint and variable data
  frames in the returned list (default: FALSE)

- ...:

  Additional arguments passed to compare::compare()

## Value

A list with:

- identical:

  Logical, TRUE if models are identical

- dimensions:

  List with nrows, ncols for each model

- constraints:

  List with comparison results for constraints

- variables:

  List with comparison results for variables

- objective:

  Comparison of objective direction

- summary:

  Text summary of differences

## See also

[`compare_lp`](https://optimal2050.github.io/multimod/reference/compare_lp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- compare_mps("original.mps", "reproduced.mps")
if (!result$identical) {
  cat(result$summary, "\n")
  # Examine constraint differences
  print(result$constraints$comparison)
}
} # }
```
