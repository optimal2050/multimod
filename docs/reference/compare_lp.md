# Compare two LP files and report differences

Reads two LP files using glpkAPI, extracts all constraint and variable
information into data frames, and performs a deep comparison using the
compare package. Constraints and variables are sorted by name before
comparison, making the comparison order-independent.

## Usage

``` r
compare_lp(
  file1,
  file2,
  ignoreOrder = TRUE,
  ignoreNameCase = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- file1:

  Path to first LP file (reference/expected)

- file2:

  Path to second LP file (to compare)

- ignoreOrder:

  Ignore row order in comparison (default TRUE, passed to
  compare::compare)

- ignoreNameCase:

  Ignore case differences in names (default FALSE, passed to
  compare::compare)

- verbose:

  Logical; if TRUE, print detailed progress (default: FALSE)

- ...:

  Additional arguments passed to compare::compare()

- compareMatrix:

  Logical; if TRUE, compare every non-zero coefficient in the constraint
  matrix (default FALSE because it can be expensive)

- matrixTolerance:

  Absolute tolerance used when flagging coefficient differences (default
  1e-9)

- matrixVerbose:

  Logical; emit progress while extracting matrices

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

- constraints1:

  Constraint data frame for file1 when returnData = TRUE

- constraints2:

  Constraint data frame for file2 when returnData = TRUE

- variables1:

  Variable data frame for file1 when returnData = TRUE

- variables2:

  Variable data frame for file2 when returnData = TRUE

- objective:

  Comparison of objective direction

- summary:

  Text summary of differences

## See also

[`compare_mps`](https://optimal2050.github.io/multimod/reference/compare_mps.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- compare_lp("original.lp", "reproduced.lp")
if (!result$identical) {
  cat(result$summary, "\n")
  # Examine constraint differences
  print(result$constraints$comparison)
}
} # }
```
