# Extract the sparse constraint matrix of an MPS file

Reads an MPS file via glpkAPI and returns the constraint matrix in
long/tidy form so it can be grouped by constraint or variable. Optional
regular expressions allow filtering rows/columns before extraction and
the resulting data can be split per constraint or per variable.

## Usage

``` r
mps_matrix_long(
  file,
  rows = NULL,
  cols = NULL,
  partition = c("none", "row", "column"),
  verbose = FALSE
)
```

## Arguments

- file:

  Path to the MPS file

- rows:

  Optional character vector of regular expressions used to select
  constraint (row) names. Default NULL keeps every constraint.

- cols:

  Optional character vector of regular expressions used to select
  variable (column) names. Default NULL keeps every variable.

- partition:

  One of "none", "row", or "column" specifying whether the returned
  object should be a single data frame (none) or a list split by
  constraint/variable.

- verbose:

  Logical; print progress while extracting (default FALSE)

## Value

Either a data frame with columns (row, col, coef) or a named list of
such data frames if partitioning is requested.
