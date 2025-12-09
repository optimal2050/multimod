# Quick comparison showing first few differences

Quick comparison showing first few differences

## Usage

``` r
quick_compare(
  df1,
  df2,
  key_col = "name",
  n = 10,
  tolerance = 1e-09,
  ignore.case = FALSE
)
```

## Arguments

- df1:

  First dataframe

- df2:

  Second dataframe

- key_col:

  Column name to use as key (default: "name")

- n:

  Number of examples to show (default: 10)

- tolerance:

  Numeric tolerance for comparing numeric columns (default: 1e-9)

- ignore.case:

  Logical; if TRUE, perform case-insensitive comparison for character
  columns (default: FALSE)
