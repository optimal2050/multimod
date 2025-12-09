# Compare two dataframes and identify differences

Compare two dataframes and identify differences

## Usage

``` r
compare_dataframes(
  df1,
  df2,
  key_col = NULL,
  compare_cols = NULL,
  label1 = "df1",
  label2 = "df2",
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

  Column name(s) to use as key (default: NULL = auto-detect) If NULL,
  uses all columns except "value" Can be a character vector for
  composite keys

- compare_cols:

  Columns to compare values for (default: all except key)

- label1:

  Label for first dataframe (default: "df1")

- label2:

  Label for second dataframe (default: "df2")

- tolerance:

  Numeric tolerance for comparing numeric columns (default: 1e-9)

- ignore.case:

  Logical; if TRUE, perform case-insensitive comparison for character
  columns (default: FALSE)

## Value

List with components:

- only_in_1: Rows only in df1

- only_in_2: Rows only in df2

- in_both: Row names that appear in both

- differences: Rows where values differ between df1 and df2

- summary: Text summary of differences
