# Compare CSV files in two directories

Finds CSV files with matching names in two directories and compares
them. Reports files that are only in one directory or the other, and
shows differences for files that exist in both.

## Usage

``` r
compare_csv_files(
  path1,
  path2,
  pattern = "\\.csv$",
  tolerance = 1e-09,
  ignore.case = FALSE,
  key_col = NULL,
  ...
)
```

## Arguments

- path1:

  Path to first directory

- path2:

  Path to second directory

- pattern:

  Regular expression pattern to filter CSV files (default: "\\csv\$")

- tolerance:

  Numeric tolerance for comparing numeric columns (default: 1e-9)

- ignore.case:

  Logical; if TRUE, perform case-insensitive comparison (default: FALSE)

- key_col:

  Column name(s) to use as key (default: NULL = auto-detect). If NULL,
  uses all columns except "value"

- ...:

  Additional arguments passed to compare_dataframes

## Value

List with components:

- only_in_path1: Files only in path1

- only_in_path2: Files only in path2

- in_both: Files in both directories

- comparisons: Named list of comparison results for each file showing
  differences

- identical_files: Files that were compared and identical

- different_files: Files that were compared and had differences
