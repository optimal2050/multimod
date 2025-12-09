# Extract the data block body for a declaration ending with a semicolon

Extract the data block body for a declaration ending with a semicolon

## Usage

``` r
extract_data_block(lines, start_line)
```

## Arguments

- lines:

  All lines from the file

- start_line:

  Line number where the declaration starts

## Value

List with data_lines (character vector), header (text between : and :=),
and next_line (first line after the terminating semicolon)
