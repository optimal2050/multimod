# Parse simple table format: : cols := rows values

Parse simple table format: : cols := rows values

## Usage

``` r
parse_simple_table_format(name, header, text, default_val)
```

## Value

A list describing the parameter: `name`, `type` (`"parameter"`), `dims`
(2), `defVal` (the default value) and `data`, a data frame with the row
index, the column index and a `value` column.
