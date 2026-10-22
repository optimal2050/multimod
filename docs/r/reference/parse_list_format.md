# Parse list format: idx value idx value

Parse list format: idx value idx value

## Usage

``` r
parse_list_format(name, text, default_val)
```

## Value

A list describing the parameter: `name`, `type` (`"parameter"`), `dims`
(1), `defVal` (the default value) and `data`, a data frame with the
index column and a `value` column.
