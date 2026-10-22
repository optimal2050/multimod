# Parse sliced table format with fixed dimensions and wildcards

Parses GMPL sliced table format like
`[fixed,*,*] : cols := rows values`. Can have multiple sliced blocks in
one parameter.

## Usage

``` r
parse_sliced_table_format(name, header, text, default_val)
```

## Value

A list describing the parameter: `name`, `type` (`"parameter"`), `dims`
(the number of index positions), `defVal` (the default value) and
`data`, a data frame with one column per index position and a `value`
column.
