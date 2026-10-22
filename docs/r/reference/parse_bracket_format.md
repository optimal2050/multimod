# Parse bracket format for parameter values

Parses GMPL bracket format like `[idx1,idx2] value [idx1,idx2] value`.

## Usage

``` r
parse_bracket_format(name, text, default_val)
```

## Value

A list describing the parameter: `name`, `type` (`"parameter"`), `dims`
(the number of index positions), `defVal` (the default value) and
`data`, a data frame with one column per index position and a `value`
column.
