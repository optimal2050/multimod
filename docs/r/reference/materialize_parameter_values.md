# Convert parameter data frames into arrays or vectors for indexing

Convert parameter data frames into arrays or vectors for indexing

## Usage

``` r
materialize_parameter_values(param, data = NULL, dim_members = list())
```

## Value

`NULL` when the parameter carries no data or a dimension has no members.
For a scalar parameter, its value. Otherwise an array with one dimension
per entry of `param$dims`, with `dimnames` taken from `dim_members`
(extended by the members observed in the data), holding the parameter
values; cells without data take the parameter's default value, or a
scalar `data`, or `NA`.
