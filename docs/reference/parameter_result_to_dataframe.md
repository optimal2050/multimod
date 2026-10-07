# Convert evaluated parameter arrays into long-form data frames

Convert evaluated parameter arrays into long-form data frames

## Usage

``` r
parameter_result_to_dataframe(result, dims)
```

## Value

A data frame with one column per dimension and a `value` column, or
`NULL` when `result` is `NULL`.
