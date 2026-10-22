# Split function arguments at the top level

This function splits an argument string like
`"x, y+z, f(a,b), g(i)$c(i)"` into individual arguments while respecting
nested parentheses.

## Usage

``` r
split_top_level_args(expr_str)
```

## Arguments

- expr_str:

  A string containing comma-separated expressions

## Value

A character vector of top-level arguments
