# Get the depth of a nested list structure (AST, multimod, and other objects)

This function calculates the depth of a nested list structure. Wrapper
for
[`purrr::pluck_depth`](https://purrr.tidyverse.org/reference/pluck_depth.html).

## Usage

``` r
depth(x)
```

## Arguments

- x:

  A list or nested list structure.

## Value

An integer representing the depth of the list.
