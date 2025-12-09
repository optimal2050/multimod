# Split LaTeX math string at top-level operators

This function splits a LaTeX math string at given types of top-level
operators and the maximum length of each chunk. The splitting is done
recursively with respect to the operator order.

## Usage

``` r
split_at_top_level_operators(
  latex_str,
  operators = c("+", "-", "\\cdot", "\\div", "="),
  indent_str = character(0),
  max_len = 80
)
```

## Arguments

- latex_str:

  A LaTeX math string

- operators:

  Vector of operators to split at

## Value

A character vector of expression chunks
