# Split LaTeX math string at top-level operators

Split LaTeX math string at top-level operators

## Usage

``` r
split_top_level_operators(
  latex_str,
  operators = c("+", "-", "\\\\cdot", "\\\\div", "=")
)
```

## Arguments

- latex_str:

  A LaTeX math string

- operators:

  Vector of operators to split at

## Value

A character vector of expression chunks, including the operators
