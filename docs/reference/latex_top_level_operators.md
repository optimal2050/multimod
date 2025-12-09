# Identify top-level LaTeX operators in an expression

Identify top-level LaTeX operators in an expression

## Usage

``` r
latex_top_level_operators(
  latex_str,
  operators = c("+", "-", "\\\\cdot", "\\\\div", "=")
)
```

## Arguments

- latex_str:

  A LaTeX math string

- operators:

  Vector of operators to detect at top level

## Value

Data frame of matched operators and positions
