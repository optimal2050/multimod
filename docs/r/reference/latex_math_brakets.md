# Generate LaTeX brackets for math expressions

Generate LaTeX brackets for math expressions

## Usage

``` r
latex_math_brakets(content = NULL, context = NULL, size = NULL)
```

## Arguments

- content:

  A LaTeX string (the expression inside the brackets)

- context:

  Optional context string (e.g., "sum", "prod", or NULL)

- size:

  Optional size prefix for the brackets to pass (e.g., `""`, `"\\big"`,
  `"\\Big"`, `"\\bigg"`, or `"\\Bigg"`)

## Value

A character vector with two elements: `open` and `close`
