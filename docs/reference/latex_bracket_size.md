# Estimate bracket size for LaTeX based on context and expression content

Estimate bracket size for LaTeX based on context and expression content

## Usage

``` r
latex_bracket_size(content, context = NULL)
```

## Arguments

- content:

  A LaTeX string (the expression inside the brackets)

- context:

  Optional context string (e.g., "sum", "prod", or NULL)

## Value

A LaTeX bracket size prefix (e.g., "", "\big", "\Big", etc.)
