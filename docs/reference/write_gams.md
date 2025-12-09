# Write a GAMS model file from a multimod object

Write a GAMS model file from a multimod object

## Usage

``` r
write_gams(model, file = NULL, format_expr = TRUE, ...)
```

## Arguments

- model:

  A `multimod` model object

- file:

  Output file path (optional)

- format_expr:

  logical; whether to format expressions with line breaks

- ...:

  Additional arguments passed to formatting functions

## Value

Character vector or writes file if `file` is given
