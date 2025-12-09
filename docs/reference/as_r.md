# Convert AST to R Code

Converts AST nodes to executable R code. This is the foundation for
creating R functions from AST expressions.

## Usage

``` r
as_r(x, ...)
```

## Arguments

- x:

  An AST node (expression, symbol, constant, etc.)

- ...:

  Additional arguments passed to methods

## Value

A character string containing R code
