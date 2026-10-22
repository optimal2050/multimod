# Create a constant AST node

Create a constant AST node

## Usage

``` r
ast_constant(value, ...)
```

## Arguments

- value:

  A numeric or character constant.

- ...:

  Additional fields stored on the node (passed to
  [`new_ast()`](https://optimal2050.github.io/multimod/r/reference/new_ast.md)).

## Value

An `constant` S3 object (subclass of `ast`).
