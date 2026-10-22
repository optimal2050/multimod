# Create a parameter AST node

Create a parameter AST node

## Usage

``` r
ast_parameter(name, dims = ast_dims(), ...)
```

## Arguments

- name:

  Name of the parameter.

- dims:

  Optional vector of dimension `dims` objects.

- ...:

  Additional fields stored on the node (passed to
  [`new_ast()`](https://optimal2050.github.io/multimod/r/reference/new_ast.md)).

## Value

An `parameter` S3 object (subclass of `ast`).
