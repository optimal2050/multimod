# Create a variable AST node

Create a variable AST node

## Usage

``` r
ast_variable(name, dims = ast_dims(), vtype = NULL, bounds = NULL, ...)
```

## Arguments

- name:

  Name of the variable.

- dims:

  Optional vector of dimension `dims` objects.

- vtype:

  Optional variable type as declared in the source model (e.g.
  `"positive"`, `"binary"`, `"integer"`).

- bounds:

  Optional list of bounds with elements `lo` and/or `up`.

- ...:

  Additional fields stored on the node (passed to
  [`new_ast()`](https://optimal2050.github.io/multimod/reference/new_ast.md)).

## Value

An `variable` S3 object (subclass of `ast`).
