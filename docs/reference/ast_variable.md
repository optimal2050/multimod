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

## Value

An `variable` S3 object (subclass of `ast`).
