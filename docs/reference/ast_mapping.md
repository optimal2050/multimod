# Create a mapping AST node

Constructs a mapping node representing a mapping or set of indices.

## Usage

``` r
ast_mapping(name, dims = ast_dims(), ...)
```

## Arguments

- name:

  A character string representing the mapping name.

- dims:

  An `dims` object representing the dimensions of the mapping.

## Value

An `mapping` S3 object (subclass of `ast`).
