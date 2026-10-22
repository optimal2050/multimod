# Create a symbol AST node (for unclassified identifiers)

Create a symbol AST node (for unclassified identifiers)

## Usage

``` r
ast_symbol(name, ...)
```

## Arguments

- name:

  A character string representing the symbol.

- ...:

  Additional fields stored on the node (passed to
  [`new_ast()`](https://optimal2050.github.io/multimod/r/reference/new_ast.md)).

## Value

An `symbol` S3 object (subclass of `ast`).
