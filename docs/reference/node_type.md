# Get the type of an AST or multimod node

This function retrieves the class of an AST node or multimod object that
represents a specific type of node in the abstract syntax tree.

## Usage

``` r
node_type(x)
```

## Arguments

- x:

  An `ast` or `multimod` object. In case of other classes, it will
  return `NULL`.

## Value

A character string representing the type of the node (e.g.,
`"expression"`, `"variable"`, `"parameter"`, `"sum"`, etc.).
