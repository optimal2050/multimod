# Recursively apply a function to all nodes of an AST

Recursively apply a function to all nodes of an AST

## Usage

``` r
map_ast(node, func, include_class = TRUE)
```

## Arguments

- node:

  An AST node or list of nodes.

- func:

  A function to apply to each node. It should return a scalar or
  structured value.

- include_class:

  Logical: if TRUE, include class name as part of result tree.

## Value

A tree with the same structure but values replaced by func(node)
