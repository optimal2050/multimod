# Create a "where" AST node

Constructs a "where" node representing a reference to a specific
location in the abstract syntax tree (AST).

## Usage

``` r
ast_where(name, content, hash = node_hash(content), ...)
```

## Arguments

- name:

  A character string representing the name of the reference, matching
  the name of the symbol replacing AST node or a branch of the AST.

- content:

  The AST content

- hash:

  Hash value for the node

- ...:

  Additional attributes

## Value

An AST where node
