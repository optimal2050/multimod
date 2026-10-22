# Create a new multimod AST (Abstract Syntax Tree) node

This is a generic constructor for all `ast` node types, used internally
by the `*()` functions to define structured equations.

Constructs a set object used as an index domain for other symbols (e.g.,
mappings, parameters, variables, equations).

## Usage

``` r
new_ast(node_type, ..., inherits_class = NULL)

ast_set(name)
```

## Arguments

- node_type:

  The type of AST node (e.g., `"expression"`, `"variable"`,
  `"constant"`, `"parameter"`, `"symbol"`).

- ...:

  Additional named fields passed as components of the AST node.

- inherits_class:

  Optional character vector of extra classes inserted between
  `node_type` and `"ast"` in the class attribute.

- name:

  Character. Name of the set (e.g., "b").

## Value

A structured S3 object of class `ast`, with a subclass matching
`node_type`.

An object of class `ast` and subclass `set`.

## Examples

``` r
ast_set("b")                     # standalone set
#> <AST set> b
```
