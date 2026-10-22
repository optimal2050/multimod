# Create a function AST node

Constructs an abstract syntax tree (AST) node representing a function.

## Usage

``` r
ast_func(name, value, index = NULL, ...)
```

## Arguments

- name:

  A character string representing the function name.

- value:

  An AST node representing the function body or expression.

- index:

  An AST node (typically dims or when) defining the index set.

- ...:

  Additional fields stored on the node (passed to
  [`new_ast()`](https://optimal2050.github.io/multimod/r/reference/new_ast.md)).

## Value

An `ast` object of class `function`.
