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

## Value

An `ast` object of class `function`.
