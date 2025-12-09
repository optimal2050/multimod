# Create a set maximum AST node

Constructs an AST node representing a maximum over a set index. This is
distinct from numeric max() functions.

## Usage

``` r
ast_setmax(index, value)
```

## Arguments

- index:

  An AST node representing the index set

- value:

  An AST node representing the value expression

## Value

An object of class `ast` and `setmax`
