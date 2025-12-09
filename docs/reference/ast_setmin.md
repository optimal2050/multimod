# Create a set minimum AST node

Constructs an AST node representing a minimum over a set index. This is
distinct from numeric min() functions.

## Usage

``` r
ast_setmin(index, value)
```

## Arguments

- index:

  An AST node representing the index set

- value:

  An AST node representing the value expression

## Value

An object of class `ast` and `setmin`
