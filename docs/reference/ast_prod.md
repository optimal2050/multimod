# Create a product AST node

Constructs an abstract syntax tree (AST) node representing a product
over an index. The index is typically a `set`, `dims` or `when` object
if filtering is applied to the index.

## Usage

``` r
ast_prod(index, value)
```

## Arguments

- index:

  Character. The index variable (e.g., `"t"`).

- value:

  An AST node representing the expression to be summed.

## Value

An object of class `ast` and `prod`.
