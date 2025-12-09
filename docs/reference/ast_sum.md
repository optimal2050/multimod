# Create a summation AST node

Constructs an abstract syntax tree (AST) node representing a summation
over an index. The index is typically a `set`, `dims` or `when` object
if filtering is applied to the index.

## Usage

``` r
ast_sum(index = ast_dims(), value)
```

## Arguments

- index:

  Character. The index variable (e.g., `"t"`).

- value:

  An AST node representing the expression to be summed.

## Value

An object of class `ast` and `sum`.
