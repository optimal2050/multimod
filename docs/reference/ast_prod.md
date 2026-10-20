# Create a product AST node

Constructs an abstract syntax tree (AST) node representing a product
over an index. The index is typically a `set`, `dims` or `when` object
if filtering is applied to the index.

## Usage

``` r
ast_prod(index, value, domain = NULL)
```

## Arguments

- index:

  An AST node representing the index variable(s) (e.g.,
  `ast_dims("t")`).

- value:

  An AST node representing the expression to be summed.

- domain:

  An optional AST node representing a domain condition. This can be a
  mapping, a logical condition (e.g., from a `$`-filter), or a
  parameter. Use `NULL` if there is no restriction.

## Value

An object of class `ast` and `prod`.
