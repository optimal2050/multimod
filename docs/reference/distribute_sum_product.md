# Inject multiplicative factors into the body of a sum node

Inject multiplicative factors into the body of a sum node

## Usage

``` r
distribute_sum_product(expr_node)
```

## Value

A `sum` AST node whose body is multiplied by the other factors, or
`NULL` when `expr_node` does not contain exactly one `sum` factor.
