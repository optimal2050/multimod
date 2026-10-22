# Build a nested product AST from a list of factor nodes

Build a nested product AST from a list of factor nodes

## Usage

``` r
build_product_ast(factors)
```

## Value

A single AST node: the only factor, or a left-nested `expression` node
with operator `*`.
