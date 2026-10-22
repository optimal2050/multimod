# Convert sum/prod with conditional indexing to JuMP syntax

Convert sum/prod with conditional indexing to JuMP syntax

## Usage

``` r
as_jump_sum_prod(x, model = NULL, ...)
```

## Arguments

- x:

  A func AST node representing sum or prod

- ...:

  Additional arguments passed to as_jump

## Value

JuMP sum/prod expression with proper filtering
