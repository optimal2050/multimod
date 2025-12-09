# Split expression by operator at top level

Split expression by operator at top level

## Usage

``` r
split_by_op(expr, op_pattern, rightmost = FALSE)
```

## Arguments

- expr:

  Character string

- op_pattern:

  Regular expression pattern for operator

- rightmost:

  Logical; if TRUE, split at rightmost occurrence (for right-associative
  operators)

## Value

List with lhs and rhs
