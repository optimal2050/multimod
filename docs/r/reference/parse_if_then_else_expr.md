# Parse if-then-else expression

Extracts condition, then_expr, and else_expr from GMPL if-then-else
syntax. Only matches if keywords are at depth 0 (not inside
parentheses/brackets).

## Usage

``` r
parse_if_then_else_expr(expr)
```

## Arguments

- expr:

  Character string containing the expression

## Value

List with condition, then_expr, else_expr (or NULL if not valid
if-then-else)
