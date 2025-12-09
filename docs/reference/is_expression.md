# Check if a string is a compound expression

Check if a string is a compound expression

## Usage

``` r
is_expression(
  s,
  ops = c("+", "-", "*", "/", "^", "=", "==", "<", "<=", ">", ">=", "=e=", "=l=", "=g=",
    "=le=", "=ge=", "and", "or", "not")
)
```

## Arguments

- s:

  Character string (GAMS-like expression)

- ops:

  Character vector of known operators (defaults: arithmetic, logical,
  relational)

## Value

Logical: TRUE if expression, FALSE if atomic

## Examples

``` r
is_expression("x + y") # TRUE
#> Error in find_top_level_operators(s, ops = ops): Invalid/Unrecognized operator: =le=
is_expression("x") # FALSE
#> Error in find_top_level_operators(s, ops = ops): Invalid/Unrecognized operator: =le=
```
