# Check if a string is a compound expression

Check if a string is a compound expression

## Usage

``` r
is_gams_expression(
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
is_gams_expression("x + y") # TRUE
#> [1] TRUE
is_gams_expression("x") # FALSE
#> [1] FALSE
```
