# Convert AST to R Function

Converts an AST expression to an executable R function. The function
arguments are automatically detected from symbols in the AST, or can be
explicitly specified.

## Usage

``` r
as_rfunction(x, args = NULL, envir = parent.frame(), ...)
```

## Arguments

- x:

  An AST node (expression, symbol, etc.)

- args:

  Character vector of explicit argument names (optional). If NULL,
  arguments are automatically extracted from the AST.

- envir:

  Environment in which to create the function (default: parent.frame())

- ...:

  Additional arguments (currently unused)

## Value

An R function that evaluates the AST expression

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple expression: DiscountRate[r]
ast <- ast_expression(
  op = "[",
  lhs = ast_symbol("DiscountRate"),
  rhs = ast_symbol("r")
)
fn <- as_rfunction(ast)
# Creates: function(DiscountRate, r) { DiscountRate[r] }

# Call it
result <- fn(DiscountRate = c(R1=0.05, R2=0.07), r = "R1")
# Returns: 0.05
} # }
```
