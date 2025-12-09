# Construct a conditional (dollar) expression node for multimod AST

This function constructs a conditional expression node of type
`"condition"`, representing GAMS-style conditional terms using the `$`
operator.

## Usage

``` r
ast_condition(condition, then)
```

## Arguments

- condition:

  The condition to check (right-hand side of `$`). Must be an AST node.

- then:

  The expression to evaluate if the condition is true (usually the
  left-hand side).

## Value

An object of class `multimod_ast` and subclass `ast_condition`

## Examples

``` r
ast_condition(
  condition = ast_symbol("i_active(i)"),
  then = ast_variable("x", c("i"))
)
#> <AST condition> 
#>   then :
#> <AST variable> x
#>   dims:  i 
#>   condition:
#> <AST symbol> 
```
