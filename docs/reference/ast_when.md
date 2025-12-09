# Construct a conditional expression node for multimod AST

This function constructs a conditional expression node of type `"when"`,
representing GAMS-style conditional terms using the `$` operator,
`if ... else ...` statements in Julia/JuMP, `if ... in ...` in
Python/Pyomo, etc.

## Usage

``` r
ast_when(condition, then, otherwise = NULL, ...)
```

## Arguments

- condition:

  The condition to check, must be an AST node.

- then:

  The expression to evaluate if the condition is true (usually the
  left-hand side).

- otherwise:

  Optional expression to evaluate if the condition is false. This part
  is not available in GAMS `$` statements, but is useful for other
  languages.

## Value

An object of class `ast` and subclass `when`.

## Examples

``` r
ast_when(
  condition = ast_symbol("i_active(i)"),
  then = ast_variable("x", c("i"))
)
#> <AST when> 
#>   condition:
#> <AST symbol> i_active(i)
#>   then :
#> <AST variable> x
#>   dims:  i 
#>   otherwise :
#> NULL
```
