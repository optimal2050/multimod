# Construct a conditional (dollar) expression node for multimod AST

This function constructs a conditional expression node of type `"cond"`,
representing GAMS-style conditional terms using the `$` operator.

## Usage

``` r
ast_cond(then, condition)
```

## Arguments

- then:

  The expression to evaluate if the condition is true (usually the
  left-hand side).

- condition:

  The condition to check (right-hand side of `$`). Must be an AST node.

## Value

An object of class `multimod_ast` and subclass `ast_cond`

## Examples

``` r
ast_cond(
  then = ast_var("x", c("i")),
  condition = ast_symbol("i_active(i)")
)
#> <multimod_ast> 
#> [1] "if (i_active(i)) {x[i]}"
```
