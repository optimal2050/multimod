# Create a multimod equation object

Constructs an equation object used in the `multimod` modeling framework.
This object represents a single equation, including left-hand side
(LHS), right-hand side (RHS), relation operator (e.g., equality or
inequality), and an optional domain.

## Usage

``` r
new_multimod_equation(
  name,
  desc = NULL,
  dims,
  lhs,
  rhs,
  relation = "==",
  domain = NULL
)

new_equation(name, desc = NULL, dims, lhs, rhs, relation = "==", domain = NULL)
```

## Arguments

- name:

  Character string. The name of the equation.

- desc:

  Optional character string. A description or label for the equation.

- dims:

  Character vector. Names of the dimensions over which the equation is
  declared.

- lhs:

  An AST (abstract syntax tree) representing the left-hand side of the
  equation.

- rhs:

  An AST representing the right-hand side of the equation.

- relation:

  Character string. The relation type: one of `"=="`, `"<="`, or `">="`.

- domain:

  Optional AST or symbol representing the domain/mapping condition for
  the equation.

## Value

An object of class `multimod_equation`, containing the parsed equation
structure.

## Examples

``` r
lhs <- ast_variable("vTechOut", dims = c("tech", "region"))
rhs <- ast_expression("*", ast_param("pTechEff", dims = c("tech")), ast_variable("vTechInp", dims = c("tech", "region")))
#> Error in ast_param("pTechEff", dims = c("tech")): could not find function "ast_param"
eq <- new_multimod_equation(
  name = "eqTechEff",
  desc = "Technology output efficiency",
  dims = c("tech", "region"),
  lhs = lhs,
  rhs = rhs,
  relation = "=="
)
#> Error: object 'rhs' not found
print(eq)
#> Error: object 'eq' not found
```
