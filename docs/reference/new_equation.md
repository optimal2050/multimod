# Create a multimod equation object

Constructs an equation object used in the `multimod` modeling framework.
This object represents a single equation, including left-hand side
(LHS), right-hand side (RHS), relation operator (e.g., equality or
inequality), and an optional domain.

## Usage

``` r
new_equation(
  name,
  desc = NULL,
  dims,
  lhs,
  rhs,
  relation = "==",
  domain = NULL,
  comment = NULL,
  dims_index_aliases = NULL,
  symbols = NULL
)
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

- comment:

  character, comment line from source file (e.g., \*@ domain hint)

- dims_index_aliases:

  Optional character vector of equation-specific iterator names, one per
  declared dimension (preserved from the source model, e.g. by
  [`read_gmpl()`](https://optimal2050.github.io/multimod/reference/read_gmpl.md)).

- symbols:

  Optional named list of known symbols (sets, aliases) used to resolve
  dimension names in
  [`ast_dims()`](https://optimal2050.github.io/multimod/reference/ast_dims.md).

## Value

An object of class `equation`, containing the parsed equation structure.

## Examples

``` r
lhs <- ast_variable("vTechOut", dims = c("tech", "region"))
rhs <- ast_expression("*",
  ast_parameter("pTechEff", dims = c("tech")),
  ast_variable("vTechInp", dims = c("tech", "region"))
)
eq <- new_equation(
  name = "eqTechEff",
  desc = "Technology output efficiency",
  dims = c("tech", "region"),
  lhs = lhs,
  rhs = rhs,
  relation = "=="
)
print(eq)
#> <AST equation> eqTechEff
#>   relation:  == 
#>   lhs:  vTechOuttech vTechOutregion 
#>   rhs:  pTechEfftech * vTechInptech 
```
