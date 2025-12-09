# Create an equation AST node

Constructs an equation node representing a mathematical equation. This
node includes a left-hand side (LHS), right-hand side (RHS), relation
operator (e.g., equality or inequality), and an optional domain (e.g.,
mapping, expression or logical condition).

## Usage

``` r
ast_equation(
  lhs,
  rhs,
  relation = "==",
  name = NULL,
  domain = NULL,
  desc = NULL
)
```

## Arguments

- lhs:

  The left-hand side of the equation (an AST node).

- rhs:

  The right-hand side of the equation (an AST node).

- relation:

  A character string representing the relation type. One of `"=="`,
  `"<="`, or `">="`. @param name Optional character string. The name of
  the equation. @param domain Optional AST node representing the domain
  condition. @param desc Optional character string. A description or
  label for the equation.

  @return An `equation` S3 object. @export
