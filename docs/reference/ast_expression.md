# Create an expression AST node

Constructs a binary operation node representing an expression such as
addition, multiplication, etc.

## Usage

``` r
ast_expression(op, lhs, rhs, brackets = NULL)
```

## Arguments

- op:

  A character string representing the operator (e.g., `+`, `*`, `/`).

- lhs:

  The left-hand side AST node.

- rhs:

  The right-hand side AST node.

## Value

An `expression` S3 object (subclass of `ast`).
