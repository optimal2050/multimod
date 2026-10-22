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

- brackets:

  Optional bracket type to wrap the expression in when rendered: `"("`,
  `"["` or the curly brace; `NULL` for none.

## Value

An `expression` S3 object (subclass of `ast`).
