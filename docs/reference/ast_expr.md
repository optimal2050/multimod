# Create an expression AST node

Constructs a binary operation node representing an expression such as
addition, multiplication, etc.

## Usage

``` r
ast_expr(op, left, right)
```

## Arguments

- op:

  A character string representing the operator (e.g., `+`, `*`, `/`).

- left:

  The left-hand side AST node.

- right:

  The right-hand side AST node.

## Value

An `ast_expr` S3 object (subclass of `multimod_ast`).
