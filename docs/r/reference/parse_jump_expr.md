# Parse a JuMP expression into multimod AST

Parse a JuMP expression into multimod AST

## Usage

``` r
parse_jump_expr(
  expr,
  symbols,
  aliases = character(),
  model = NULL,
  depth = 0L,
  max_depth = 100L
)
```

## Arguments

- expr:

  Character string holding one JuMP (Julia) expression.

- symbols:

  Symbol table from
  [`build_symbols_list()`](https://optimal2050.github.io/multimod/r/reference/build_symbols_list.md).

- aliases:

  Named character vector mapping the short index names the expression
  uses onto the names the model declares.

- model:

  The model, used to resolve a mapping's declared index names.

- depth, max_depth:

  Recursion guard.

## Value

An AST node.
