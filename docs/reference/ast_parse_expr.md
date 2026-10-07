# Language-aware AST expression parser

Language-aware AST expression parser

## Usage

``` r
ast_parse_expr(expr, symbols = NULL, language = NULL, context = NULL, ...)
```

## Arguments

- expr:

  A character string to parse, or an AST node (returned as is).

- symbols:

  Symbol table passed to the parser.

- language, context:

  See
  [`resolve_ast_language()`](https://optimal2050.github.io/multimod/reference/resolve_ast_language.md).

- ...:

  Passed on to the language parser.

## Value

An AST node.
