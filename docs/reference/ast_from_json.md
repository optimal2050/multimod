# Rebuild an AST node from a language-neutral list

Rebuild an AST node from a language-neutral list

## Usage

``` r
ast_from_json(j)
```

## Arguments

- j:

  A list as produced by
  [`ast_to_json()`](https://optimal2050.github.io/multimod/reference/ast_to_json.md),
  or the result of
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  with `simplifyVector = FALSE`.

## Value

An AST node.

## See also

[`ast_to_json()`](https://optimal2050.github.io/multimod/reference/ast_to_json.md)

## Examples

``` r
n <- ast_expression("*", ast_symbol("a"), ast_symbol("b"))
identical(ast_from_json(ast_to_json(n)), n)
#> [1] TRUE
```
