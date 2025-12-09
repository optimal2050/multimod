# Create an AST node for dimensions (dims) of a symbol

Constructs an `dims` object, which represents the declared dimensions of
a variable, parameter, or equation in a structured symbolic model. Each
dimension is typically represented as a set reference (`set`) or as a
symbolic identifier (`symbol`). This is similar to `index`, but
specifically for dimensions.

## Usage

``` r
ast_dims(...)
```

## Arguments

- ...:

  One or more dimension expressions, typically created using
  [`ast_set()`](https://optimal2050.github.io/multimod/reference/new_ast.md)
  or
  [`ast_symbol()`](https://optimal2050.github.io/multimod/reference/ast_symbol.md).
  Expected AST objects or character strings, comma-separated. If
  character strings are provided, they will be converted to `symbol`
  objects.

## Value

A list of class `dims` and `ast`, representing the dimension nodes.

## Examples

``` r
ast_dims(list(ast_set("tech"), ast_set("region")))
#> <AST dims> 
#> [tech,region]
ast_dims(ast_symbol("t"))
#> <AST dims> 
#> [t]
```
