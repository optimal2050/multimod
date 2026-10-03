# Schema node type of an AST node

Maps an S3 AST node onto its schema node type, resolving the
declaration/reference ambiguity through the `"multimod"` class marker: a
node carrying it is a declaration, one without it is a reference.

## Usage

``` r
schema_type(x)
```

## Arguments

- x:

  An AST node.

## Value

A single string naming a node type in
[`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md),
or `NA_character_` if `x` is not an AST node or its class is unknown.

## See also

[`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md)

## Examples

``` r
schema_type(ast_symbol("t"))
#> [1] "symbol"
schema_type(ast_set("tech"))   # a reference
#> [1] "set_ref"
schema_type(new_set("tech"))   # a declaration
#> [1] "set_decl"
```
