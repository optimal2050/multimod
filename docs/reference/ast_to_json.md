# Convert an AST node to a language-neutral list

The list is plain R data - no classes - ready for
[`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
or any other serialiser. Node types are tagged with their schema name,
so declarations and references stay distinguishable.

## Usage

``` r
ast_to_json(x, data = c("omit", "inline"))
```

## Arguments

- x:

  An AST node.

- data:

  How to treat `data` payloads: `"omit"` (the default) leaves them out,
  on the basis that bulk data belongs in the Parquet store; `"inline"`
  embeds them.

## Value

A nested list.

## See also

[`ast_from_json()`](https://optimal2050.github.io/multimod/reference/ast_from_json.md),
[`model_to_json()`](https://optimal2050.github.io/multimod/reference/model_to_json.md),
[`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md)

## Examples

``` r
n <- ast_expression("*", ast_symbol("a"), ast_symbol("b"))
str(ast_to_json(n), max.level = 2)
#> List of 5
#>  $ node    : chr "expression"
#>  $ op      : chr "*"
#>  $ lhs     :List of 2
#>   ..$ node: chr "symbol"
#>   ..$ name: chr "a"
#>  $ rhs     :List of 2
#>   ..$ node: chr "symbol"
#>   ..$ name: chr "b"
#>  $ brackets: NULL
```
