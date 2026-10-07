# Emit a JSON Schema for the multimod AST

Generated from
[`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md),
so it cannot drift from what R actually writes. Intended for non-R
readers of the serialised model - a C++ core, a Python binding, or an
editor with schema-aware completion.

## Usage

``` r
ast_json_schema(path = NULL)
```

## Arguments

- path:

  Optional file to write to.

## Value

The JSON Schema as a character string.

## See also

[`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md),
[`model_to_json()`](https://optimal2050.github.io/multimod/reference/model_to_json.md)

## Examples

``` r
schema <- ast_json_schema()
cat(substr(schema, 1, 300))
#> {
#>   "$schema": "https://json-schema.org/draft/2020-12/schema",
#>   "$id": "https://multimod.dev/schema/ast-v1.0.json",
#>   "title": "multimod model",
#>   "description": "Abstract syntax tree of a multimod optimization model. Generated from ast_schema(); do not edit by hand.",
#>   "type": "object",
#>   "proper
```
