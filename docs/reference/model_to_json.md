# Serialise a model to JSON

Writes the model's structure - sets, mappings, parameters, variables and
equations - in the language-neutral encoding described in
[`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md).

## Usage

``` r
model_to_json(model, path = NULL, data = c("omit", "inline"), pretty = TRUE)
```

## Arguments

- model:

  A multimod model.

- path:

  Optional file to write to. If `NULL`, the JSON is returned as a
  character string.

- data:

  `"omit"` (default) or `"inline"`; see Details.

- pretty:

  Logical; pretty-print the JSON.

## Value

The JSON string, invisibly if `path` was given.

## Details

Bulk `data` payloads are omitted by default: they belong in the Parquet
store that
[`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md)
writes, not in a JSON document. Pass `data = "inline"` for a
self-contained file, which is practical for small models.

## See also

[`model_from_json()`](https://optimal2050.github.io/multimod/reference/model_from_json.md),
[`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md),
[`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md)

## Examples

``` r
m <- example_models$energyRt$multimod
js <- model_to_json(m)
m2 <- model_from_json(js)
```
