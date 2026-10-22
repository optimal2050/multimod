# Rebuild a model from JSON

Rebuild a model from JSON

## Usage

``` r
model_from_json(json)
```

## Arguments

- json:

  A JSON string, or a path to a file written by
  [`model_to_json()`](https://optimal2050.github.io/multimod/r/reference/model_to_json.md).

## Value

A multimod model.

## See also

[`model_to_json()`](https://optimal2050.github.io/multimod/r/reference/model_to_json.md)

## Examples

``` r
f <- tempfile(fileext = ".json")
model_to_json(example_models$energyRt$multimod, path = f)
m2 <- model_from_json(f)
```
