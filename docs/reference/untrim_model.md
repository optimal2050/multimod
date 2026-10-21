# Untrim model or specific elements

Removes `$trimmed` flags from model or specific elements.

## Usage

``` r
untrim_model(model, elements = "all")
```

## Arguments

- model:

  A multimod model object

- elements:

  Character vector of element types to untrim: "all" (default), "sets",
  "parameters", "mappings", "variables", "equations"

## Value

Model with trimmed flags removed

## Examples

``` r
# \donttest{
trimmed <- trim_model(example_models$energyRt$multimod, verbose = FALSE)
is_trimmed(trimmed)
#> [1] TRUE

# Untrim only variables and equations
partly <- untrim_model(trimmed, elements = c("variables", "equations"))

# Untrim everything
restored <- untrim_model(trimmed)
is_trimmed(restored)
#> [1] FALSE
# }
```
