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
if (FALSE) { # \dontrun{
# Untrim everything
model <- untrim_model(model)

# Untrim only variables and equations
model <- untrim_model(model, elements = c("variables", "equations"))
} # }
```
