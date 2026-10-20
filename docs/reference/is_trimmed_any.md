# Check if any elements are actually trimmed

Walks through model structure to check if any sets, parameters,
mappings, variables, or equations have `$trimmed = TRUE`. More robust
than checking only the `model$misc$trimmed` flag.

## Usage

``` r
is_trimmed_any(model, include_equations = TRUE)
```

## Arguments

- model:

  A multimod model object

- include_equations:

  Logical. If TRUE (default), include equations in check. If FALSE, only
  check data elements (sets, parameters, mappings, variables).

## Value

Logical indicating if any elements are trimmed

## Examples

``` r
m <- example_models$energyRt$multimod
is_trimmed_any(m)
#> [1] FALSE

m_trimmed <- trim_model(m, verbose = FALSE)
is_trimmed_any(m_trimmed)
#> [1] TRUE
# Only data elements (excluding equations)
is_trimmed_any(m_trimmed, include_equations = FALSE)
#> [1] TRUE
```
