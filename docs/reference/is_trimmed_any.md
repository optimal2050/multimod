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
if (FALSE) { # \dontrun{
# Check if model has any trimmed elements
if (is_trimmed_any(model)) {
  cat("Model has trimmed elements\n")
}
# Check only data elements (excluding equations)
if (is_trimmed_any(model, include_equations = FALSE)) {
  cat("Model has trimmed data elements\n")
}
} # }
```
