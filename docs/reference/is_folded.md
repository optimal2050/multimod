# Check if a parameter or model is folded

Tests whether a parameter has been folded (reduced dimensions) or
whether a model contains any folded parameters.

## Usage

``` r
is_folded(x)
```

## Arguments

- x:

  A parameter object or multimod model object

## Value

Logical value indicating if the parameter/model is folded

## Details

For parameters: Returns TRUE if the parameter has active_dims set
(indicating it has been folded), FALSE otherwise.

For models: Returns TRUE if the model has any folded parameters, FALSE
otherwise. Also returns TRUE if the model has folded_equations.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check if a parameter is folded
is_folded(model$parameters$pTechCinp2use)

# Check if a model has any folded parameters
is_folded(model)
} # }
```
