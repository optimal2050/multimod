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
m <- example_models$energyRt$multimod
is_folded(m)
#> [1] FALSE
is_folded(m$parameters$pTechCinp2use)
#> [1] FALSE

spec <- create_fold_spec(m, fold_dims = "timeslice", verbose = FALSE)
m_folded <- fold_model(m, fold_spec = spec, verbose = FALSE)
is_folded(m_folded)
#> [1] TRUE
```
