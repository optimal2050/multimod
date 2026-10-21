# Unfold model to restore original dimensions

Removes all fold information and restores parameters to their original
full-dimensional state.

## Usage

``` r
unfold_model(model, verbose = TRUE)
```

## Arguments

- model:

  A multimod model object

- verbose:

  Logical; print progress messages (default: TRUE)

## Value

Modified model with fold removed

## Examples

``` r
m <- example_models$energyRt$multimod
spec <- create_fold_spec(m, fold_dims = "timeslice", verbose = FALSE)
folded <- fold_model(m, fold_spec = spec, verbose = FALSE)
is_folded(folded)
#> [1] TRUE
restored <- unfold_model(folded)
#> ✓ Unfolded pTimesliceWeight 
#> 
#> Unfolded 1 parameters
is_folded(restored)
#> [1] FALSE
```
