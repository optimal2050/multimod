# Fold equations to match folded parameters

Scans all equation ASTs and reduces dimensions of folded parameters.
Creates model\$folded_equations with updated parameter references.
Parameter declarations will use active_dims (4D instead of 5D).

## Usage

``` r
fold_equations(model, verbose = TRUE)
```

## Arguments

- model:

  Model object with folded parameters

- verbose:

  Logical; print progress

## Value

Model with folded_equations slot
