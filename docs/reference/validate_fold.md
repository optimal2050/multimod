# Validate fold integrity

Checks that folded data is consistent with original data and that no
information was lost during folding.

## Usage

``` r
validate_fold(model, verbose = TRUE)
```

## Arguments

- model:

  A multimod model object

- verbose:

  Logical; print validation results (default: TRUE)

## Value

List of validation results, or NULL if all valid
