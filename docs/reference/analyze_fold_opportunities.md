# Analyze fold opportunities without applying

Reports which parameters could be folded and the potential compression.

## Usage

``` r
analyze_fold_opportunities(
  model,
  fold_slice = TRUE,
  fold_region = FALSE,
  fold_year = FALSE,
  tolerance = 1e-10
)
```

## Arguments

- model:

  A multimod model object

- fold_slice:

  Logical; consider slice dimension

- fold_region:

  Logical; consider region dimension

- fold_year:

  Logical; consider year dimension

- tolerance:

  Numeric tolerance for value equality (default: 1e-10)

## Value

Data frame with fold analysis
