# Analyze fold opportunities without applying

Reports which parameters could be folded and the potential compression.

## Usage

``` r
analyze_fold_opportunities(
  model,
  fold_timeslice = TRUE,
  fold_region = FALSE,
  fold_year = FALSE,
  tolerance = 1e-10
)
```

## Arguments

- model:

  A multimod model object

- fold_timeslice:

  Logical; consider the timeslice dimension

- fold_region:

  Logical; consider region dimension

- fold_year:

  Logical; consider year dimension

- tolerance:

  Numeric tolerance for value equality (default: 1e-10)

## Value

Data frame with fold analysis

## Examples

``` r
ops <- analyze_fold_opportunities(example_models$energyRt$multimod)
head(ops)
#>                         parameter    original_dims folded_dims removed_dims
#> pTimesliceWeight pTimesliceWeight year × timeslice        year    timeslice
#>                  original_rows folded_rows compression
#> pTimesliceWeight            68           4          17
```
