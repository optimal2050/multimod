# Report the magnitude spectrum of an assembled LP

Report the magnitude spectrum of an assembled LP

## Usage

``` r
check_matrix_numbers(
  lp,
  small = 1e-04,
  large = 1e+06,
  top = 15,
  verbose = TRUE
)
```

## Arguments

- lp:

  An LP from
  [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md).

- small, large:

  Thresholds for the "too small" / "too large" counts.

- top:

  How many offender families to list.

- verbose:

  Logical; print the report.

## Value

Invisibly, a list with `summary` (one row per section), `decades` (a
magnitude histogram of the matrix coefficients), and `offenders_row` /
`offenders_col` (entries below `small`, by equation and by variable).

## Examples

``` r
lp <- model_to_lp(example_models$toy)
chk <- check_matrix_numbers(lp, verbose = FALSE)
chk$summary
#>     section entries zeros nonfinite min_abs max_abs orders below_small
#> 1   COLUMNS      29     0         0    0.25     120    2.7           0
#> 2 OBJECTIVE       1     0         0    1.00       1    0.0           0
#> 3       RHS      10     8         0  600.00    1000    0.2           0
#> 4    BOUNDS       0     0         0      NA      NA     NA           0
#>   above_large
#> 1           0
#> 2           0
#> 3           0
#> 4           0
```
