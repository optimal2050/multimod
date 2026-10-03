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
  [`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md).

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
