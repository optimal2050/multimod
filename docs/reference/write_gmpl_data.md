# Write GMPL data file from multimod model

Creates a .dat file with set members, mapping tuples, and parameter
values. Supports lazy loading - data is loaded from disk only when
needed. If model has been folded, uses folded_data by default for
reduced file size.

## Usage

``` r
write_gmpl_data(model, file = NULL, use_folded = TRUE, INF = 1e+20)
```

## Arguments

- model:

  A multimod model object

- file:

  Output file path (optional)

- use_folded:

  Logical; use folded_data if available (default: TRUE)

- INF:

  numeric; value to use for Inf in parameter defaults (default: 1e20)

## Value

Character vector or writes file if `file` is given
