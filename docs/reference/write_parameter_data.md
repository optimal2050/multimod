# Write parameter data (values over dimensions)

Write parameter data (values over dimensions)

## Usage

``` r
write_parameter_data(
  param_obj,
  model = NULL,
  use_folded = TRUE,
  validate_timestamp = TRUE
)
```

## Arguments

- param_obj:

  Parameter object

- model:

  Model object (for lazy loading)

- use_folded:

  Logical; use folded_data if available (default: TRUE)

- validate_timestamp:

  Logical; validate fold timestamp (default: TRUE)

## Value

Character vector of GMPL data statements
