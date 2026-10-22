# Analyze if a dimension can be safely folded

Checks if a parameter's dimension is redundant (values constant across
it). Uses per-group validation: for each combination of the other
dimensions, checks whether the values are identical within that group.

## Usage

``` r
analyze_dimension_redundancy(
  param,
  dim_to_test,
  model,
  tolerance = 1e-10,
  validation_mapping = NULL
)
```

## Arguments

- param:

  Parameter object

- dim_to_test:

  Dimension name to analyze

- model:

  Model object (for set membership and alias resolution)

- tolerance:

  Numeric tolerance for value equality (default 1e-10)

- validation_mapping:

  Optional. Name of mapping to use for coverage validation (e.g.,
  "mTechTimeslice"). If NULL, auto-detects based on parameter
  dimensions.

## Value

List with can_fold (logical) and reason (character)

## Details

The test is all or nothing: if any group shows variation, the dimension
cannot be folded, because partial folding is not supported.
