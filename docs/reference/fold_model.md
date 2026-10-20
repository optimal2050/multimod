# Fold model dimensions to reduce data size

Reduces redundant dimensions in parameters where data is constant across
those dimensions. Only folds high-cardinality indexing dimensions
(timeslice, optionally region/year) while preserving structural
dimensions (tech, comm, etc.).

## Usage

``` r
fold_model(
  model,
  fold_spec,
  tolerance = 1e-10,
  verbose = TRUE,
  .progress = "auto"
)
```

## Arguments

- model:

  A multimod model object

- fold_spec:

  Data frame specifying fold configuration. Must have columns:
  param_name, dim_to_fold, validation_mapping. Use
  [`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md)
  to generate proper fold specification with entity-specific mappings.

- tolerance:

  Numeric tolerance for considering values equal (default: 1e-10)

- verbose:

  Logical; print progress messages (default: TRUE)

- .progress:

  Progress bar control: `"auto"` (default) shows a `progressr` bar for
  large models, `TRUE` forces it on (requires `progressr`), `FALSE`
  disables it.

## Value

Modified model with folded parameters

## Details

The algorithm uses per-group validation: for each combination of OTHER
dimensions, it checks if values are identical. This is an "all or
nothing" approach - if ANY group shows variation, the dimension is not
folded.

Folding analyzes each parameter to identify dimensions that don't
provide variation in the data. For each parameter, it groups by all
OTHER dimensions and checks if values are constant within each group
(within tolerance).

Example: For `pStorageInpEff[stg, comm, region, year, timeslice]`
testing timeslice:

- Groups by `(stg, comm, region, year)`

- For each group, checks if ALL timeslice values are identical

- Only folds if EVERY group has constant values

This handles sparse data correctly - if a parameter only has values for
peak hours (1 timeslice out of 17), that's fine. It only checks if the
values that DO exist are constant within each group.

The fold creates three slots per parameter:

- `data`: Original complete data (preserved)

- `folded_data`: Collapsed data with reduced dimensions

- `active_dims`: Dimensions remaining after fold

Safety measures:

- Only folds dimensions explicitly requested by user

- Never folds structural dimensions (tech, comm, sup, dem, stg, trade,
  etc.)

- Skips parameters where all dimensions would be folded (would become
  scalar)

- Validates consistency before folding

- Updates equation ASTs to match reduced dimensions

## Examples

``` r
m <- example_models$energyRt$multimod
fold_spec <- create_fold_spec(m, fold_dims = "timeslice", verbose = FALSE)
# Review and edit fold_spec to ensure correct validation mappings
m_folded <- fold_model(m, fold_spec = fold_spec, verbose = FALSE)
get_fold_summary(m_folded, format = "text")
```
