# Fold model dimensions to reduce data size

Reduces redundant dimensions in parameters where data is constant across
those dimensions. Only folds high-cardinality indexing dimensions
(slice, optionally region/year) while preserving structural dimensions
(tech, comm, etc.).

## Usage

``` r
should_enable_progress_fold(
  model,
  fold_spec,
  param_threshold = 30,
  row_threshold = 50000
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

Example: For `pStorageInpEff[stg, comm, region, year, slice]` testing
slice:

- Groups by `(stg, comm, region, year)`

- For each group, checks if ALL slice values are identical

- Only folds if EVERY group has constant values

This handles sparse data correctly - if a parameter only has values for
peak hours (1 slice out of 17), that's fine. It only checks if the
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
if (FALSE) { # \dontrun{
# Specify fold dimensions with entity-specific mappings
fold_dims <- list(
  slice = c("mSupSlice", "mDemSlice", "mStorageSlice", "mTradeSlice"),
  region = c("mTechRegion", "mSupRegion", "mDemRegion", "mStorageRegion"),
  year = c("mTechYear", "mSupYear", "mDemYear", "mStorageYear")
)
fold_spec <- create_fold_spec(model, fold_dims = fold_dims)
# Review and edit fold_spec to ensure correct validation mappings
model <- fold_model(model, fold_spec = fold_spec)
} # }
```
