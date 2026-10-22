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
  [`create_fold_spec()`](https://optimal2050.github.io/multimod/r/reference/create_fold_spec.md)
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

The algorithm uses per-group validation: for each combination of the
other dimensions, it checks whether the values are identical. The test
is all or nothing: if any group shows variation, the dimension is not
folded.

Folding analyzes each parameter to identify dimensions that do not
provide variation in the data. For each parameter, it groups by all
other dimensions and checks whether the values are constant within each
group (within tolerance).

Example: For `pStorageInpEff[stg, comm, region, year, timeslice]`
testing timeslice:

- Groups by `(stg, comm, region, year)`

- For each group, checks whether all timeslice values are identical

- Folds only if every group has constant values

Sparse data is handled: a parameter with values only for peak hours (one
timeslice out of 17) is tested on the values that exist, which must be
constant within each group.

The fold creates three slots per parameter:

- `data`: Original complete data (preserved)

- `folded_data`: Collapsed data with reduced dimensions

- `active_dims`: Dimensions remaining after fold

Safeguards:

- Only the dimensions named in `fold_spec` are folded

- Dimensions that identify an entity (for an energyRt model: tech, comm,
  sup, dem, stg, trade) are never folded

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
