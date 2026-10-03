# Create a fold specification for a model's parameters

Scans a model's parameters and reports, for each, whether a dimension
can be folded away - that is, whether the parameter's values are
constant along it within tolerance - together with the mapping that
should be used to validate the fold.

## Usage

``` r
create_fold_spec(model, fold_dims, tolerance = 1e-10, verbose = TRUE)
```

## Arguments

- model:

  A multimod model object.

- fold_dims:

  Dimension configuration. Either:

  - a character vector of dimension names (e.g. `c("timeslice")`);

  - a named list mapping dimension to validation mapping (e.g.
    `list(timeslice = "mCommTimeslice")`);

  - a named list mapping dimension to entity-specific mappings (e.g.
    `list(timeslice = list(tech = "mTechTimeslice", stg = c("mStorageComm", "mCommTimeslice")))`),
    where a character vector is a chain.

- tolerance:

  Numeric tolerance for detecting constant values.

- verbose:

  Logical; print progress.

## Value

A data frame with one row per parameter/dimension pair: `param_name`,
`dim_to_fold`, `validation_mapping`, `validation_mapping_raw`,
`can_fold` and `reason`.

## Details

Nothing here is energyRt-specific: the function walks `model$parameters`
and the `fold_dims` configuration it is given. Only the *default*
mapping names (`mTechTimeslice` and friends) follow energyRt's
conventions, and those are supplied by the caller.

## See also

`fold_model()`,
[`create_fold_spec_energyRt()`](https://optimal2050.github.io/multimod/reference/create_fold_spec_energyRt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fold_spec <- create_fold_spec(model, fold_dims = "timeslice")

fold_spec <- create_fold_spec(
  model,
  fold_dims = list(
    timeslice = list(
      tech = "mTechTimeslice",
      comm = "mCommTimeslice",
      stg  = c("mStorageComm", "mCommTimeslice")   # a chain
    )
  )
)
} # }
```
