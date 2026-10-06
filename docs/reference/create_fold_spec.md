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
`can_fold`, `unproven` and `reason`. `unproven` marks a dimension
refused for want of a validation mapping rather than because folding was
shown to be wrong: constancy holds over the data present, but nothing
proves the data covers every applicable member, so folding could
broadcast a value where the default applies. Supplying the mapping
settles it either way.

## Details

Nothing here is energyRt-specific: the function walks `model$parameters`
and the `fold_dims` configuration it is given. Only the *default*
mapping names (`mTechTimeslice` and friends) follow energyRt's
conventions, and those are supplied by the caller.

With an entity-specific configuration, the **first** matching entity
dimension wins: `tech`, then `sup`, then `comm`, then `stg`; a parameter
matching none gets `NA` and is not validated.

A character vector is a **mapping chain**, joined in sequence on the
columns the mappings share. For
`stg = c("mStorageComm", "mCommTimeslice")`: `mStorageComm[stg, comm]`
joined with `mCommTimeslice[comm, timeslice]` yields `[stg, timeslice]`.

## See also

[`fold_model()`](https://optimal2050.github.io/multimod/reference/fold_model.md)

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
