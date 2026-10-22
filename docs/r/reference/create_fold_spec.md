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

[`fold_model()`](https://optimal2050.github.io/multimod/r/reference/fold_model.md)

## Examples

``` r
m <- example_models$energyRt$multimod
fold_spec <- create_fold_spec(m, fold_dims = "timeslice")
#> 
#> === Fold Specification Generated ===
#> Total entries: 9 
#> Can fold: 1 
#> Cannot fold: 8 
#>   of which unproven -- a validation mapping would allow them: 6 
#> 
#> REVIEW CAREFULLY: validation_mapping may be incorrect for non-standard models
#> 
head(fold_spec)
#>         param_name dim_to_fold validation_mapping validation_mapping_raw
#> 1    pTechCinp2use   timeslice               <NA>                     NA
#> 2        pTechAfLo   timeslice               <NA>                     NA
#> 3         pSupCost   timeslice               <NA>                     NA
#> 4 pTimesliceWeight   timeslice               <NA>                     NA
#> 5    pTimesliceAgg   timeslice               <NA>                     NA
#> 6   pStorageInpEff   timeslice               <NA>                     NA
#>   can_fold unproven
#> 1    FALSE     TRUE
#> 2    FALSE     TRUE
#> 3    FALSE     TRUE
#> 4     TRUE    FALSE
#> 5    FALSE    FALSE
#> 6    FALSE     TRUE
#>                                                                                                                                                                                                                   reason
#> 1       Values are constant, but the parameter never indexes 5 of 17 timeslice members (ANNUAL, FAL, SPR), which resolve to the default. Supply a validation mapping for this dimension to confirm they are unreachable.
#> 2       Values are constant, but the parameter never indexes 5 of 17 timeslice members (ANNUAL, FAL, SPR), which resolve to the default. Supply a validation mapping for this dimension to confirm they are unreachable.
#> 3 Values are constant, but the parameter never indexes 16 of 17 timeslice members (FAL, FAL_DAY, FAL_NGT), which resolve to the default. Supply a validation mapping for this dimension to confirm they are unreachable.
#> 4                                                                                                                                                    Values constant across dimension in all 4 groups (tolerance: 1e-10)
#> 5                                                                                                              Dimension appears 2 times in the parameter (year, timeslice, timeslice); folding would remove all of them
#> 6       Values are constant, but the parameter never indexes 5 of 17 timeslice members (ANNUAL, FAL, SPR), which resolve to the default. Supply a validation mapping for this dimension to confirm they are unreachable.

# Entity-specific validation mappings (a character vector is a chain)
fold_spec <- create_fold_spec(
  m,
  fold_dims = list(
    timeslice = list(comm = "mCommTimeslice")
  ),
  verbose = FALSE
)
```
