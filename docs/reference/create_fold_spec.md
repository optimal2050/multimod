# Create fold specification for parameters (generic interface)

Generic interface for creating fold specifications. Supports all the
same features as `create_fold_spec_energyRt` but without
energyRt-specific defaults.

## Usage

``` r
create_fold_spec(model, fold_dims, tolerance = 1e-10, verbose = TRUE)
```

## Arguments

- model:

  A multimod model object

- fold_dims:

  Dimension configuration. Can be:

  - Character vector: dimension names (e.g., `c("slice")`)

  - Named list (simple): dimension → mapping (e.g.,
    `list(slice = "mCommSlice")`)

  - Named list (advanced): dimension → entity-specific mappings (e.g.,
    `list(slice = list(tech = "mTechSlice", comm = "mCommSlice"))`)

- tolerance:

  Numeric tolerance for detecting constant values (default: 1e-10)

- verbose:

  Logical; print progress messages (default: TRUE)

## Value

Data frame with fold specification for each parameter

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple: fold slice dimension
fold_spec <- create_fold_spec(model, fold_dims = "slice")

# With single validation mapping
fold_spec <- create_fold_spec(model, fold_dims = list(slice = "mCommSlice"))

# Entity-specific (advanced)
fold_spec <- create_fold_spec(
  model,
  fold_dims = list(
    slice = list(
      tech = "mTechSlice",
      comm = "mCommSlice",
      stg = c("mStorageComm", "mCommSlice")  # Chain
    )
  )
)
} # }
```
