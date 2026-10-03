# Create fold specification for energyRt models (convenience wrapper)

Convenience wrapper around the generic
[`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md)
function. Uses the same underlying implementation but provides
energyRt-specific defaults for entity-to-mapping associations.

## Usage

``` r
create_fold_spec_energyRt(model, fold_dims, tolerance = 1e-10, verbose = TRUE)
```

## Arguments

- model:

  A multimod model object (from energyRt)

- fold_dims:

  Dimension configuration. Same format as
  [`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md).
  Can be:

  - Character vector: dimension names (e.g., `c("timeslice")`)

  - Named list (simple): dimension → mapping (e.g.,
    `list(timeslice = "mCommTimeslice")`)

  - Named list (advanced): dimension → entity-specific mappings (e.g.,
    `list(timeslice = list(tech = "mTechTimeslice", comm = "mCommTimeslice"))`)

- tolerance:

  Numeric tolerance for considering values equal (default: 1e-10)

- verbose:

  Logical; print progress messages (default: TRUE)

## Value

Data frame with columns: param_name, dim_to_fold, validation_mapping,
can_fold, reason

## Details

**Deprecated.** This is now a thin alias for
[`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md),
which does the work and is not energyRt-specific. Use that instead.

Until 2026-09-24 this name was defined *twice* in this file. The first
definition delegated to
[`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md),
which delegated straight back - infinite mutual recursion, avoided only
because the second definition silently overrode the first and carried
the real implementation. The implementation now lives under
[`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md)
and this name forwards to it.

Mapping priority (for entity-specific configuration):

- Uses FIRST matching entity dimension

- If parameter has "tech" → use tech mapping

- Else if has "sup" → use sup mapping

- Else if has "comm" → use comm mapping

- Else if has "stg" → use stg mapping

- Else → NA (no validation)

Mapping chains join mappings by common columns: Example:
`stg = c("mStorageComm", "mCommTimeslice")`

- `mStorageComm[stg, comm]` ⨝ `mCommTimeslice[comm, timeslice]` →
  `[stg, timeslice]`

For derived mappings (chains), mappings are joined in sequence by common
columns. Example: `stg = c("mStorageComm", "mCommTimeslice")`

- `mStorageComm[stg, comm]` inner_join `mCommTimeslice[comm, timeslice]`
  → `[stg, timeslice]`

## Examples

``` r
if (FALSE) { # \dontrun{
# Explicit configuration for energyRt models
fold_spec <- create_fold_spec_energyRt(model, 
  fold_dims = list(
    timeslice = list(
      tech = "mTechTimeslice",
      sup = "mSupTimeslice", 
      comm = "mCommTimeslice",
      stg = c("mStorageComm", "mCommTimeslice"),
      trade = c("mTradeComm", "mCommTimeslice"),
      imp = c("mImportComm", "mCommTimeslice")
    )
  ))

# Review and edit
View(fold_spec)

# Use edited specification
model <- fold_model(model, fold_spec = fold_spec)
} # }
```
