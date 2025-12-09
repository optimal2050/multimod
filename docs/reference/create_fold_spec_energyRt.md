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

  - Character vector: dimension names (e.g., `c("slice")`)

  - Named list (simple): dimension → mapping (e.g.,
    `list(slice = "mCommSlice")`)

  - Named list (advanced): dimension → entity-specific mappings (e.g.,
    `list(slice = list(tech = "mTechSlice", comm = "mCommSlice"))`)

- tolerance:

  Numeric tolerance for considering values equal (default: 1e-10)

- verbose:

  Logical; print progress messages (default: TRUE)

## Value

Data frame with columns: param_name, dim_to_fold, validation_mapping,
can_fold, reason

## Details

**Note**: This function has the SAME functionality as
[`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md).
The only difference is naming - this version emphasizes it's designed
for energyRt models, but both functions support all features
(entity-specific mappings, chains, etc.).

Mapping priority (for entity-specific configuration):

- Uses FIRST matching entity dimension

- If parameter has "tech" → use tech mapping

- Else if has "sup" → use sup mapping

- Else if has "comm" → use comm mapping

- Else if has "stg" → use stg mapping

- Else → NA (no validation)

Mapping chains join mappings by common columns: Example:
`stg = c("mStorageComm", "mCommSlice")`

- `mStorageComm[stg, comm]` ⨝ `mCommSlice[comm, slice]` → `[stg, slice]`

For derived mappings (chains), mappings are joined in sequence by common
columns. Example: `stg = c("mStorageComm", "mCommSlice")`

- `mStorageComm[stg, comm]` inner_join `mCommSlice[comm, slice]` →
  `[stg, slice]`

## Examples

``` r
if (FALSE) { # \dontrun{
# Explicit configuration for energyRt models
fold_spec <- create_fold_spec_energyRt(model, 
  fold_dims = list(
    slice = list(
      tech = "mTechSlice",
      sup = "mSupSlice", 
      comm = "mCommSlice",
      stg = c("mStorageComm", "mCommSlice"),
      trade = c("mTradeComm", "mCommSlice"),
      imp = c("mImportComm", "mCommSlice")
    )
  ))

# Review and edit
View(fold_spec)

# Use edited specification
model <- fold_model(model, fold_spec = fold_spec)
} # }
```
