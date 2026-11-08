# Load multimod model from disk

Load multimod model from disk

## Usage

``` r
load_model(
  path,
  load_data = TRUE,
  load_structure = TRUE,
  canonical = FALSE,
  verbose = TRUE
)
```

## Arguments

- path:

  Character. Path to model directory

- load_data:

  Logical. Load all data into memory (default: TRUE)

- load_structure:

  Logical. Load equations and variables (default: TRUE)

- canonical:

  Logical. Put the set members in code-point order and the mapping rows
  in the order of their columns (C collation), so that the matrix built
  from the model does not depend on who wrote the directory.

- verbose:

  Logical. Print progress messages

## Value

A multimod model object

## Details

The structure is read from `model.rds`, or from `model.json` when there
is no `model.rds` (see `structure` in
[`save_model()`](https://optimal2050.github.io/multimod/r/reference/save_model.md)).
Set members missing from the structure are read from
`sets/<name>/data.*`.

## Examples

``` r
ws <- file.path(tempdir(), "energyRt_ws")
save_model(example_models$energyRt$multimod, ws, format = "csv", verbose = FALSE)

# Load model with all data
m <- load_model(ws, verbose = FALSE)

# Load structure only (for inspection)
m_light <- load_model(ws, load_data = FALSE, verbose = FALSE)
```
