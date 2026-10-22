# Load multimod model from disk

Load multimod model from disk

## Usage

``` r
load_model(path, load_data = TRUE, load_structure = TRUE, verbose = TRUE)
```

## Arguments

- path:

  Character. Path to model directory

- load_data:

  Logical. Load all data into memory (default: TRUE)

- load_structure:

  Logical. Load equations and variables (default: TRUE)

- verbose:

  Logical. Print progress messages

## Value

A multimod model object

## Examples

``` r
ws <- file.path(tempdir(), "energyRt_ws")
save_model(example_models$energyRt$multimod, ws, verbose = FALSE)

# Load model with all data
m <- load_model(ws, verbose = FALSE)

# Load structure only (for inspection)
m_light <- load_model(ws, load_data = FALSE, verbose = FALSE)
```
