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
if (FALSE) { # \dontrun{
# Load model with all data
model <- load_model("models/utopia")

# Load structure only (for inspection)
model <- load_model("models/utopia", load_data = FALSE)
} # }
```
