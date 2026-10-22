# Populate sets from model's own mappings and parameters

Populate sets from model's own mappings and parameters

## Usage

``` r
populate_sets_from_data(model, load_data = FALSE)
```

## Arguments

- model:

  Multimod model

- load_data:

  Logical. Load data from disk if needed?

## Value

Modified model with set data populated

## Details

Extracts unique members for each set from the model's own mappings and
parameters. Useful when model already has data loaded.
