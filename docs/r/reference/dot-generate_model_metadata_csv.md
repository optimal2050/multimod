# Write the sets and parameters metadata CSV files of a model workspace

Write the sets and parameters metadata CSV files of a model workspace

## Usage

``` r
.generate_model_metadata_csv(model, model_dir)
```

## Arguments

- model:

  A multimod model object.

- model_dir:

  Workspace directory; `sets/sets.csv` and `parameters/parameters.csv`
  are written under it.

## Value

Invisibly, `NULL`; called for its side effect of writing the files.
