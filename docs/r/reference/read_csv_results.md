# Read CSV results from GMPL solver solution directory and save to model variables

Reads variable solution values from CSV files in the solver solution
directory, copies them to the model's variables/ directory, and attaches
them to the model's variable objects in the same format as model data.

## Usage

``` r
read_csv_results(model, model_dir, verbose = TRUE)
```

## Arguments

- model:

  A multimod model object

- model_dir:

  Path to model root directory

- verbose:

  Logical; print progress messages (default: TRUE)

## Value

Updated model object with solution data attached to variables
