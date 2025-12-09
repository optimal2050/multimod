# Solve GLPK model using glpkAPI

Solve GLPK model using glpkAPI

## Usage

``` r
execute_glpkAPI(
  model_dir,
  mod_file = NULL,
  dat_file = NULL,
  output_dir = NULL,
  verbose = TRUE,
  save_solution = FALSE,
  export_vars = FALSE,
  export_mps = FALSE
)
```

## Arguments

- model_dir:

  Path to directory containing .mod and .dat files

- mod_file:

  Name of .mod file (default: searches for \*.mod)

- dat_file:

  Name of .dat file (default: searches for \*.dat)

- output_dir:

  Directory for solution output (default: model_dir/output)

- verbose:

  Logical, print solver output (default: TRUE)

- save_solution:

  Logical, save solution to CSV files (default: FALSE)

- export_mps:

  Logical, export model to MPS format (default: FALSE)

## Value

List with solution data and status
