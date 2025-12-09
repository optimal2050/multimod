# Export GLPK model to MPS or CPLEX LP format

Export GLPK model to MPS or CPLEX LP format

## Usage

``` r
export_glpk_model(
  model_dir,
  mod_file = NULL,
  dat_file = NULL,
  output_file,
  format = NULL,
  verbose = TRUE
)
```

## Arguments

- model_dir:

  Path to directory containing .mod and .dat files

- mod_file:

  Name of .mod file (default: searches for \*.mod)

- dat_file:

  Name of .dat file (default: searches for \*.dat)

- output_file:

  Path to output file (extension determines format: .mps or .lp)

- format:

  Format to export: "MPS" or "CPLEX_LP" (default: auto-detect from
  output_file)

- verbose:

  Logical, print progress messages (default: TRUE)

## Value

Invisible path to created file
