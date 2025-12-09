# Get GLPK model statistics without solving

Get GLPK model statistics without solving

## Usage

``` r
get_glpk_model_stats(
  model_dir,
  mod_file = NULL,
  dat_file = NULL,
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

- verbose:

  Logical, print statistics (default: TRUE)

## Value

List with model statistics (n_rows, n_cols, n_nonzeros)
