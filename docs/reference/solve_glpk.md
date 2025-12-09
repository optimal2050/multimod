# Solve GLPK model using glpsol command line tool

Solve GLPK model using glpsol command line tool

## Usage

``` r
solve_glpk(mod_file, dat_file, output_dir = NULL, glpsol_path = NULL)
```

## Arguments

- mod_file:

  Path to .mod file

- dat_file:

  Path to .dat file

- output_dir:

  Directory for solution output

- glpsol_path:

  Path to glpsol executable. If `NULL`, uses
  [`get_multimod_glpsol()`](https://optimal2050.github.io/multimod/reference/multimod-config.md).

## Value

List with solution data and status
