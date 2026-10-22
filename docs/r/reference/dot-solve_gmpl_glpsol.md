# Internal function to solve with glpsol executable

Internal function to solve with glpsol executable

## Usage

``` r
.solve_gmpl_glpsol(gmpl_dir, glpsol_path, verbose, timeout)
```

## Value

A list with elements `success`, `objective`, `solve_time`, `status`,
`exit_code`, `solver_output` and, when the solution file was parsed,
`variables` (a data frame of `name` and `value`).
