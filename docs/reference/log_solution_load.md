# Log solution loading from solver to variables directory

Creates or appends to solution_log.txt with timestamp and details of
which solver's solution was loaded into the model's variables/
directory.

## Usage

``` r
log_solution_load(model_dir, solver_name, variables_loaded, verbose = TRUE)
```

## Arguments

- model_dir:

  Path to model root directory

- solver_name:

  Name of solver (e.g., "gmpl", "jump", "gams")

- variables_loaded:

  Character vector of variable names that were loaded

- verbose:

  Logical; print log entry to console (default: TRUE)

## Value

NULL (writes to log file as side effect)
