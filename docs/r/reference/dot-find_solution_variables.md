# Internal: Find solution variables in a path

Detects solution files in various locations:

- Direct solution directory with CSV files

- Model root with solvers/gmpl/solution/

- Model root with variables/ directory

## Usage

``` r
.find_solution_variables(path, solver = NULL)
```

## Arguments

- path:

  Path to model directory or solution directory

- solver:

  Solver name to use if path is a model root (e.g., "gmpl", "jump")

## Value

Named list of file paths by variable name
