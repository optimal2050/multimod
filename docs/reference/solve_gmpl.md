# Solve GMPL model using glpsol command line tool

Solves an optimization model using GLPK solver (glpkAPI or glpsol)

## Usage

``` r
solve_gmpl(
  model = NULL,
  model_dir,
  method = "glpkAPI",
  verbose = TRUE,
  load_results = !is.null(model),
  glpsol_path = "glpsol",
  timeout = NULL
)
```

## Arguments

- model:

  Optional model object (for loading results back)

- model_dir:

  Directory containing model files (model.mod, data.dat, etc.)

- method:

  Solver method: "glpkAPI" (default, uses execute_glpkAPI), "glpsol"
  (uses system2 with glpsol executable)

- verbose:

  Logical; print progress messages (default: TRUE)

- load_results:

  Logical; load solution CSV results back into model object (default:
  !is.null(model))

- glpsol_path:

  Path to glpsol executable (default: "glpsol", only used if
  method="glpsol")

- timeout:

  Numeric; timeout in seconds (default: NULL, no timeout)

## Value

List with: model (if load_results=TRUE), success, objective, solve_time,
status
