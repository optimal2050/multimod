# Solve a Pyomo model exported by `multimod`

Runs the generated `model.py` (or `solve.py`, written by
`write_pyomo(split = TRUE)`) in `solvers/pyomo/` with the configured
Python interpreter, and returns basic status information together with
the status the script writes to `solution/status.json`. With
`load_results = TRUE` the variable values the script exports are read
back into `model`.

## Usage

``` r
solve_pyomo(
  model = NULL,
  model_dir,
  solver_dir = "pyomo",
  env_name = "multimod",
  python = NULL,
  solver = NULL,
  env = NULL,
  load_results = FALSE,
  verbose = c("buffer", "stream", "quiet"),
  timeout = NULL
)
```

## Arguments

- model:

  Optional multimod model. Required when `load_results = TRUE`: the
  solution of each variable is stored in
  `model$variables[[name]]$solution`.

- model_dir:

  Path to the saved model directory created by
  [`save_model()`](https://optimal2050.github.io/multimod/r/reference/save_model.md).
  Must contain `solvers/<solver_dir>/model.py` (generated via
  [`write_pyomo()`](https://optimal2050.github.io/multimod/r/reference/write_pyomo.md)).

- solver_dir:

  Name of the solver sub-directory inside `model_dir/solvers`. Defaults
  to "pyomo" to match
  [`write_pyomo()`](https://optimal2050.github.io/multimod/r/reference/write_pyomo.md).

- env_name:

  Name of the Conda environment handled by
  [`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md).
  Defaults to "multimod".

- python:

  Optional path to a Python executable. When omitted it is resolved by
  [`get_multimod_python()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md).

- solver:

  Optional solver name that is exported to the Python process via the
  `PYOMO_SOLVER` environment variable.

- env:

  Named character vector of additional environment variables to pass to
  the Python process.

- load_results:

  Logical; when `TRUE`, read `solution/variables.json` into `model` and
  return the model as `result$model`. The file is written by the
  generated script only when the scaffold was produced with
  `write_pyomo(export_vars = TRUE)`.

- verbose:

  Controls solver logging capture. Logical (`TRUE` = buffer output,
  `FALSE` = quiet) or one of `"buffer"`, `"stream"`, `"quiet"`.

- timeout:

  Optional timeout (in seconds). Requires the `processx` package.

## Value

A list describing the solver run (exit code, success flag, elapsed time,
raw output, parsed status metadata when available, and environment
details). With `load_results = TRUE`, also `model`, the input model with
a `solution` data frame on every variable the solver reported.
`objective` is the objective value the script records in `status.json`.

## Details

The Python environment is set up beforehand, for instance once with
[`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md);
`solve_pyomo()` installs nothing. When the interpreter cannot import
`pyomo`, it stops and says so.
