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
  ensure_env = FALSE,
  packages = NULL,
  python_version = NULL,
  ensure_miniconda = TRUE,
  reinstall_env = FALSE,
  python = NULL,
  solver = NULL,
  env = NULL,
  load_results = FALSE,
  verbose = c("buffer", "stream", "quiet"),
  timeout = NULL,
  ...
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

- ensure_env:

  Logical; when `TRUE` the environment is checked with
  [`mm_check_python()`](https://optimal2050.github.io/multimod/r/reference/mm_check_python.md)
  and
  [`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md)
  runs if it is not usable. Default `FALSE`: the configured interpreter
  is used as it is, and nothing is installed.

- packages:

  Character vector of Python packages to request when the helper runs.
  `NULL` (default) uses
  [`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md)'s
  own list, so the environment contents have a single owner.

- python_version:

  Optional Python version passed through to
  [`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md).

- ensure_miniconda:

  Forwarded to
  [`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md).

- reinstall_env:

  When `TRUE`, asks the helper to recreate the Conda environment before
  solving.

- python:

  Optional path to a Python executable. When omitted the Python binary
  reported by
  [`reticulate::py_config()`](https://rstudio.github.io/reticulate/reference/py_config.html)
  for the activated environment is used.

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

- ...:

  Additional arguments forwarded to
  [`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md).

## Value

A list describing the solver run (exit code, success flag, elapsed time,
raw output, parsed status metadata when available, and environment
details). With `load_results = TRUE`, also `model`, the input model with
a `solution` data frame on every variable the solver reported.
`objective` is the objective value the script records in `status.json`.

## Details

The Python environment is created once with
[`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md).
With `ensure_env = TRUE`, `solve_pyomo()` checks the environment and
creates it when it is not usable, which may install Miniconda and Python
packages.
