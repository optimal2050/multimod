# Solve a Pyomo model exported by `multimod`

Executes the generated `solve.py` script inside `solvers/pyomo/` after
ensuring a Python environment exists. By default the helper
[`setup_python_environment()`](https://optimal2050.github.io/multimod/reference/setup_python_environment.md)
is invoked so that a Conda environment named "multimod" (configurable)
is created, populated with Pyomo, and activated via the `reticulate`
toolkit. Once the environment is ready the solver script is executed
with [`system2()`](https://rdrr.io/r/base/system2.html) or `processx`
and basic status information is returned. Result loading is not
implemented yet but the metadata emitted by `solve.py` (e.g.
`solution/status.json`) is surfaced when available.

## Usage

``` r
solve_pyomo(
  model = NULL,
  model_dir,
  solver_dir = "pyomo",
  env_name = "multimod",
  ensure_env = TRUE,
  packages = c("pyomo"),
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

  Optional multimod model; currently only used when
  `load_results = TRUE` to indicate where solutions should be loaded
  (not yet implemented).

- model_dir:

  Path to the saved model directory created by
  [`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md).
  Must contain `solvers/<solver_dir>/solve.py` (generated via
  [`write_pyomo()`](https://optimal2050.github.io/multimod/reference/write_pyomo.md)).

- solver_dir:

  Name of the solver sub-directory inside `model_dir/solvers`. Defaults
  to "pyomo" to match
  [`write_pyomo()`](https://optimal2050.github.io/multimod/reference/write_pyomo.md).

- env_name:

  Name of the Conda environment handled by
  [`setup_python_environment()`](https://optimal2050.github.io/multimod/reference/setup_python_environment.md).
  Defaults to "multimod".

- ensure_env:

  Logical; when `TRUE` (default) the helper
  [`setup_python_environment()`](https://optimal2050.github.io/multimod/reference/setup_python_environment.md)
  runs to create/activate the environment. Set to `FALSE` if you want to
  manage activation yourself.

- packages:

  Character vector of Python packages to request when the helper runs.
  Defaults to `"pyomo"`.

- python_version:

  Optional Python version passed through to
  [`setup_python_environment()`](https://optimal2050.github.io/multimod/reference/setup_python_environment.md).

- ensure_miniconda:

  Forwarded to
  [`setup_python_environment()`](https://optimal2050.github.io/multimod/reference/setup_python_environment.md).

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

  Logical; reserved for future work. When `TRUE` a warning is issued
  because loading solutions back into R is not yet implemented.

- verbose:

  Controls solver logging capture. Logical (`TRUE` = buffer output,
  `FALSE` = quiet) or one of `"buffer"`, `"stream"`, `"quiet"`.

- timeout:

  Optional timeout (in seconds). Requires the `processx` package.

- ...:

  Additional arguments forwarded to
  [`setup_python_environment()`](https://optimal2050.github.io/multimod/reference/setup_python_environment.md).

## Value

A list describing the solver run (exit code, success flag, elapsed time,
raw output, parsed status metadata when available, and environment
details).
