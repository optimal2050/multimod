# Write Pyomo model artifacts

This mirrors
[`write_jump()`](https://optimal2050.github.io/multimod/reference/write_jump.md)
but targets Pyomo. It writes `model.py`, holding the component
declarations and, by default, the solve runner, and `data.py`, which
reads the model's data from the workspace. With `split = TRUE` the
runner is written to a separate `solve.py`.

## Usage

``` r
write_pyomo(
  model,
  model_dir = NULL,
  solver_dir = "pyomo",
  model_name = "model",
  solver = "highs",
  use_folded = TRUE,
  cleanup = TRUE,
  data_mode = c("external", "embedded"),
  split = FALSE,
  export_vars = FALSE,
  export_data = FALSE,
  export_mps = FALSE,
  ...
)
```

## Arguments

- model:

  A `multimod` model object

- model_dir:

  Model workspace directory. The scaffold is written to
  `<model_dir>/solvers/<solver_dir>/` and reads its data from the
  workspace, so the directory must hold one written by
  [`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md);
  if it does not, the model is saved there first (CSV format). Defaults
  to the directory the model was loaded from or saved to
  (`model$base_path`).

- solver_dir:

  Solver subdirectory name (default: "jump", e.g., "jump-highs")

- model_name:

  Name for the JuMP model variable (default: "model")

- solver:

  Python solver name passed to `pyomo.opt.SolverFactory` (default
  `"highs"`).

- use_folded:

  logical; whether to use folded equations if available (default: TRUE)

- cleanup:

  logical; whether to remove existing solver_dir before generating new
  files (default: TRUE)

- data_mode:

  Whether data should be stored externally in files (`"external"`,
  default) or embedded directly inside the generated Python code
  (`"embedded"`).

- split:

  Logical flag controlling whether solver execution should live in a
  separate `solve.py` (when `TRUE`) or be embedded directly in
  `model.py` (default). The embedded mode mirrors the two-file workflow
  used by the other writers (`model` + `data`).

- export_vars:

  logical; whether to export variable values to CSV files in
  vars_export/ directory after solve (default: FALSE). Note: Solution is
  always saved to solution/ directory regardless of this setting
  (required for load_results).

- export_data:

  logical; whether to export parameter data to CSV files in data_export/
  directory (default: FALSE)

- export_mps:

  logical or character; if TRUE, exports model to MPS format; if
  character, specifies filename (default: FALSE)

- ...:

  Additional arguments (not used)

## Value

Invisibly, the path of the written `model.py`.
