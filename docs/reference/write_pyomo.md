# Write Pyomo model artifacts

This mirrors
[`write_jump()`](https://optimal2050.github.io/multimod/reference/write_jump.md)
but targets Python/Pyomo. The function emits a `model.py` file
containing component declarations, a `data_loader.py` helper that knows
how to stream data from the saved model directory (or use embedded
literals), and a lightweight `solve.py` runner.

## Usage

``` r
write_pyomo(
  model,
  file = NULL,
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

- file:

  Output file path (optional, overrides model_dir/solver_dir)

- model_dir:

  Directory where model was saved with save_model() (contains model.rds)

- solver_dir:

  Solver subdirectory name (default: "jump", e.g., "jump-highs",
  "jump-cplex")

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
  used by the other writers (`model` + `data`). Set `split = TRUE` to
  preserve the previous three-file scaffold.

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
