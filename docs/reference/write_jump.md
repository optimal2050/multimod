# Write a Julia/JuMP model file from a multimod object

Write a Julia/JuMP model file from a multimod object

## Usage

``` r
write_jump(
  model,
  file = NULL,
  model_dir = NULL,
  solver_dir = "jump",
  output_name = "model.jl",
  model_name = "model",
  optimizer = "HiGHS.Optimizer",
  optimizer_attributes = NULL,
  use_folded = TRUE,
  cleanup = TRUE,
  export_data = FALSE,
  export_vars = FALSE,
  export_lp = FALSE,
  export_mps = FALSE,
  data_mode = c("external", "embedded"),
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

- output_name:

  Output filename (default: "model.jl")

- model_name:

  Name for the JuMP model variable (default: "model")

- optimizer:

  Julia optimizer to use (e.g., "HiGHS.Optimizer", "GLPK.Optimizer",
  "CPLEX.Optimizer")

- optimizer_attributes:

  Named list of optimizer attributes to set (e.g., list(solver = "ipm",
  presolve = "on", parallel = "on")). For HiGHS: solver can be
  "simplex", "ipm" (interior point/barrier), or "choose" (default). Use
  "ipm" for large models (\>1M rows) as it scales better than simplex.
  If NULL, uses solver defaults with automatic barrier method selection
  for large models (\>1M constraints).

- use_folded:

  logical; whether to use folded equations if available (default: TRUE)

- cleanup:

  logical; whether to remove existing solver_dir before generating new
  files (default: TRUE)

- export_data:

  logical; whether to export parameter data to CSV files in data_export/
  directory (default: FALSE)

- export_vars:

  logical; whether to export variable values to CSV files in
  vars_export/ directory after solve (default: FALSE). Note: Solution is
  always saved to solution/ directory regardless of this setting
  (required for load_results).

- export_lp:

  logical or character; if TRUE, exports model to LP format; if
  character, specifies filename (default: FALSE)

- export_mps:

  logical or character; if TRUE, exports model to MPS format; if
  character, specifies filename (default: FALSE)

- data_mode:

  character; `"external"` (default) generates a data.jl that reads
  parameter data from files at run time, `"embedded"` writes the data
  into the Julia source

- ...:

  Additional arguments (not used)

## Value

Character vector of Julia code, or writes file if path is determined

## Examples

``` r
m <- example_models$energyRt$multimod

# Explicit file path
write_jump(m, file = tempfile(fileext = ".jl"))

# Into a saved model workspace: <model_dir>/solvers/<solver_dir>/model.jl
ws <- file.path(tempdir(), "energyRt_ws")
save_model(m, ws, verbose = FALSE)
write_jump(m, model_dir = ws)
write_jump(m, model_dir = ws, solver_dir = "jump-highs")

# Barrier method for large models
write_jump(m, model_dir = ws,
           optimizer_attributes = list(solver = "ipm", parallel = "on"))
#> Removing existing solver directory: C:\Users\admin\AppData\Local\Temp\RtmpQVAZuj/energyRt_ws/solvers/jump
```
