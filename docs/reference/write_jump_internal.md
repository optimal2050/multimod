# Generate metadata CSV files for sets, mappings, and parameters

Generate metadata CSV files for sets, mappings, and parameters

## Usage

``` r
write_jump_internal(
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
  use_haskey = TRUE,
  data_mode = c("external", "embedded"),
  ...
)
```
