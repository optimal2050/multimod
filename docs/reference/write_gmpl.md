# Write a GMPL/MathProg model file from a multimod object

Write a GMPL/MathProg model file from a multimod object

## Usage

``` r
write_gmpl(
  model,
  file = NULL,
  model_dir = model$base_dir,
  format_expr = FALSE,
  include_solve = TRUE,
  export_vars = NULL,
  use_table_output = FALSE,
  use_folded = NULL,
  objective = "cost",
  export_data = FALSE,
  drop_default_values = FALSE,
  INF = 1e+20,
  check = TRUE,
  resolve_aliases = TRUE,
  verbose = FALSE,
  export_mps = FALSE,
  stop_on_error = TRUE,
  ...
)
```

## Arguments

- model:

  A `multimod` model object

- file:

  Output file path (optional, ignored if model_dir is provided)

- model_dir:

  Optional directory path for organized model structure (creates
  solvers/gmpl/)

- format_expr:

  logical; whether to format expressions with line breaks

- include_solve:

  logical; whether to include solve and end statements

- export_vars:

  character vector of variable names to export to CSV; if NULL
  (default), all variables are exported; if FALSE or character(0), no
  custom export

- use_table_output:

  logical; whether to add GMPL table statement for output (CSV format);
  default FALSE

- use_folded:

  logical; whether to use folded equations/data if available (default:
  NULL = auto-detect)

- objective:

  character; name of the objective variable (default: "cost")

- export_data:

  logical; whether to insert data export code after parameters (default:
  FALSE)

- drop_default_values:

  logical; if TRUE with export_data, exclude values that match defaults
  (default: FALSE)

- INF:

  numeric; value to use for Inf in parameter defaults (default: 1e20)

- check:

  logical; if TRUE, run validation checks before writing (default: TRUE)

- resolve_aliases:

  logical; if TRUE, resolve set aliases to base sets in generated GMPL
  (default: TRUE)

- verbose:

  logical; if TRUE, print detailed validation progress (default: FALSE)

- export_mps:

  logical; if TRUE, also export MPS format file (default: FALSE)

- stop_on_error:

  logical; if TRUE, stop on validation errors; if FALSE, issue warnings
  (default: TRUE)

- ...:

  Additional arguments passed to formatting functions

## Value

Character vector or writes file if `file` or `model_dir` is given
