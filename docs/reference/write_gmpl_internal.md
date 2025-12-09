# Write a full GMPL model file from a model_structure object

Write a full GMPL model file from a model_structure object

## Usage

``` r
write_gmpl_internal(
  model,
  file = NULL,
  format_expr = FALSE,
  include_solve = TRUE,
  export_vars = NULL,
  use_table_output = FALSE,
  use_folded = FALSE,
  objective = "cost",
  export_data = FALSE,
  drop_default_values = FALSE,
  ...
)
```

## Arguments

- model:

  A `model_structure` object from read_gams

- file:

  Output file path (optional)

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

- ...:

  Additional arguments passed to formatting functions

## Value

Character vector or writes file if `file` is given Internal function to
write GMPL model
