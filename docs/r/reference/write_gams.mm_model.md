# Write a full GAMS model file from a multimod object

Write a full GAMS model file from a multimod object

## Usage

``` r
# S3 method for class 'mm_model'
write_gams(
  model,
  file = NULL,
  format_expr = TRUE,
  gdx = NULL,
  include_solve = FALSE,
  objective = "vObjective",
  ...
)
```

## Arguments

- model:

  A `multimod` model object

- file:

  Output file path (optional)

- format_expr:

  logical; whether to format expressions with line breaks

- gdx:

  Name of a GDX to `$load` the data from, as written by
  [`write_gams_data()`](https://optimal2050.github.io/multimod/r/reference/write_gams_data.md).
  `NULL` (default) emits declarations and equations only. NEOS decodes
  its input GDX to `in.gdx`, so that is the useful name for a NEOS job.

- include_solve:

  Logical; append the `model` / `solve` statements. Needed for a
  runnable job, and harmless otherwise.

- objective:

  Name of the variable to minimise.

- ...:

  Additional arguments passed to formatting functions

## Value

Character vector or writes file if `file` is given
