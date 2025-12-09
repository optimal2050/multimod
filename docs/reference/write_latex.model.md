# Write LaTeX representation of a model

Write LaTeX representation of a model

## Usage

``` r
# S3 method for class 'model'
write_latex(
  x,
  file = NULL,
  append = FALSE,
  preamble = NULL,
  ending = NULL,
  title = paste0("Model: ", x$name),
  subtitle = x$desc,
  author = x$authors,
  show_date = TRUE,
  subsection_number = TRUE,
  model_view = c("reduced", "full", "both"),
  include_data = TRUE,
  data_detail = c("brief", "detailed"),
  set_display_max_inline = 10,
  set_display_head_tail_threshold = 100,
  set_display_head_n = 3,
  set_display_tail_n = 3,
  include_toc = FALSE,
  include_sets = TRUE,
  include_aliases = TRUE,
  include_index_aliases = TRUE,
  include_parameters = TRUE,
  include_variables = TRUE,
  include_equations = TRUE,
  include_mappings = TRUE,
  eq_substitute = list(when = "condition"),
  alias_map = NULL,
  use_model_aliases = TRUE,
  use_aliases_in_declarations = FALSE,
  folded_color = "blue",
  trimmed_color = "red",
  verbose = FALSE,
  ...
)
```

## Arguments

- append:

  Logical; if `TRUE`, appends to the file instead of overwriting.

- preamble:

  Character vector; LaTeX preamble to use. If `NULL`, uses the default
  preamble.

- ending:

  Character vector; LaTeX ending to use. If `NULL`, uses the default
  ending.

- subsection_number:

  Logical; if `TRUE`, includes subsection numbering in the LaTeX output.

- model_view:

  Character; display mode: "reduced" (folded dims, skip trimmed), "full"
  (original model), or "both" (comparison). Default auto-detects.

- include_data:

  Logical; if TRUE, include data context (set sizes, etc.). If FALSE,
  show pure theory. Default TRUE.

- data_detail:

  Character; "brief" (counts only) or "detailed" (full stats).

- set_display_max_inline:

  Maximum set elements to show inline.

- set_display_head_tail_threshold:

  Threshold for head/tail display.

- set_display_head_n:

  Number of head elements for large sets.

- set_display_tail_n:

  Number of tail elements for large sets.

- include_toc:

  Logical; include table of contents. Default FALSE.

- include_sets:

  Logical; include sets section. Default TRUE.

- include_aliases:

  Logical; include aliases section. Default TRUE.

- include_index_aliases:

  Logical; include index aliases section. Default TRUE.

- include_parameters:

  Logical; include parameters section. Default TRUE.

- include_variables:

  Logical; include variables section. Default TRUE.

- include_equations:

  Logical; include equations section. Default TRUE.

- include_mappings:

  Logical; include mappings section. Default TRUE.

- eq_substitute:

  Named list; substitutions for specific AST elements in the equation
  with `ast_where` object to display conditions, indices, etc. below the
  equation.

- use_model_aliases:

  Logical; if TRUE, auto-use model\$index_aliases. Default TRUE.

- use_aliases_in_declarations:

  Logical; if TRUE, use aliased dimension names in parameter/variable
  declarations. If FALSE (default), show full names in declarations but
  use aliases in equations.

- folded_color:

  Character; LaTeX color name for folded annotations. Default "blue".

- trimmed_color:

  Character; LaTeX color name for trimmed annotations. Default "red".

- verbose:

  Logical; if `TRUE`, prints additional information during processing.

- ...:

  Additional arguments passed to the `as_latex` function for rendering
  the equation.
