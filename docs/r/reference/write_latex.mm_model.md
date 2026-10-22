# Write LaTeX representation of a model

Write LaTeX representation of a model

## Usage

``` r
# S3 method for class 'mm_model'
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
  use_latex_names = TRUE,
  use_aliases_in_declarations = FALSE,
  folded_color = "blue",
  trimmed_color = "red",
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  An `mm_equation` (or, for the model method, an `mm_model`) object.

- file:

  Path to the output LaTeX file. If `NULL`, the LaTeX source is returned
  as a character string.

- append:

  Logical; if `TRUE`, appends to the file instead of overwriting.

- preamble:

  Character vector; LaTeX preamble to use. If `NULL`, uses the default
  preamble.

- ending:

  Character vector; LaTeX ending to use. If `NULL`, uses the default
  ending.

- title:

  Character; document title (default: `"Model: <name>"`).

- subtitle:

  Character; document subtitle (default: the model description).

- author:

  Character; document author (default: `x$authors`).

- show_date:

  Logical; if `TRUE`, print the date on the title page.

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

- alias_map:

  Named list mapping dimension names to index aliases; if `NULL` and
  `use_model_aliases = TRUE`, taken from `x$index_aliases`.

- use_model_aliases:

  Logical; if TRUE, auto-use model\$index_aliases. Default TRUE.

- use_latex_names:

  Logical; if TRUE, use LaTeX symbol names stored in
  `x$metadata$latex_names` when available. Default TRUE.

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

## Value

Invisibly, `file` when a path is given; otherwise the LaTeX source as a
single character string.
