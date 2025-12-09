# Write LaTeX representation of an equation

Write LaTeX representation of an equation

## Usage

``` r
# S3 method for class 'equation'
write_latex(
  x,
  file,
  append = FALSE,
  standalone = !append,
  preamble = NULL,
  ending = NULL,
  subsection_number = FALSE,
  eq_substitute = list(when = "condition"),
  alias_map = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- append:

  Logical; if `TRUE`, appends to the file instead of overwriting.

- standalone:

  Logical; if `TRUE`, writes a complete LaTeX document with preamble and
  ending.

- preamble:

  Character vector; LaTeX preamble to use. If `NULL`, uses the default
  preamble.

- ending:

  Character vector; LaTeX ending to use. If `NULL`, uses the default
  ending.

- subsection_number:

  Logical; if `TRUE`, includes subsection numbering in the LaTeX output.

- eq_substitute:

  Named list; substitutions for specific AST elements in the equation
  with `ast_where` object to display conditions, indices, etc. below the
  equation.

- verbose:

  Logical; if `TRUE`, prints additional information during processing.

- ...:

  Additional arguments passed to the `as_latex` function for rendering
  the equation.
