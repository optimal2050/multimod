# Write LaTeX representation of an equation

Write LaTeX representation of an equation

## Usage

``` r
# S3 method for class 'mm_equation'
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

- x:

  An `mm_equation` (or, for the model method, an `mm_model`) object.

- file:

  Path to the output LaTeX file.

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

- alias_map:

  Named list mapping dimension names to index aliases used in the
  rendered equations; `NULL` for none.

- verbose:

  Logical; if `TRUE`, prints additional information during processing.

- ...:

  Additional arguments passed to the `as_latex` function for rendering
  the equation.

## Value

Invisibly, `file`.
