# Convert objects to LaTeX format

Convert objects to LaTeX format

Convert a character string to LaTeX-safe format

## Usage

``` r
as_latex(x, ...)

# Default S3 method
as_latex(x, ...)

# S3 method for class 'character'
as_latex(x, math = FALSE, bold = FALSE, italic = FALSE, ...)

# S3 method for class 'call'
as_latex(x, brackets = NULL, ...)

# S3 method for class 'mm_set'
as_latex(x, math_env = "text", ...)

# S3 method for class 'dims'
as_latex(x, brackets = NULL, subscript_dims = NULL, ...)

# S3 method for class 'mm_mapping'
as_latex(x, brackets = NULL, subscript_dims = is.null(brackets), ...)

# S3 method for class 'mm_parameter'
as_latex(
  x,
  brackets = NULL,
  subscript_dims = is.null(brackets),
  use_index_aliases = FALSE,
  model = NULL,
  ...
)

# S3 method for class 'mm_variable'
as_latex(
  x,
  brackets = NULL,
  subscript_dims = TRUE,
  use_index_aliases = FALSE,
  model = NULL,
  ...
)

# S3 method for class 'symbol'
as_latex(x, ...)

# S3 method for class 'shift'
as_latex(x, brackets = NULL, ...)

# S3 method for class 'constant'
as_latex(x, ...)

# S3 method for class 'unary'
as_latex(x, brackets = NULL, ...)

# S3 method for class 'expression'
as_latex(x, brackets = NULL, ...)

# S3 method for class 'when'
as_latex(
  x,
  brackets = NULL,
  use_indicator = getOption("multimod.latex.use_indicator", FALSE),
  indicator_symbol = getOption("multimod.latex.indicator_symbol", "\\delta"),
  ...
)

# S3 method for class 'sum'
as_latex(x, brackets = NULL, use_index_aliases = FALSE, model = NULL, ...)

# S3 method for class 'prod'
as_latex(x, brackets = NULL, ...)

# S3 method for class 'func'
as_latex(x, brackets = NULL, subscript_dims = TRUE, ...)
```

## Arguments

- x:

  A character string (or vector of strings) to be converted to LaTeX.

- ...:

  Further arguments passed to `as_latex()` methods for child nodes.

- math:

  Logical; if TRUE, wraps the result in math mode (`$...$`).

- bold:

  Logical; if TRUE, wraps the text in `\\textbf{...}`.

- italic:

  Logical; if TRUE, wraps the text in `\\textit{...}`.

- brackets:

  Optional bracket type used to wrap the rendered expression: `"("`,
  `"["` or the curly brace; `NULL` or `FALSE` for none.

- math_env:

  LaTeX command used to typeset set names (e.g. `"text"`, `"mathcal"`);
  `NULL` returns the plain name.

- subscript_dims:

  Logical; if `TRUE`, dimensions are rendered as a subscript, otherwise
  they are wrapped in `brackets`.

- use_index_aliases:

  Logical; if `TRUE`, dimension names are replaced by the index aliases
  stored in `model`.

- model:

  Optional `mm_model` providing `index_aliases` when
  `use_index_aliases = TRUE`.

- use_indicator:

  Logical; if `TRUE`, conditional (`when`) nodes are rendered with an
  indicator function instead of a conditional bar.

- indicator_symbol:

  LaTeX macro used as the indicator symbol when `use_indicator = TRUE`
  (default: option `multimod.latex.indicator_symbol`).

## Value

A character vector of LaTeX-safe strings.

## Examples

``` r
as_latex("alpha & beta_1 = 0.5%")
#> [1] "alpha \\& beta\\_1 = 0.5\\%"
as_latex("theta", math = TRUE)
#> [1] "$theta$"
as_latex("Note:", bold = TRUE, italic = TRUE)
#> [1] "\\textit{\\textbf{Note:}}"
```
