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

# S3 method for class 'set'
as_latex(x, math_env = "text", ...)

# S3 method for class 'dims'
as_latex(x, brackets = NULL, subscript_dims = NULL, ...)

# S3 method for class 'expression'
as_latex(x, brackets = NULL, subscript_dims = is.null(brackets), ...)

# S3 method for class 'parameter'
as_latex(
  x,
  brackets = NULL,
  subscript_dims = is.null(brackets),
  use_index_aliases = FALSE,
  model = NULL,
  ...
)

# S3 method for class 'variable'
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
as_latex(x, brackets = NULL, ...)

# S3 method for class 'prod'
as_latex(x, brackets = NULL, ...)

# S3 method for class 'func'
as_latex(x, brackets = NULL, subscript_dims = TRUE, ...)
```

## Arguments

- x:

  A character string (or vector of strings) to be converted to LaTeX.

- math:

  Logical; if TRUE, wraps the result in math mode (`$...$`).

- bold:

  Logical; if TRUE, wraps the text in `\\textbf{...}`.

- italic:

  Logical; if TRUE, wraps the text in `\\textit{...}`.

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
