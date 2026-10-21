# Write LaTeX representation of an equation or model

Write LaTeX representation of an equation or model

## Usage

``` r
write_latex(x, file, ...)
```

## Arguments

- x:

  An `mm_equation` or an `mm_model`.

- file:

  Path to the output LaTeX file. For a model, `NULL` returns the LaTeX
  source instead of writing it.

- ...:

  Additional arguments passed to the specific method.

## Value

For an `mm_equation`, invisibly `file`. For an `mm_model`, invisibly
`file` when a path is given, otherwise the LaTeX source as a single
character string.

## Examples

``` r
tex <- write_latex(example_models$toy, file = NULL)
cat(substr(tex, 1, 600))
#> \documentclass{article}
#> \usepackage[letterpaper,margin=1in]{geometry}
#> \usepackage{amsmath,amssymb}
#> \usepackage{breqn}
#> \usepackage{graphicx}
#> \usepackage{longtable}
#> \usepackage{adjustbox}
#> \usepackage{multicol}
#> \usepackage{bm}
#> \usepackage[mathcal]{eucal}
#> \usepackage{tikz}
#> \begin{document}
#> 
#> \title{Model: toy\\\large two-region capacity expansion toy model}
#> \date{\today}
#> \maketitle
#> 
#> \section{Sets}
#> \texttt{region} -- model regions
#>   \quad \textit{(2 elements: north, south)}
#> \\
#> \texttt{tech} -- generation technologies
#>   \quad \textit{(3 elements: coal, gas, wind)}
#> \\
#> 
#> \section{Aliases}\
#> \begin{flushl
```
