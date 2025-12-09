# Format a LaTeX equation across multiple lines using aligned

Format a LaTeX equation across multiple lines using aligned

## Usage

``` r
format_latex_aligned(lhs, rhs, rel = "=", max_len = 90)
```

## Arguments

- lhs:

  LaTeX string of the left-hand side

- rhs:

  LaTeX string of the right-hand side

- rel:

  Relational operator (e.g., `=`, `\le`, `\ge`)

- max_len:

  Maximum allowed line length before splitting

## Value

Character string of the formatted LaTeX code
