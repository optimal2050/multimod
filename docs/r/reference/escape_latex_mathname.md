# Escape underscores in math mode names

In math mode, underscores create subscripts. This function escapes them
with backslash to display literal underscores in variable/parameter
names.

## Usage

``` r
escape_latex_mathname(name)
```

## Arguments

- name:

  A string containing a variable or parameter name

## Value

The name with underscores escaped for LaTeX math mode
