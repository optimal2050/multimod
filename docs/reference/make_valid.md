# Make a multimod model valid by reconciling symbols

Converts unresolved `symbol` nodes inside equations and parameter
formulas into typed AST nodes when possible, correcting casing and
ensuring references match declared parameters, variables, mappings, or
sets.

## Usage

``` r
make_valid(x, ...)
```

## Arguments

- x:

  A `multimod` model object

- ...:

  Reserved for future use

- verbose:

  Logical; emit a short summary of applied fixes

- stop_on_error:

  Logical; passed to
  [`validate()`](https://optimal2050.github.io/multimod/reference/validate.md)
  when `revalidate = TRUE`

- revalidate:

  Logical; run
  [`validate()`](https://optimal2050.github.io/multimod/reference/validate.md)
  after applying fixes

## Value

A model object with updated AST nodes
