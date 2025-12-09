# Remove variables not appearing in any non-trimmed equation

Final pass to mark variables that don't appear in any active equation.

## Usage

``` r
remove_unused_variables(model, verbose = FALSE)
```

## Arguments

- model:

  A multimod model object

- verbose:

  Logical. Print progress messages

## Value

Model with unused variables trimmed
