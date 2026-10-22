# Mark variables with empty domains

Checks if any dimension of a variable references an empty or trimmed
set.

## Usage

``` r
mark_empty_variables(model, verbose = FALSE)
```

## Arguments

- model:

  A multimod model object

- verbose:

  Logical. Print progress messages

## Value

Model with `$trimmed = TRUE` on variables with empty domains
