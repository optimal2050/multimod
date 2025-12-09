# Validate index_aliases for a model

Checks that index_aliases are properly defined and don't conflict with
set names, aliases, or other symbols in the model.

## Usage

``` r
validate_index_aliases(x, verbose = FALSE)
```

## Arguments

- x:

  A multimod model object

- verbose:

  Logical; if TRUE, print detailed messages

## Value

A list with components: errors, warnings, info
