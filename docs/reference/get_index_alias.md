# Get index alias for a set

Retrieves the index alias for a set name from a multimod model. If no
alias exists, generates one on-the-fly.

## Usage

``` r
get_index_alias(model, set_name)
```

## Arguments

- model:

  A multimod model object

- set_name:

  Character. Name of the set

## Value

Character. The index alias for the set

## Examples

``` r
model <- example_models$energyRt$multimod
model <- add_index_aliases(model)
#> Model already has index_aliases. Use overwrite=TRUE to replace them.
get_index_alias(model, "tech")  # "h"
#> [1] "h"
```
