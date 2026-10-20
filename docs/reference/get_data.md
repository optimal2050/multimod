# Get data from model (with optional lazy loading)

Retrieve parameter, mapping, or set data from a model. Automatically
loads from disk if the model is stored and data is not in memory.

## Usage

``` r
get_data(model, name, type = c("parameter", "mapping", "set", "variable"), ...)
```

## Arguments

- model:

  A multimod model object

- name:

  Character. Name of parameter, mapping, or set

- type:

  Character. Type of object: "parameter", "mapping", or "set"

- ...:

  Additional filtering arguments (for future use)

## Value

Data.frame, character vector (for sets), or NULL

## Examples

``` r
m <- example_models$energyRt$multimod

# Parameter data
head(get_data(m, "pTechOlife"))
#>   tech region value
#> 1 ECOA     C1    30
#> 2 ECOA     C2    30
#> 3 ECOA     C3    30
#> 4 ECOA     C4    30
#> 5 ECOA     C5    30
#> 6 ECOA     W1    30

# Mapping data
head(get_data(m, "mCommTimeslice", type = "mapping"))
#>   comm timeslice
#> 1  COA    ANNUAL
#> 2  GAS    ANNUAL
#> 3  BIO    ANNUAL
#> 4  NUC    ANNUAL
#> 5  SOL   FAL_DAY
#> 6  SOL   FAL_NGT
```
