# Update parameter data

Low-level function to update parameter data in a model. Use this after
data import or interpolation to set parameter values.

## Usage

``` r
update_parameter(model, name, data, folded_data = NULL, auto_save = FALSE)
```

## Arguments

- model:

  A multimod model object

- name:

  Character. Parameter name

- data:

  Data.frame with parameter values

- folded_data:

  Data.frame with folded parameter values (optional)

- auto_save:

  Logical. If TRUE and model is on-disk, immediately save to disk

## Value

Modified model object

## Examples

``` r
m <- example_models$energyRt$multimod
olife <- get_data(m, "pTechOlife")
olife$value <- olife$value + 5
m <- update_parameter(m, "pTechOlife", olife)
head(get_data(m, "pTechOlife"))
#>   tech region value
#> 1 ECOA     C1    35
#> 2 ECOA     C2    35
#> 3 ECOA     C3    35
#> 4 ECOA     C4    35
#> 5 ECOA     C5    35
#> 6 ECOA     W1    35
```
