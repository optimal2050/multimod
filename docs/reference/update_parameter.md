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
if (FALSE) { # \dontrun{
# Update parameter data
model <- update_parameter(model, "pTechCost", new_tech_cost_data)

# Update with folded data
model <- update_parameter(model, "pTechCost",
                         data = tech_cost_data,
                         folded_data = tech_cost_folded)

# Update and immediately save to disk
model <- update_parameter(model, "pTechCost", new_data, auto_save = TRUE)
} # }
```
