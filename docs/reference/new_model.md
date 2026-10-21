# Create a multimod model object

Create a multimod model object

## Usage

``` r
new_model(
  name = NULL,
  desc = NULL,
  sets = list(),
  aliases = list(),
  mappings = list(),
  parameters = list(),
  variables = list(),
  equations = list(),
  inMemory = TRUE,
  base_path = NULL,
  metadata = list(),
  ...
)
```

## Arguments

- name:

  model name (character, optional)

- desc:

  model description (character)

- sets:

  named list of sets

- aliases:

  named list of set aliases

- mappings:

  named list of mappings

- parameters:

  named list of parameter objects

- variables:

  named list of variable objects

- equations:

  named list of equation objects

- inMemory:

  logical; whether the model data is held in memory (default `TRUE`)
  rather than read from `base_path`

- base_path:

  base directory for relative data paths when data is stored on disk

- metadata:

  Named list with auxiliary information (e.g., language, source_file,
  data_source).

- ...:

  Additional named fields stored on the model object.

## Value

A model object

## Examples

``` r
region <- new_set("region", desc = "model regions", data = c("north", "south"))
demand <- new_parameter("pDemand", desc = "annual demand, MWh", dims = "region",
                        data = data.frame(region = c("north", "south"),
                                          value = c(1000, 600)))
m <- new_model(name = "tiny", desc = "one set and one parameter",
               sets = list(region = region),
               parameters = list(pDemand = demand))
m
#> Model: tiny
#> Description: one set and one parameter
#> Sets:  1 
#> Mappings:  0 
#> Aliases:  0 
#> Parameters:  1 
#> Variables:  0 
#> Equations:  0 
```
