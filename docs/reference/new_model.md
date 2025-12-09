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

- desc:

  model description (character)

- sets:

  named list of sets

- mappings:

  named list of mappings

- parameters:

  named list of parameter objects

- variables:

  named list of variable objects

- equations:

  named list of equation objects

- metadata:

  Named list with auxiliary information (e.g., language, source_file,
  data_source).

## Value

A model object
