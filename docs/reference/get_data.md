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
if (FALSE) { # \dontrun{
# Get parameter data
data <- get_data(model, "pTechCost")

# Get mapping data
data <- get_data(model, "mTechRegion", type = "mapping")

# Get set members
members <- get_data(model, "tech", type = "set")
} # }
```
