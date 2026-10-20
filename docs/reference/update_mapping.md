# Update mapping data

Low-level function to update mapping data in a model.

## Usage

``` r
update_mapping(model, name, data, auto_save = FALSE)
```

## Arguments

- model:

  A multimod model object

- name:

  Character. Mapping name

- data:

  Data.frame with mapping tuples

- auto_save:

  Logical. If TRUE and model is on-disk, immediately save to disk

## Value

Modified model object

## Examples

``` r
m <- example_models$energyRt$multimod
map <- get_data(m, "mCommTimeslice", type = "mapping")
m <- update_mapping(m, "mCommTimeslice", map[1:10, ])
nrow(get_data(m, "mCommTimeslice", type = "mapping"))
#> [1] 10
```
