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
if (FALSE) { # \dontrun{
# Update mapping data
model <- update_mapping(model, "mTechRegion", new_mapping_data)
} # }
```
