# Populate sets from energyRt scenario

Populate sets from energyRt scenario

## Usage

``` r
populate_sets_from_scenario(model, scenario)
```

## Arguments

- model:

  Multimod model

- scenario:

  energyRt scenario object

## Value

Modified model with set data populated

## Details

Extracts unique members for each set by scanning through all parameters
and mappings in the energyRt scenario. For each dimension that matches a
set name, collects unique values.
