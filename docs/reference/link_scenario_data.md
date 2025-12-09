# Link energyRt scenario data to model

Links data from an energyRt scenario to a multimod model structure. The
data remains on disk (lazy loading) - only references are stored.

## Usage

``` r
link_scenario_data(model, scenario, inMemory = FALSE)
```

## Arguments

- model:

  A multimod model object

- scenario:

  An energyRt scenario object with modInp@parameters

- inMemory:

  Logical. Load parameter data into memory instead of referencing
  on-disk Arrow files?

## Value

The model with linked data references
