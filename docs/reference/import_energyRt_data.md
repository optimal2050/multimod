# Import energyRt scenario data into a multimod model

Combines set population, parameter linking, and special handling for
bounds-style parameters (stored as single objects in energyRt but split
into \*Lo/\*Up entries inside multimod models). Creates a detailed
import log stored in model\$misc\$data_import_log.

## Usage

``` r
import_energyRt_data(
  model,
  scenario,
  inMemory = scenario@inMemory,
  log_file = NULL
)
```

## Arguments

- model:

  Multimod model

- scenario:

  energyRt scenario object

- inMemory:

  Logical. Load parameter data into memory?

- log_file:

  Optional path to export import log as CSV

## Value

Modified model with scenario data linked and import log
