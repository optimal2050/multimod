# Data Processing Scripts

This directory contains scripts for preparing package datasets.

## Current Dataset: example_models

The `DATASET.R` script combines all example models into a single unified dataset.

### Structure

```r
data(example_models)

# Contains:
example_models$energyRt     # energyRt Utopia test case
  $gams                         # GAMS format (model + data)
  $gmpl                         # GMPL format (model + data)
  $jump                         # JuMP format (model + data)
  $multimod                     # Parsed multimod object
  $metadata                     # Source information

example_models$OSeMOSYS     # OSeMOSYS Utopia test case
  $gmpl                         # GMPL format (model + data)
  $multimod                     # Parsed multimod object
  $metadata                     # Source information
```

### Usage

```r
# Load examples
data(example_models)

# Use pre-parsed models
summary(example_models$energyRt$multimod)

# Parse from source (no temp files needed)
model <- read_gams(example_models$energyRt$gams$model)
```

## Old Datasets (Moved to depreciated/)

The following datasets have been removed and consolidated into `example_models`:

- `energyRt_source.rda` → Use `example_models$energyRt`
- `utopia_multimod.rda` → Use `example_models$energyRt$multimod`

## energyRt Model Source Code

The `energyRt_source.R` script imports energyRt model files from the BASE_UTOPIA
scenario in multiple optimization modeling languages (GAMS, GMPL, JuMP, Pyomo).

### Prerequisites

The BASE_UTOPIA scenario must be generated in the multimod package:

```r
# From energyRt package
library(energyRt)
scen <- readRDS("path/to/scenario.RData")
write(scen, "multimod/dev/scenarios/BASE_UTOPIA", ...)
```

Or use existing generated files in `dev/scenarios/BASE_UTOPIA/`.

### Import Data

From R console in package root:

```r
source("data-raw/energyRt_source.R")
```

Or from command line:

```bash
Rscript data-raw/import_energyrt.R
```

This will create `data/energyRt_source.rda`.

### Data Structure

```r
energyRt_source <- list(
  gams = list(
    model = <character vector of GAMS model code>,
    data = <character vector of GAMS data assignments>
  ),
  gmpl = list(
    model = <character vector of GMPL model code>,
    data = <character vector of GMPL data statements>
  ),
  jump = list(
    model = <character vector of JuMP/Julia code>,
    data = <character vector of Julia data>
  ),
  pyomo = list(
    model = <character vector of Pyomo/Python code>,
    data = <character vector of Python data>
  ),
  metadata = list(
    source = "energyRt package",
    scenario = "BASE_UTOPIA (Utopia test case)",
    license = "MIT",
    ...
  )
)
```

### Include File Resolution

The import script automatically resolves and combines include files:

- **GAMS**: Processes `$include` directives recursively
- **GMPL**: Combines .mod and .dat files
- **JuMP**: Resolves `include()` statements in Julia
- **Pyomo**: Combines Python module files

Each included file is marked with comments showing the original structure:

```gams
* --- Included from: input/region.gms ---
<content>
* --- End include: input/region.gms ---
```

### License

energyRt is distributed under the Apache License 2.0, the same license as
multimod itself.
