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
summary(example_models$OSeMOSYS$multimod)

# Parse from source (no temp files needed)
model <- read_gmpl(
  model_file = example_models$OSeMOSYS$gmpl$model,
  data_file = example_models$OSeMOSYS$gmpl$data
)
```

## Old Datasets (Moved to depreciated/)

The following datasets have been removed and consolidated into `example_models`:

- `osemosys_source.rda` → Use `example_models$OSeMOSYS`
- `energyRt_source.rda` → Use `example_models$energyRt`
- `osemosys_gmpl_source.rda` → Use `example_models$OSeMOSYS`
- `utopia_multimod.rda` → Use `example_models$energyRt$multimod`

## OSeMOSYS GMPL Source Code

The `osemosys_source.R` script imports OSeMOSYS model source code from the
OSeMOSYS_GNU_MathProg repository for testing purposes.

### Prerequisites

Clone the OSeMOSYS_GNU_MathProg repository:

```bash
cd C:/Users/admin/source
git clone https://github.com/OSeMOSYS/OSeMOSYS_GNU_MathProg.git
```

### Import Data

From R console in package root:

```r
source("data-raw/osemosys_source.R")
```

Or from command line:

```bash
Rscript data-raw/import_osemosys.R
```

This will create `data/osemosys_source.rda`.

### Data Structure

```r
osemosys_source <- list(
  model = <character vector of model code lines>,
  data = <character vector of data file lines>,
  metadata = list(
    source = "OSeMOSYS_GNU_MathProg",
    repository = "https://github.com/OSeMOSYS/OSeMOSYS_GNU_MathProg",
    license = "Apache-2.0",
    ...
  )
)
```

### License

OSeMOSYS is distributed under Apache License 2.0. See `LICENSE.note` for details.

### Citation

See `inst/CITATION` for proper citation format.

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

eneryRt is distributed under GNU Affero General Public License v3.0 (AGPL-3.0).
This is a copyleft license - derivative works must also be open source.
