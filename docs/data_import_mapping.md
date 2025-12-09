# Data Import Mapping System

## Overview

The data import mapping system tracks the relationship between energyRt scenario data and multimod model parameters. This is essential for understanding data provenance and handling special cases like bounds parameters that split during import.

## Key Problem Solved

### Bounds Parameter Splitting

**energyRt storage:**
```
Parameter: pTechAf (type = "bounds")
Data structure:
  tech region year slice type  value
  ECOA   R1   2025  A_D   up    1.0
  ECOA   R1   2025  A_D   lo    0.0
  ...
Default values: c(0, 1)  # lo, up
```

**Multimod/Solver export:**
```
Two separate parameters:
- pTechAfLo (lower bounds)
- pTechAfUp (upper bounds)
```

The import log tracks this transformation so you can trace back to the source.

## Import Log Structure

The import log is stored in `model$misc$data_import_log` and contains:

### 1. Sets (`$sets`)
Tracks set population from scenario parameters.

```r
$sets$tech
  $multimod_name: "tech"
  $type: "set"
  $status: "populated"
  $n_members: 10
  $source_parameters: c("pTechCap", "pTechAf", ...)
  $n_sources: 5
```

### 2. Parameters (`$parameters`)
Tracks regular parameter linkage.

```r
$parameters$pDemand
  $multimod_name: "pDemand"
  $scenario_name: "pDemand"
  $type: "parameter"
  $status: "linked"
  $dims: "comm, region, year, slice"
  $n_dims: 4
  $defVal: "0"
  $has_data: TRUE
  $n_rows: 1250
  $path: "scenario/parameters/pDemand"
  $inMemory: FALSE
```

### 3. Mappings (`$mappings`)
Tracks mapping linkage (similar structure to parameters).

### 4. Bounds (`$bounds`)
Special tracking for bounds parameter splits.

```r
$bounds$pTechAf
  $scenario_name: "pTechAf"
  $type: "bounds"
  $multimod_lo: "pTechAfLo"
  $multimod_up: "pTechAfUp"
  $status_lo: "split_linked"
  $status_up: "split_linked"
  $dims: "tech, region, year, slice"
  $n_dims: 4
  $defVal_lo: 0
  $defVal_up: 1
  $path: "scenario/parameters/pTechAf"
  $issues: NA
```

### 5. Unmatched (`$unmatched`)
Summary of elements that couldn't be matched.

```r
$unmatched
  $multimod_params_without_data: c("pCustomParam", ...)
  $multimod_mappings_without_data: c()
  $scenario_params_unmatched: c("pObsoleteParam", ...)
  $scenario_mappings_unmatched: c()
  $scenario_bounds_unmatched: c()
```

## Usage

### Basic Import with Logging

```r
library(multimod)

# Load model and scenario
model <- read_gams("model.gms") %>% as_multimod()
scenario <- energyRt::loadScenario("scenario.RData")

# Import with automatic logging and CSV export
model <- import_energyRt_data(
  model, 
  scenario, 
  inMemory = FALSE,
  log_file = "import_log.csv"  # Optional CSV export
)
```

### Accessing the Import Log

```r
# Get the full log
log <- get_import_log(model)

# View structure
str(log, max.level = 2)

# Check timestamp
model$misc$data_import_timestamp
```

### Inspecting Bounds Mapping

```r
# Show all bounds parameter splits
bounds_df <- show_bounds_mapping(model)
print(bounds_df)

# Output:
#   scenario_param multimod_lo   multimod_up   status_lo      status_up      dims
# 1 pTechAf        pTechAfLo     pTechAfUp     split_linked   split_linked   tech,region,year,slice
# 2 pStoreCap      pStoreCapLo   pStoreCapUp   split_linked   split_linked   storage,region,year
```

### Finding Unmatched Elements

```r
# Display summary of unmatched elements
show_unmatched(model)

# Or access programmatically
log <- get_import_log(model)
unmatched_params <- log$unmatched$multimod_params_without_data
```

### Exporting Log to CSV

```r
# Export at any time
export_import_log_from_model(model, "my_import_log.csv")
```

## CSV Export Format

The CSV export flattens the log structure:

| category    | multimod_name | scenario_name | status        | dims            | defVal | n_rows | path                    |
|-------------|---------------|---------------|---------------|-----------------|--------|--------|-------------------------|
| set         | tech          | NA            | populated     | NA              | NA     | NA     | NA                      |
| parameter   | pDemand       | pDemand       | linked        | comm,region,... | 0      | 1250   | scenario/parameters/... |
| bounds_lo   | pTechAfLo     | pTechAf       | split_linked  | tech,region,... | 0      | NA     | scenario/parameters/... |
| bounds_up   | pTechAfUp     | pTechAf       | split_linked  | tech,region,... | 1      | NA     | scenario/parameters/... |

## Use Cases

### 1. Debugging Missing Data
```r
show_unmatched(model)
# Identifies parameters in model without scenario data
# and scenario parameters not matched in model
```

### 2. Tracing Data Provenance
```r
log <- get_import_log(model)

# Find source of a multimod parameter
param_log <- log$parameters$pDemand
cat("Data source:", param_log$path, "\n")
cat("Dimensions:", param_log$dims, "\n")

# Find which scenario param feeds a bounds parameter
bounds_log <- log$bounds$pTechAf
cat("Lower bound from:", bounds_log$scenario_name, "\n")
cat("Default lower:", bounds_log$defVal_lo, "\n")
```

### 3. Validating Import Completeness
```r
log <- get_import_log(model)

# Count successful links
n_params_linked <- sum(sapply(log$parameters, 
                              function(x) x$status == "linked"))
n_params_total <- length(log$parameters)

cat("Parameters:", n_params_linked, "/", n_params_total, "linked\n")

# Check for issues
if (length(log$unmatched$multimod_params_without_data) > 0) {
  warning("Some model parameters have no data!")
}
```

### 4. Reconstructing Bounds Parameters
```r
# If you need to reconstruct the original bounds structure
log <- get_import_log(model)

for (b in log$bounds) {
  cat("\nBounds parameter:", b$scenario_name, "\n")
  cat("  Split into:", b$multimod_lo, "and", b$multimod_up, "\n")
  cat("  Defaults: [", b$defVal_lo, ",", b$defVal_up, "]\n")
  
  # Load the split data
  lo_data <- model$parameters[[b$multimod_lo]]$data
  up_data <- model$parameters[[b$multimod_up]]$data
  
  # Could recombine if needed...
}
```

## Status Values

### Parameter/Mapping Status
- `"linked"`: Successfully matched and data linked
- `"unmatched_in_multimod"`: Exists in scenario but not in model structure
- `"split_linked"`: Bounds parameter successfully split and linked

### Set Status
- `"populated"`: Set has members extracted from data
- `"empty"`: Set defined but no members found

## Implementation Details

The import log is created during `import_energyRt_data()` execution:

1. **Set population** - Tracks which parameters contribute members to each set
2. **Regular linking** - Links numpar and map type parameters
3. **Bounds processing** - Identifies bounds parameters, splits data by type column, creates Lo/Up parameters
4. **Unmatched detection** - Compares model structure with scenario data
5. **Log storage** - Stores complete log in `model$misc$data_import_log`
6. **CSV export** - Optionally exports flattened version

## Related Functions

- `import_energyRt_data()` - Main import function with logging
- `get_import_log()` - Retrieve log from model
- `show_bounds_mapping()` - Display bounds parameter splits
- `show_unmatched()` - Display unmatched elements
- `export_import_log_from_model()` - Export log to CSV

## Future Enhancements

Potential additions:
- Reverse mapping (multimod → energyRt for writing results back)
- Data quality checks (missing values, out-of-range, etc.)
- Version tracking for scenario data
- Diff between imports from different scenarios
