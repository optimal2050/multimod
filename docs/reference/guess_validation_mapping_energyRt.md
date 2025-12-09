# Guess validation mapping for fold specification (energyRt-specific)

Uses parameter dimensions and entity-to-mapping configuration to select
the appropriate validation mapping. Uses PRIORITY-BASED selection:
returns the mapping for the FIRST matching entity dimension found.

## Usage

``` r
guess_validation_mapping_energyRt(
  param_name,
  dim_to_fold,
  dims,
  entity_mapping
)
```

## Arguments

- param_name:

  Parameter name (for debugging)

- dim_to_fold:

  Dimension to fold

- dims:

  All dimensions in parameter

- entity_mapping:

  Named list mapping entity dimensions to validation mappings. Values
  can be single string or character vector for chains. Example:
  list(tech = "mTechSlice", stg = c("mStorageComm", "mCommSlice"))

## Value

Mapping specification: single string or character vector for chains

## Details

Supports mapping chains for derived mappings (e.g., stg → comm → slice).
