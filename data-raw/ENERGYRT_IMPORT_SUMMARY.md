# energyRt Source Code Import Summary

## Overview

Successfully imported energyRt BASE_UTOPIA scenario model files in multiple formats:

- **GAMS**: 42,464 model lines + 39,624 data lines
- **GMPL**: 1,084 model lines + 38,874 data lines  
- **JuMP**: 7,379 model lines
- **Pyomo**: 3,065 model lines

## Key Features

### Include File Resolution

The import script automatically resolves and combines include files:

1. **GAMS Format**
   - Processes `$include` directives recursively
   - Combines main model file with all inc*.gms files
   - Merges data.gms with all input/*.gms files
   - Total: 82,088 lines

2. **GMPL Format**
   - Self-contained .mod and .dat files
   - No includes to resolve
   - Total: 39,958 lines

3. **JuMP Format**
   - Resolves Julia `include()` statements
   - Combines model definition with data includes
   - Total: 7,379 lines

4. **Pyomo Format**
   - Python-based model files
   - Typically self-contained
   - Total: 3,065 lines

### Data Structure

```r
energyRt_source <- list(
  gams = list(model = chr[42464], data = chr[39624]),
  gmpl = list(model = chr[1084], data = chr[38874]),
  jump = list(model = chr[7379], data = NULL),
  pyomo = list(model = chr[3065], data = NULL),
  metadata = list(...)
)
```

## Usage Examples

### Load the dataset

```r
data(energyRt_source)
```

### Extract specific format

```r
# Get GMPL model
gmpl_model <- energyRt_source$gmpl$model
gmpl_data <- energyRt_source$gmpl$data

# Write to files
writeLines(gmpl_model, "utopia.mod")
writeLines(gmpl_data, "utopia.dat")
```

### Test parser

```r
# Test multimod GMPL parser
model <- read_gmpl("utopia.mod")
model <- load_data_gmpl(model, "utopia.dat")
```

### Compare formats

```r
# Compare line counts
sapply(energyRt_source[1:4], function(x) {
  c(model = length(x$model), data = length(x$data))
})
```

## Implementation Details

### GAMS Include Resolution

The `resolve_includes()` function:
- Recursively processes `$include` directives
- Prevents circular includes
- Preserves original structure in comments
- Handles relative paths correctly

Example output:
```gams
* --- Included from: input/region.gms ---
set
region /
R1
R2
...
* --- End include: input/region.gms ---
```

### Benefits

1. **Single File Distribution**: Each format has just two files (model + data)
2. **Complete Context**: No missing includes or dependencies
3. **Testing Ready**: Can immediately test parsers without file dependencies
4. **Version Control**: Single .rda file tracks entire model
5. **Reproducible**: Import script can re-generate from source

## File Locations

- **Import Script**: `data-raw/energyRt_source.R`
- **Documentation**: `R/data-energyRt_source.R`
- **Dataset**: `data/energyRt_source.rda`
- **Helper**: `data-raw/import_energyrt.R`
- **Instructions**: `data-raw/README.md`

## Next Steps

1. Test GAMS parser with resolved model
2. Test GMPL parser with combined files
3. Validate JuMP parser
4. Compare parsed structures across formats
5. Use for regression testing
6. Create conversion test suite

## License

energyRt is distributed under MIT License. See package documentation for details.
