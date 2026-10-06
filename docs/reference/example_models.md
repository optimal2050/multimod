# Example Model in Multiple Formats

An example energy system model in several optimization modeling
languages (GAMS, GMPL, JuMP/Julia), along with its parsed multimod
representation. Used for testing and validating multimod's parsers and
conversion functions.

## Usage

``` r
example_models
```

## Format

A list with 1 element:

- energyRt:

  energyRt TOPIA test case with components:

  gams

  : List with `model`: energyRt's GAMS template source
    (`gams/energyRt.gms`) as a character vector, ready for
    `read_gams(include = FALSE)`

  multimod

  : Parsed multimod model object with energyRt data attached

  metadata

  : List with source, scenario, repository, license, energyRt_version,
    gams_source, date_imported, note

## Source

- energyRt: <https://github.com/energyRt/energyRt>

## Details

### Structure

The example contains:

- **Source code** in several formats (character vectors, ready to parse)

- **Parsed multimod object** (already converted, ready to use)

- **Metadata** about the source, license, and provenance

All source code character vectors can be used directly with
[`read_gams()`](https://optimal2050.github.io/multimod/reference/read_gams.md),
[`read_gmpl()`](https://optimal2050.github.io/multimod/reference/read_gmpl.md),
etc. without writing temporary files.

### energyRt Test Case

The energyRt package is an R-based energy systems modeling framework.
The TOPIA R7 kit is a small 7-region, multi-timeslice test case used for
validation.

The model *structure* is parsed from energyRt's GAMS template with
`read_gams(include = FALSE)`; GAMS is the source of truth because the
GMPL/JuMP/Pyomo templates are still evolving. The *data* is attached
from an interpolated energyRt scenario. See
`data-raw/build_energyRt_fixture.R`.

**License**: Apache License 2.0, the same license as multimod itself.
See `LICENSE.note`.

## Usage

    data(example_models)

    energyRt_gams  <- example_models$energyRt$gams$model
    energyRt_model <- example_models$energyRt$multimod

## References

energyRt package: <https://github.com/energyRt/energyRt>

## See also

- [`read_gams`](https://optimal2050.github.io/multimod/reference/read_gams.md)
  for parsing GAMS models

- [`read_gmpl`](https://optimal2050.github.io/multimod/reference/read_gmpl.md)
  for parsing GMPL models

- [`as_multimod`](https://optimal2050.github.io/multimod/reference/as_multimod.md)
  for converting model structures to multimod

## Examples

``` r
# Load examples
data(example_models)

# Inspect structure
names(example_models)
#> [1] "energyRt"
names(example_models$energyRt)
#> [1] "gams"     "multimod" "metadata"

# Check metadata
example_models$energyRt$metadata
#> $source
#> [1] "energyRt TOPIA kit"
#> 
#> $scenario
#> [1] "R7 / topia_seasons / base horizon"
#> 
#> $repository
#> [1] "https://github.com/optimal2050/energyRt"
#> 
#> $license
#> [1] "Apache-2.0"
#> 
#> $energyRt_version
#> [1] "0.90.0.9012"
#> 
#> $gams_source
#> [1] "gams/energyRt.gms"
#> 
#> $date_imported
#> [1] "2026-10-04"
#> 
#> $note
#> [1] "Model structure parsed from energyRt's GAMS template with read_gams(include = FALSE); data attached from an interpolated energyRt scenario. See data-raw/build_energyRt_fixture.R."
#> 

# Use the pre-parsed multimod model
summary(example_models$energyRt$multimod)
#>               Length Class  Mode     
#> name            0    -none- NULL     
#> desc            0    -none- NULL     
#> sets           27    -none- list     
#> aliases         7    -none- list     
#> mappings      324    -none- list     
#> parameters    218    -none- list     
#> variables      93    -none- list     
#> equations     153    -none- list     
#> inMemory        1    -none- logical  
#> base_path       0    -none- NULL     
#> metadata        2    -none- list     
#> objectives      1    -none- list     
#> index_aliases  44    -none- character
#> misc            2    -none- list     

if (FALSE) { # \dontrun{
# Parse from source (no temp files needed)
model_gams <- read_gams(
  file_or_text = example_models$energyRt$gams$model,
  include = FALSE
)

# Export to different formats
write_gmpl(example_models$energyRt$multimod, "tmp/energyrt")
write_pyomo(example_models$energyRt$multimod, "tmp/energyrt_pyomo")
} # }
```
