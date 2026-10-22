# Example models

Two example models: a small capacity-expansion LP written with the
multimod constructors, and the energyRt energy system model, both as
GAMS source text and as the multimod model parsed from it with data
attached.

## Usage

``` r
example_models
```

## Format

A list with 2 elements:

- toy:

  A two-region capacity-expansion LP (3 technologies, 2 regions, 4
  parameters, 3 variables, 3 equations) written with the multimod
  constructors in `data-raw/toy_model.R`. Every parameter carries data,
  so
  [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md)
  gives a 9 x 13 LP and
  [`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.md)
  finds the optimum (198000).

- energyRt:

  The energyRt model on the TOPIA test case:

  gams

  :   List with `model`: energyRt's GAMS template source
      (`gams/energyRt.gms`) as a character vector, ready for
      `read_gams(include = FALSE)`.

  multimod

  :   The multimod model parsed from that source, with data from an
      interpolated energyRt scenario attached.

  metadata

  :   List with source, scenario, repository, license, energyRt_version,
      gams_source, date_imported, note.

## Source

- energyRt: <https://github.com/optimal2050/energyRt>

## Details

energyRt is an R package for energy system modeling. The TOPIA R7 kit is
a seven-region test case with a seasonal timeslice calendar.

The model *structure* is parsed from energyRt's GAMS template with
`read_gams(include = FALSE)`; the *data* is attached from an
interpolated energyRt scenario. See `data-raw/build_energyRt_fixture.R`
in the source repository.

**License**: the energyRt content is distributed under the Apache
License 2.0, the license of multimod itself. See the `COPYRIGHTS` file
installed with the package,
`system.file("COPYRIGHTS", package = "multimod")`.

## See also

- [`read_gams`](https://optimal2050.github.io/multimod/r/reference/read_gams.md)
  for parsing GAMS models

- [`read_gmpl`](https://optimal2050.github.io/multimod/r/reference/read_gmpl.md)
  for parsing MathProg models

- [`as_multimod`](https://optimal2050.github.io/multimod/r/reference/as_multimod.md)
  for converting model structures to multimod

## Examples

``` r
names(example_models)
#> [1] "energyRt" "toy"     
names(example_models$energyRt)
#> [1] "gams"     "multimod" "metadata"
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

# the small model built in R
example_models$toy
#> Model: toy
#> Description: two-region capacity expansion toy model
#> Language:  multimod 
#> Sets:  2 
#> Mappings:  2 
#> Aliases:  0 
#> Parameters:  4 
#> Variables:  3 
#> Equations:  3 

# the parsed energyRt model
example_models$energyRt$multimod
#> Model: 
#> Source:  C:/Users/admin/Documents/R/energyRt/gams/energyRt.gms 
#> Language:  GAMS 
#> Sets:  27 
#> Mappings:  324 
#> Aliases:  7 
#> Parameters:  218 
#> Variables:  93 
#> Equations:  153 

# \donttest{
# parse the structure again from the bundled GAMS source
structure <- read_gams(
  file_or_text = example_models$energyRt$gams$model,
  include = FALSE
)
# }
```
