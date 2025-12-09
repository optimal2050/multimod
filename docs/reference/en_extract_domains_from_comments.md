# Extract energyRt domain mappings from GAMS file comments

Reads \*@ domain mapping hints from the GAMS source file and applies
them to variables in a model_structure object. This enables sparse
indexing in Julia/JuMP export.

## Usage

``` r
en_extract_domains_from_comments(model, verbose = FALSE)
```

## Arguments

- model:

  A model_structure object (from read_gams())

- verbose:

  Print progress messages

## Value

Modified model_structure with domain field populated for variables

## Examples

``` r
if (FALSE) { # \dontrun{
model <- read_gams("energyRt.gms")
model <- en_extract_domains_from_comments(model)
} # }
```
