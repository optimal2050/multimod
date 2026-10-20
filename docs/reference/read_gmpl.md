# Read GMPL model file

Read GMPL model file

## Usage

``` r
read_gmpl(
  model_file,
  data_file = NULL,
  as_multimod = !is.null(data_file),
  verbose = FALSE
)
```

## Arguments

- model_file:

  Path to GMPL model file or character vector of GMPL code

- data_file:

  Optional path to GMPL data file

- as_multimod:

  Logical; convert to multimod format (default: TRUE if data_file
  provided)

- verbose:

  Logical; print detailed progress messages (default: FALSE)

## Value

Parsed GMPL model structure or multimod object

## Examples

``` r
# Parse model text directly (a character vector of GMPL lines)
structure <- read_gmpl(c(
  "set REGION;",
  "param Demand{r in REGION};",
  "var x{r in REGION} >= 0;",
  "minimize cost: sum{r in REGION} x[r];",
  "s.t. meet{r in REGION}: x[r] >= Demand[r];",
  "end;"
))
names(structure$parameters)
#> [1] "Demand"

if (FALSE) { # \dontrun{
# Model and data files on disk
model <- read_gmpl("model.mod", "data.dat")
} # }
```
