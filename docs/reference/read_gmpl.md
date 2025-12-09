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
if (FALSE) { # \dontrun{
model <- read_gmpl("model.mod", "data.dat")
} # }
```
