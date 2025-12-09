# Analyze parameter redundancy in model

Reports repeated values in parameters to identify redundancy before and
after folding. This helps assess folding effectiveness and identify
remaining optimization potential.

## Usage

``` r
analyze_parameter_redundancy(
  model,
  params = NULL,
  verbose = TRUE,
  return_details = FALSE
)
```

## Arguments

- model:

  A multimod model object (folded or unfolded)

- params:

  Character vector of parameter names to analyze. If NULL (default),
  analyzes all parameters with data.

- verbose:

  Logical; print detailed output (default: TRUE)

- return_details:

  Logical; return detailed data frame instead of summary (default:
  FALSE)

## Value

If return_details=FALSE, invisibly returns summary data frame. If
return_details=TRUE, returns detailed data frame with per-parameter
statistics.

## Details

For each parameter with data, calculates:

- Original redundancy: ratio of repeated values to total rows

- Folded redundancy: ratio of repeated values in folded data (if folded)

- Compression achieved through folding

Redundancy metrics:

- **Original redundancy**: percentage of rows that have duplicate values
  (within the full parameter dataset)

- **Folded redundancy**: percentage of rows with duplicate values after
  folding

- **Compression ratio**: original rows / folded rows

- **Remaining potential**: estimated further compression if all
  redundancy removed

High original redundancy with no folding indicates missed optimization
opportunity. High folded redundancy suggests additional dimensions could
be folded.

## Examples

``` r
if (FALSE) { # \dontrun{
# Analyze all parameters
analyze_parameter_redundancy(model)

# Analyze specific parameters
analyze_parameter_redundancy(model, params = c("pTechCinp2use", "pSupCost"))

# Get detailed results
results <- analyze_parameter_redundancy(model, return_details = TRUE)
View(results)
} # }
```
