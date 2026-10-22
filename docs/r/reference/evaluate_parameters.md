# Evaluate Parameter Formulas and Defaults

Convert AST formulas and default expressions to computed numeric values.
Handles dependency resolution through topological sorting.

## Usage

``` r
evaluate_parameters(
  model,
  data = list(),
  preserve_ast = TRUE,
  verbose = FALSE,
  strategy = c("auto", "dag", "iterative")
)
```

## Arguments

- model:

  Model object with parameters

- data:

  Named list of input data (sets and parameter values)

- preserve_ast:

  If TRUE, store original AST in param\$misc\$ast\_\*

- verbose:

  Print progress messages

- strategy:

  Evaluation strategy: "dag" for topological order, "iterative" for
  multi-pass retries, or "auto" (default) to try DAG first and fall back
  to iterative on failure.

## Value

Updated model with computed parameter values

## Examples

``` r
# A GMPL model with a computed parameter, parsed from text
gmpl <- read_gmpl(c(
  "set REGION;",
  "param DiscountRate{r in REGION};",
  "param GrowthFactor{r in REGION} := 1 + DiscountRate[r];",
  "var x{r in REGION} >= 0;",
  "minimize obj: sum{r in REGION} GrowthFactor[r] * x[r];",
  "s.t. c1{r in REGION}: x[r] >= 1;",
  "end;"
))
data <- list(
  REGION = c("R1", "R2"),
  DiscountRate = c(R1 = 0.05, R2 = 0.07)
)
model <- evaluate_parameters(gmpl, data)
model$parameters$GrowthFactor$data
#>   REGION value
#> 1     R1  1.05
#> 2     R2  1.07
```
