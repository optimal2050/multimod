# Write a GAMS model file from a multimod object

Write a GAMS model file from a multimod object

## Usage

``` r
write_gams(model, file = NULL, format_expr = TRUE, ...)
```

## Arguments

- model:

  A `multimod` model object

- file:

  Output file path (optional)

- format_expr:

  logical; whether to format expressions with line breaks

- ...:

  Additional arguments passed to formatting functions

## Value

Character vector or writes file if `file` is given

## Examples

``` r
gms <- write_gams(example_models$toy)
cat(head(gms, 25), sep = "\n")
#> * Model: toy
#> * two-region capacity expansion toy model
#> 
#> sets
#> tech  generation technologies
#> region  model regions
#> ;
#> 
#> sets
#> mTR[tech,region]  technology x region pairs
#> mR[region]  regions
#> ;
#> 
#> parameters
#> pCapCost[tech]  'annualized capacity cost, EUR/kW'
#> pVarCost[tech]  'variable cost, EUR/MWh'
#> pAvail[tech,region]  availability factor
#> pDemand[region]  'annual demand, MWh'
#> ;
#> 
#> variables
#> vCost  'total system cost, EUR'
#> ;
#> positive variables
#> vCap[tech,region]  'installed capacity, kW'
```
