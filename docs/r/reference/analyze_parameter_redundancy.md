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
m <- example_models$energyRt$multimod

# Analyze specific parameters
analyze_parameter_redundancy(m, params = c("pTechCinp2use", "pSupCost"))
#> 
#> === Parameter Redundancy Analysis ===
#> Parameters analyzed: 2 
#> Folded parameters: 0 
#> 
#> --- Summary Statistics ---
#> Original redundancy (avg): 97.2%
#> 
#> --- High Redundancy Parameters (>50% repeated values) ---
#>     param_name is_folded orig_redundancy_pct fold_redundancy_pct
#>  pTechCinp2use     FALSE                99.8                  NA
#>       pSupCost     FALSE                94.7                  NA
#>  remaining_potential
#>                  448
#>                   19
#> 
#> --- Folded Parameters with Remaining Redundancy (>20%) ---
#> None - excellent folding!
#> 
#> --- Unfolded Parameters with High Redundancy ---
#>     param_name orig_redundancy_pct orig_rows orig_unique remaining_potential
#>  pTechCinp2use                99.8      1344           3                 448
#>       pSupCost                94.7        76           4                  19
#> 
#> These parameters are good candidates for folding

# Analyze all parameters and keep the per-parameter details
results <- analyze_parameter_redundancy(m, return_details = TRUE)
#> 
#> === Parameter Redundancy Analysis ===
#> Parameters analyzed: 38 
#> Folded parameters: 0 
#> 
#> --- Summary Statistics ---
#> Original redundancy (avg): 80.4%
#> 
#> --- High Redundancy Parameters (>50% repeated values) ---
#>          param_name is_folded orig_redundancy_pct fold_redundancy_pct
#>       pTechCinp2use     FALSE                99.8                  NA
#>         pTradeIrEff     FALSE                99.8                  NA
#>           pTechAfLo     FALSE                99.7                  NA
#>      pStorageInpEff     FALSE                99.7                  NA
#>      pStorageOutEff     FALSE                99.7                  NA
#>       pTimesliceAgg     FALSE                99.4                  NA
#>    pTimesliceWeight     FALSE                98.5                  NA
#>       pTradeInvcost     FALSE                97.9                  NA
#>           pTradeEac     FALSE                97.9                  NA
#>               pWacc     FALSE                96.4                  NA
#>                pSdr     FALSE                96.4                  NA
#>  pStorageOutInvcost     FALSE                96.4                  NA
#>      pStorageOutEac     FALSE                96.4                  NA
#>  pStorageDurationLo     FALSE                96.4                  NA
#>  pStorageDurationUp     FALSE                96.4                  NA
#>     pImportRowPrice     FALSE                96.4                  NA
#>          pTechFixom     FALSE                96.0                  NA
#>        pTechInvcost     FALSE                96.0                  NA
#>         pTradeStock     FALSE                95.8                  NA
#>            pTechEac     FALSE                95.7                  NA
#>            pSupCost     FALSE                94.7                  NA
#>      pTechStockSurv     FALSE                92.8                  NA
#>          pTechOlife     FALSE                90.9                  NA
#>        pTechCap2act     FALSE                85.7                  NA
#>     pDiscountFactor     FALSE                85.7                  NA
#>       pStorageOlife     FALSE                85.7                  NA
#>          pTechStock     FALSE                84.9                  NA
#>         pTradeOlife     FALSE                83.3                  NA
#>       pTradeCap2Act     FALSE                83.3                  NA
#>       pYearFraction     FALSE                75.0                  NA
#>       pTechEmisComm     FALSE                66.7                  NA
#>    pTechWeatherAfUp     FALSE                66.7                  NA
#>     pTimesliceShare     FALSE                58.8                  NA
#>  remaining_potential
#>                448.0
#>                576.0
#>                336.0
#>                336.0
#>                336.0
#>                180.0
#>                 68.0
#>                 48.0
#>                 48.0
#>                 28.0
#>                 28.0
#>                 28.0
#>                 28.0
#>                 28.0
#>                 28.0
#>                 28.0
#>                 25.1
#>                 25.1
#>                 24.0
#>                 23.3
#>                 19.0
#>                 13.9
#>                 11.0
#>                  7.0
#>                  7.0
#>                  7.0
#>                  6.6
#>                  6.0
#>                  6.0
#>                  4.0
#>                  3.0
#>                  3.0
#>                  2.4
#> 
#> --- Folded Parameters with Remaining Redundancy (>20%) ---
#> None - excellent folding!
#> 
#> --- Unfolded Parameters with High Redundancy ---
#>          param_name orig_redundancy_pct orig_rows orig_unique
#>       pTechCinp2use                99.8      1344           3
#>         pTradeIrEff                99.8       576           1
#>           pTechAfLo                99.7       336           1
#>      pStorageInpEff                99.7       336           1
#>      pStorageOutEff                99.7       336           1
#>       pTimesliceAgg                99.4       180           1
#>    pTimesliceWeight                98.5        68           1
#>       pTradeInvcost                97.9        48           1
#>           pTradeEac                97.9        48           1
#>               pWacc                96.4        28           1
#>                pSdr                96.4        28           1
#>  pStorageOutInvcost                96.4        28           1
#>      pStorageOutEac                96.4        28           1
#>  pStorageDurationLo                96.4        28           1
#>  pStorageDurationUp                96.4        28           1
#>     pImportRowPrice                96.4        56           2
#>          pTechFixom                96.0       176           7
#>        pTechInvcost                96.0       176           7
#>         pTradeStock                95.8        24           1
#>            pTechEac                95.7       140           6
#>            pSupCost                94.7        76           4
#>      pTechStockSurv                92.8        97           7
#>          pTechOlife                90.9        44           4
#>        pTechCap2act                85.7         7           1
#>     pDiscountFactor                85.7        21           3
#>       pStorageOlife                85.7         7           1
#>          pTechStock                84.9       106          16
#>         pTradeOlife                83.3         6           1
#>       pTradeCap2Act                83.3         6           1
#>       pYearFraction                75.0         4           1
#>       pTechEmisComm                66.7         3           1
#>    pTechWeatherAfUp                66.7         3           1
#>     pTimesliceShare                58.8        17           7
#>  remaining_potential
#>                448.0
#>                576.0
#>                336.0
#>                336.0
#>                336.0
#>                180.0
#>                 68.0
#>                 48.0
#>                 48.0
#>                 28.0
#>                 28.0
#>                 28.0
#>                 28.0
#>                 28.0
#>                 28.0
#>                 28.0
#>                 25.1
#>                 25.1
#>                 24.0
#>                 23.3
#>                 19.0
#>                 13.9
#>                 11.0
#>                  7.0
#>                  7.0
#>                  7.0
#>                  6.6
#>                  6.0
#>                  6.0
#>                  4.0
#>                  3.0
#>                  3.0
#>                  2.4
#> 
#> These parameters are good candidates for folding
head(results)
#>      param_name n_dims                                dims is_folded
#> 1 pYearFraction      1                                year     FALSE
#> 2    pTechOlife      2                        tech, region     FALSE
#> 3 pTechCinp2use      5 tech, comm, region, year, timeslice     FALSE
#> 4 pTechEmisComm      2                          tech, comm     FALSE
#> 5    pTechFixom      3                  tech, region, year     FALSE
#> 6  pTechInvcost      3                  tech, region, year     FALSE
#>   removed_dims orig_rows orig_unique orig_redundancy_pct fold_rows fold_unique
#> 1                      4           1                75.0        NA          NA
#> 2                     44           4                90.9        NA          NA
#> 3                   1344           3                99.8        NA          NA
#> 4                      3           1                66.7        NA          NA
#> 5                    176           7                96.0        NA          NA
#> 6                    176           7                96.0        NA          NA
#>   fold_redundancy_pct compression_ratio remaining_potential
#> 1                  NA                NA                 4.0
#> 2                  NA                NA                11.0
#> 3                  NA                NA               448.0
#> 4                  NA                NA                 3.0
#> 5                  NA                NA                25.1
#> 6                  NA                NA                25.1
```
