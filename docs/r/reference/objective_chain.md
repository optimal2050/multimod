# Definition equations the objective depends on

Walks from the objective's variable through the definitions it refers
to. An equality whose left-hand side is a bare variable indexed by the
equation's own dimensions, the variable continuous with a lower bound of
0 or none and no upper bound, defines that variable. The walk enters a
defined variable when every other equation referring to it is itself a
definition (an accounting total), and not when its definition carries a
dimension named in `stop_dims`; a variable some constraint refers to,
such as a capacity, keeps its columns. Substituting the result while the
matrix is built
([`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.md)
with `stream = TRUE, substitute = `) writes the objective on the columns
the definitions resolve to instead of on one defined variable; the
constant part is kept beside the index tables and
[`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md)
adds it to the objective.

## Usage

``` r
objective_chain(model, stop_dims = NULL)
```

## Arguments

- model:

  A multimod model.

- stop_dims:

  Dimension names at which the walk stops, for example `"timeslice"` to
  keep per-period totals as rows; `NULL` follows every definition.

## Value

The equation names, each definition after the ones it refers to and the
objective's own definition last.

## Examples

``` r
objective_chain(example_models$energyRt$multimod, stop_dims = "timeslice")
#>  [1] "eqSupCost"         "eqTechEac"         "eqTechRetCost"    
#>  [4] "eqStorageRetCost"  "eqTradeRetCost"    "eqTechFixom"      
#>  [7] "eqTechVarom"       "eqStorageEac"      "eqStorageFixom"   
#> [10] "eqStorageVarom"    "eqImportRowCost"   "eqExportRowCost"  
#> [13] "eqTradeEac"        "eqTradeFixom"      "eqImportIrCost"   
#> [16] "eqExportIrCost"    "eqTaxCost"         "eqSubsCost"       
#> [19] "eqTotalUserCosts"  "eqDummyImportCost" "eqDummyExportCost"
#> [22] "eqCost"            "eqObjective"      
```
