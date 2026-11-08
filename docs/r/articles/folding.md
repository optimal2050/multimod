# Model redundancy

``` r

library(multimod)
mod <- example_models$energyRt$multimod
mod
#> Model: 
#> Source:  energyRt/gams/energyRt.gms 
#> Language:  GAMS 
#> Sets:  27 
#> Mappings:  324 
#> Aliases:  7 
#> Parameters:  218 
#> Variables:  93 
#> Equations:  153
```

A framework model such as energyRt declares each parameter over every
dimension it may vary by (technology, commodity, region, year,
timeslice) and carries equations for features a given scenario may not
use. Two transformations remove the resulting redundancy without
changing the optimum:

1.  **Folding** drops a dimension from a parameter whose values are
    constant across it (data redundancy).
2.  **Trimming** flags sets, parameters, mappings, variables and
    equations with no data or no instances (structural redundancy).

Both are reversible, and both are honoured by the MathProg, JuMP, Pyomo
and LaTeX writers and by the matrix route.

## Folding

A parameter with the same value in every timeslice is stored once per
remaining index combination, and the equations that use it lose the
folded index:

    # before
    vTechInp[t,c,r,y,s] >= pTechCinp2use[t,c,r,y,s] * vTechAct[t,r,y,s]
    # after
    vTechInp[t,c,r,y,s] >= pTechCinp2use[t,c,r,y]   * vTechAct[t,r,y,s]

### Redundancy in the data

[`analyze_parameter_redundancy()`](https://optimal2050.github.io/multimod/r/reference/analyze_parameter_redundancy.md)
counts, per parameter, the rows that repeat a value.

``` r

red <- analyze_parameter_redundancy(mod, verbose = FALSE)
red <- red[order(-red$orig_redundancy_pct), ]
knitr::kable(
  head(red[, c("param_name", "dims", "orig_rows", "orig_unique",
               "orig_redundancy_pct")], 8),
  row.names = FALSE,
  caption = "Parameters with the most repeated values (rows; redundancy in %)"
)
```

| param_name | dims | orig_rows | orig_unique | orig_redundancy_pct |
|:---|:---|---:|---:|---:|
| pTechCinp2use | tech, comm, region, year, timeslice | 1344 | 3 | 99.8 |
| pTradeIrEff | trade, region, region, year, timeslice | 576 | 1 | 99.8 |
| pTechAfLo | tech, region, year, timeslice | 336 | 1 | 99.7 |
| pStorageInpEff | stg, comm, region, year, timeslice | 336 | 1 | 99.7 |
| pStorageOutEff | stg, comm, region, year, timeslice | 336 | 1 | 99.7 |
| pTimesliceAgg | year, timeslice, timeslice | 180 | 1 | 99.4 |
| pTimesliceWeight | year, timeslice | 68 | 1 | 98.5 |
| pTradeInvcost | trade, region, year | 48 | 1 | 97.9 |

Parameters with the most repeated values (rows; redundancy in %)
{.table}

### Fold specification

[`create_fold_spec()`](https://optimal2050.github.io/multimod/r/reference/create_fold_spec.md)
takes one entry per dimension to fold, naming the *validation mappings*
that state which entities may vary by that dimension; a parameter folds
only when its values are constant within every group the mapping allows.
In energyRt these are `mTechTimeslice` for technologies and
`mCommTimeslice` for commodities.

``` r

fold_spec <- create_fold_spec(
  mod,
  fold_dims = list(
    timeslice = list(tech = "mTechTimeslice", comm = "mCommTimeslice")
  ),
  verbose = FALSE
)
knitr::kable(
  head(fold_spec[, c("param_name", "validation_mapping", "can_fold", "unproven")], 8),
  row.names = FALSE,
  caption = "Fold specification, first rows"
)
```

| param_name       | validation_mapping | can_fold | unproven |
|:-----------------|:-------------------|:---------|:---------|
| pTechCinp2use    | mTechTimeslice     | TRUE     | FALSE    |
| pTechAfLo        | mTechTimeslice     | TRUE     | FALSE    |
| pSupCost         | mCommTimeslice     | TRUE     | FALSE    |
| pTimesliceWeight | NA                 | TRUE     | FALSE    |
| pTimesliceAgg    | NA                 | FALSE    | FALSE    |
| pStorageInpEff   | mCommTimeslice     | TRUE     | FALSE    |
| pStorageOutEff   | mCommTimeslice     | TRUE     | FALSE    |
| pTradeIrEff      | NA                 | FALSE    | FALSE    |

Fold specification, first rows {.table}

`unproven` marks parameters without a validation mapping for their
entity type. Several dimensions are folded in the order given:

``` r

create_fold_spec(
  mod,
  fold_dims = list(
    timeslice = list(tech = "mTechTimeslice", comm = "mCommTimeslice"),
    region    = list(comm = "mCommRegion")
  )
)
```

### Fold and unfold

``` r

mod_folded <- fold_model(mod, fold_spec = fold_spec, verbose = FALSE)
c(original = is_folded(mod), folded = is_folded(mod_folded))
#> original   folded 
#>    FALSE     TRUE
fs <- get_fold_summary(mod_folded, format = "data.frame")
knitr::kable(
  fs[, c("parameter", "removed_dims", "original_rows", "folded_rows", "compression")],
  row.names = FALSE,
  caption = "Folded parameters (rows; compression = original / folded rows)"
)
```

| parameter        | removed_dims | original_rows | folded_rows | compression |
|:-----------------|:-------------|--------------:|------------:|:------------|
| pTechCinp2use    | timeslice    |          1344 |         112 | 12.0x       |
| pTechAfLo        | timeslice    |           336 |          28 | 12.0x       |
| pSupCost         | timeslice    |            76 |          76 | 1.0x        |
| pTimesliceWeight | timeslice    |            68 |           4 | 17.0x       |
| pStorageInpEff   | timeslice    |           336 |          28 | 12.0x       |
| pStorageOutEff   | timeslice    |           336 |          28 | 12.0x       |

Folded parameters (rows; compression = original / folded rows) {.table}

A folded parameter keeps its declared dimensions in `$dims`;
[`get_active_dims()`](https://optimal2050.github.io/multimod/r/reference/get_active_dims.md)
returns the ones in force.

``` r

p <- mod_folded$parameters$pTechCinp2use
p$dims
#> <AST dims> 
#> [tech,comm,region,year,timeslice]
get_active_dims(p)
#> <AST dims> 
#> [tech,comm,region,year]
```

[`unfold_model()`](https://optimal2050.github.io/multimod/r/reference/unfold_model.md)
restores the original data:

``` r

mod_unfolded <- unfold_model(mod_folded, verbose = FALSE)
c(original = nrow(mod$parameters$pTechCinp2use$data),
  unfolded = nrow(mod_unfolded$parameters$pTechCinp2use$data))
#> original unfolded 
#>     1344     1344
```

## Trimming

Trimming flags with `$trimmed = TRUE`, and deletes nothing:

- sets, parameters and mappings without data;
- variables over an empty domain;
- equations with an empty domain or only trimmed variables.

Anything a remaining equation still uses is then unflagged, so no
element a surviving equation needs is trimmed. Folding comes first,
since it can empty a parameter.

``` r

mod_trimmed <- trim_model(mod_folded, verbose = FALSE)
knitr::kable(
  get_trim_summary(mod_trimmed, format = "data.frame"),
  row.names = FALSE,
  caption = "Elements trimmed, by type (counts)"
)
```

| element_type | total | trimmed | remaining | percent_trimmed |
|:-------------|------:|--------:|----------:|----------------:|
| sets         |    27 |       6 |        21 |            22.2 |
| parameters   |   218 |       1 |       217 |             0.5 |
| mappings     |   324 |     160 |       164 |            49.4 |
| variables    |    93 |      10 |        83 |            10.8 |
| equations    |   153 |     103 |        50 |            67.3 |

Elements trimmed, by type (counts) {.table}

[`is_trimmed()`](https://optimal2050.github.io/multimod/r/reference/is_trimmed.md)
applies to a model or to one element;
[`untrim_model()`](https://optimal2050.github.io/multimod/r/reference/untrim_model.md)
clears the flags.

``` r

trimmed_eqs <- get_trim_summary(mod_trimmed, format = "list")$equations$names
head(trimmed_eqs)
#> [1] "eqTechGrp2Sng"    "eqTechSng2Grp"    "eqTechGrp2Grp"    "eqTechShareInpLo"
#> [5] "eqTechShareInpUp" "eqTechShareOutLo"
is_trimmed(mod_trimmed$equations$eqTechGrp2Sng)
#> [1] TRUE
is_trimmed(mod_trimmed$equations$eqTechCap)
#> [1] FALSE
is_trimmed(untrim_model(mod_trimmed))
#> [1] FALSE
```

## Folding and trimming compared

|  | Folding | Trimming |
|----|----|----|
| Targets | data redundancy | structural redundancy |
| Mechanism | collapses a constant dimension | flags unused elements |
| Reduces | parameter rows, coefficients | sets, parameters, variables, equations emitted |
| Needs | validation mappings | nothing; driven by the data |
| Reversed by | [`unfold_model()`](https://optimal2050.github.io/multimod/r/reference/unfold_model.md) | [`untrim_model()`](https://optimal2050.github.io/multimod/r/reference/untrim_model.md) |

## Writing a reduced model

[`write_gmpl()`](https://optimal2050.github.io/multimod/r/reference/write_gmpl.md),
[`write_jump()`](https://optimal2050.github.io/multimod/r/reference/write_jump.md)
and
[`write_pyomo()`](https://optimal2050.github.io/multimod/r/reference/write_pyomo.md)
use the folded dimensions and skip trimmed elements;
[`write_latex()`](https://optimal2050.github.io/multimod/r/reference/write_latex.md)
shows the reduced model, or the full one with the removed parts marked
(`model_view = "full"`);
[`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md)
builds the matrix from the active dimensions.

``` r

dir <- file.path(tempdir(), "trimmed")
save_model(mod_trimmed, dir, format = "csv")
write_gmpl(mod_trimmed, model_dir = dir)
write_jump(mod_trimmed, model_dir = dir)
```

A fold specification is a data frame; saved with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html), it applies to other
scenarios of the same model.

## Limitations

- A parameter folds only when every group is constant across the
  dimension.
- Folding needs validation mappings; for a model other than energyRt,
  the specification names them.
- Folding holds the parameter data in memory while it runs.
- Trimming only flags elements; their data stays in the model and on
  disk.

## The optimum is unchanged

``` r

r0 <- solve_highs(mod, attach = FALSE)
r1 <- solve_highs(mod_folded, attach = FALSE)
r2 <- solve_highs(mod_trimmed, attach = FALSE)
c(original = r0$objective, folded = r1$objective, trimmed = r2$objective)
#> original   folded  trimmed 
#> 46398.42 46398.42 46398.42
```
