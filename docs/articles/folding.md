# Folding and trimming

``` r

library(multimod)
mod <- example_models$energyRt$multimod
mod
#> Model: 
#> Source:  C:/Users/admin/Documents/R/energyRt/gams/energyRt.gms 
#> Language:  GAMS 
#> Sets:  27 
#> Mappings:  324 
#> Aliases:  7 
#> Parameters:  218 
#> Variables:  93 
#> Equations:  153
```

## The flexibility-performance tradeoff

Framework models such as energyRt are written for maximum flexibility:
one generic set of equations covers technologies, storage, trade,
emissions and more, and every parameter is indexed over every dimension
it could ever vary by (technology, commodity, region, year, timeslice).
A given scenario uses only part of that:

- a single-region model has no interregional trade,
- most technologies have the same efficiency in every timeslice,
- features the scenario does not use leave sets and mappings empty, and
  the equations that depend on them generate no constraints.

The flexibility is paid for at model-build time and in the solver: data
that is repeated across a dimension inflates the data files and the
coefficient count, and equations with empty domains are still processed.
`multimod` offers two complementary transformations that remove this
redundancy without changing the optimum:

1.  **Folding** removes a dimension from a parameter when the values are
    constant across it (data redundancy).
2.  **Trimming** marks sets, parameters, mappings, variables and
    equations that have no data or no instances, so code generators skip
    them (structural redundancy).

Both are reversible and both are honoured by every writer and by the
direct matrix route.

## Folding

### The idea

Take a 5-dimensional parameter whose values do not depend on the
timeslice:

    pTechCinp2use[E01, ELC, R1, 2025, s1] = 0.95
    pTechCinp2use[E01, ELC, R1, 2025, s2] = 0.95
    ...

Folding drops the `timeslice` dimension and keeps one row per remaining
index combination. Equations that reference the parameter are rewritten
to omit the folded index:

    # before
    vTechInp[t,c,r,y,s] >= pTechCinp2use[t,c,r,y,s] * vTechAct[t,r,y,s]
    # after
    vTechInp[t,c,r,y,s] >= pTechCinp2use[t,c,r,y]   * vTechAct[t,r,y,s]

### Diagnose redundancy

[`analyze_parameter_redundancy()`](https://optimal2050.github.io/multimod/reference/analyze_parameter_redundancy.md)
reports, per parameter, how many rows carry a repeated value. High
redundancy marks a folding candidate.

``` r

red <- analyze_parameter_redundancy(mod, verbose = FALSE)
red <- red[order(-red$orig_redundancy_pct), ]
head(red[, c("param_name", "dims", "orig_rows", "orig_unique",
             "orig_redundancy_pct")], 8)
#>          param_name                                   dims orig_rows
#> 3     pTechCinp2use    tech, comm, region, year, timeslice      1344
#> 30      pTradeIrEff trade, region, region, year, timeslice       576
#> 8         pTechAfLo          tech, region, year, timeslice       336
#> 23   pStorageInpEff     stg, comm, region, year, timeslice       336
#> 24   pStorageOutEff     stg, comm, region, year, timeslice       336
#> 20    pTimesliceAgg             year, timeslice, timeslice       180
#> 19 pTimesliceWeight                        year, timeslice        68
#> 35    pTradeInvcost                    trade, region, year        48
#>    orig_unique orig_redundancy_pct
#> 3            3                99.8
#> 30           1                99.8
#> 8            1                99.7
#> 23           1                99.7
#> 24           1                99.7
#> 20           1                99.4
#> 19           1                98.5
#> 35           1                97.9
```

### Build a fold specification

[`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md)
takes `fold_dims`, a named list: one entry per dimension to fold, whose
value names the *validation mappings* to check against. For an energyRt
model the technology parameters are validated against `mTechTimeslice`
and the commodity parameters against `mCommTimeslice`:

``` r

fold_spec <- create_fold_spec(
  mod,
  fold_dims = list(
    timeslice = list(tech = "mTechTimeslice", comm = "mCommTimeslice")
  ),
  verbose = FALSE
)
fold_spec[, c("param_name", "validation_mapping", "can_fold", "unproven")]
#>         param_name validation_mapping can_fold unproven
#> 1    pTechCinp2use     mTechTimeslice     TRUE    FALSE
#> 2        pTechAfLo     mTechTimeslice     TRUE    FALSE
#> 3         pSupCost     mCommTimeslice     TRUE    FALSE
#> 4 pTimesliceWeight               <NA>     TRUE    FALSE
#> 5    pTimesliceAgg               <NA>    FALSE    FALSE
#> 6   pStorageInpEff     mCommTimeslice     TRUE    FALSE
#> 7   pStorageOutEff     mCommTimeslice     TRUE    FALSE
#> 8      pTradeIrEff               <NA>    FALSE    FALSE
#> 9  pImportRowPrice               <NA>    FALSE     TRUE
```

The result is a data frame with one row per parameter that carries the
dimension. `can_fold` is `TRUE` when the values are constant across the
dimension in every group formed by the other indices. `unproven` flags
parameters that failed only because no validation mapping was found for
their entity type; supplying one (for example `sup = "mSupTimeslice"`)
would let them be checked.

Why validate against a mapping? A parameter may be constant across the
timeslice *in the data* only because the scenario never varied it. The
mapping says which entities are allowed to vary by that dimension, so
folding stays consistent with the model structure, not just with one
dataset. A value of `c("mStorageComm", "mCommTimeslice")` chains two
mappings when the entity is linked to the dimension indirectly.

Several dimensions can be folded in one specification; they are applied
in the order given, each on the result of the previous one:

``` r

create_fold_spec(
  mod,
  fold_dims = list(
    timeslice = list(tech = "mTechTimeslice", comm = "mCommTimeslice"),
    region    = list(comm = "mCommRegion")
  )
)
```

### Apply the fold

``` r

mod_folded <- fold_model(mod, fold_spec = fold_spec, verbose = FALSE)
is_folded(mod)
#> [1] FALSE
is_folded(mod_folded)
#> [1] TRUE
```

[`fold_model()`](https://optimal2050.github.io/multimod/reference/fold_model.md)
collapses the data of every parameter with `can_fold = TRUE`, rewrites
each equation that references one of them, and records what it did in
`model$misc$fold_summary`.
[`get_fold_summary()`](https://optimal2050.github.io/multimod/reference/get_fold_summary.md)
reads that record:

``` r

get_fold_summary(mod_folded, format = "data.frame")[
  , c("parameter", "removed_dims", "original_rows", "folded_rows", "compression")]
#>          parameter removed_dims original_rows folded_rows compression
#> 1    pTechCinp2use    timeslice          1344         112       12.0x
#> 2        pTechAfLo    timeslice           336          28       12.0x
#> 3         pSupCost    timeslice            76          76        1.0x
#> 4 pTimesliceWeight    timeslice            68           4       17.0x
#> 5   pStorageInpEff    timeslice           336          28       12.0x
#> 6   pStorageOutEff    timeslice           336          28       12.0x
```

With `format = "text"` the same information is printed as a report;
`format = "list"` returns it as nested lists.

### Inspect a folded parameter

[`is_folded()`](https://optimal2050.github.io/multimod/reference/is_folded.md)
also works on a single parameter, and
[`get_active_dims()`](https://optimal2050.github.io/multimod/reference/get_active_dims.md)
returns the dimensions in force after folding; the declaration stays in
`$dims`.

``` r

p <- mod_folded$parameters$pTechCinp2use
is_folded(p)
#> [1] TRUE
p$dims              # as declared
#> <AST dims> 
#> [tech,comm,region,year,timeslice]
get_active_dims(p)  # in force after folding
#> <AST dims> 
#> [tech,comm,region,year]

# parameters the fold did not touch keep their declaration
is_folded(mod_folded$parameters$pTechInvcost)
#> [1] FALSE
```

The fold metadata lives in `p$misc$fold_info`:

``` r

p$misc$fold_info[c("original_dims", "removed_dims", "original_rows",
                   "folded_rows", "compression_ratio")]
#> $original_dims
#> [1] "tech"      "comm"      "region"    "year"      "timeslice"
#> 
#> $removed_dims
#> [1] "timeslice"
#> 
#> $original_rows
#> [1] 1344
#> 
#> $folded_rows
#> [1] 112
#> 
#> $compression_ratio
#> [1] 12
```

### Unfold

[`unfold_model()`](https://optimal2050.github.io/multimod/reference/unfold_model.md)
restores the original data and dimensions of every folded parameter:

``` r

mod_unfolded <- unfold_model(mod_folded, verbose = FALSE)
nrow(mod_unfolded$parameters$pTechCinp2use$data)
#> [1] 1344
nrow(mod$parameters$pTechCinp2use$data)
#> [1] 1344
```

### Aliases and repeated dimensions

A parameter can be declared with one dimension name while its data uses
an alias column (`comm` declared, `acomm` in the data), or a dimension
can appear twice in different roles (`{region, region}` stored as `src`,
`dst`). Folding resolves the first through `model$aliases` and keeps
every data column other than the folded one for the second, so both
cases fold without special handling.

## Trimming

### The idea

A scenario rarely exercises every feature of a framework model. Elements
that end up without data or without instances are:

- **empty sets and mappings** (no storage technologies, no trade
  routes),
- **empty parameters**,
- **variables** indexed by an empty set,
- **equations** whose domain is empty, or that only reference trimmed
  variables.

Trimming marks these with `$trimmed = TRUE`. Nothing is deleted; code
generators and
[`write_latex()`](https://optimal2050.github.io/multimod/reference/write_latex.md)
check the flag and skip (or annotate) the element.

### Four phases

    1. mark sets, parameters and mappings with no data
    2. mark variables whose domain is empty
    3. mark equations with an empty domain, or with only trimmed variables
    4. untrim anything a non-trimmed equation still uses

Phase 4 is what keeps trimming conservative: an element is only trimmed
when no surviving equation needs it.

### Apply the trim

Fold first, then trim: folding can empty a parameter that trimming then
removes.

``` r

mod_trimmed <- trim_model(mod_folded, verbose = FALSE)
is_trimmed(mod)
#> [1] FALSE
is_trimmed(mod_trimmed)
#> [1] TRUE
get_trim_summary(mod_trimmed, format = "data.frame")
#>   element_type total trimmed remaining percent_trimmed
#> 1         sets    27       6        21            22.2
#> 2   parameters   218       1       217             0.5
#> 3     mappings   324     160       164            49.4
#> 4    variables    93      10        83            10.8
#> 5    equations   153     103        50            67.3
```

The example scenario uses a small share of the energyRt equations, so
most of them trim away. As with folding, `format = "text"` prints a
report and `format = "list"` returns nested lists.
[`is_trimmed()`](https://optimal2050.github.io/multimod/reference/is_trimmed.md)
works on individual elements too:

``` r

is_trimmed(mod_trimmed$equations$eqTechCap)
#> [1] FALSE
is_trimmed(mod_trimmed$equations$eqTechInpTot)
#> [1] FALSE
head(get_trim_summary(mod_trimmed, format = "list")$equations$names)
#> [1] "eqTechGrp2Sng"    "eqTechSng2Grp"    "eqTechGrp2Grp"    "eqTechShareInpLo"
#> [5] "eqTechShareInpUp" "eqTechShareOutLo"
```

### Untrim

[`untrim_model()`](https://optimal2050.github.io/multimod/reference/untrim_model.md)
clears the flags, on all element types by default or on the ones named
in `elements`:

``` r

mod_back <- untrim_model(mod_trimmed)
is_trimmed(mod_back)
#> [1] FALSE
```

## Folding versus trimming

|  | Folding | Trimming |
|----|----|----|
| Targets | data redundancy | structural redundancy |
| Mechanism | collapses a constant dimension | flags unused elements |
| Reduces | parameter rows, coefficient count | sets, parameters, variables, equations emitted |
| Needs | a fold specification (validation mappings) | nothing; driven by the data |
| Reversed by | [`unfold_model()`](https://optimal2050.github.io/multimod/reference/unfold_model.md) | [`untrim_model()`](https://optimal2050.github.io/multimod/reference/untrim_model.md) |

**Fold** when parameters are constant across a dimension the scenario
does not need (timeslice is the usual case), when data files or solver
memory are the bottleneck, or when a model written for many regions or
years is run with one.

**Trim** when the scenario leaves optional features unused, when empty
sets or parameters exist, or when many equations generate no
constraints.

**Do both** for framework models: fold first, then trim. Check that the
objective is unchanged before relying on a transformed model (see the
appendix).

Folding and trimming change what the generated model *contains*; they do
not change what it *means*. Do not fold a dimension that genuinely
varies in the data
([`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md)
refuses, but a hand-edited specification can override it), and keep an
untrimmed copy when the complete structure is wanted for documentation
or comparison.

## Writing transformed models

Every writer honours the flags.
[`write_gmpl()`](https://optimal2050.github.io/multimod/reference/write_gmpl.md),
[`write_jump()`](https://optimal2050.github.io/multimod/reference/write_jump.md)
and
[`write_pyomo()`](https://optimal2050.github.io/multimod/reference/write_pyomo.md)
use the folded dimensions by default and skip trimmed elements;
[`write_latex()`](https://optimal2050.github.io/multimod/reference/write_latex.md)
chooses `model_view = "reduced"` for a folded or trimmed model and
annotates the removed elements in `model_view = "full"`.
[`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md)
and
[`solve_highs()`](https://optimal2050.github.io/multimod/reference/solve_highs.md)
assemble the matrix from the active dimensions directly.

``` r

dir <- file.path(tempdir(), "trimmed")
save_model(mod_trimmed, dir, format = "ipc")
write_gmpl(mod_trimmed, model_dir = dir)
write_jump(mod_trimmed, model_dir = dir)
write_latex(mod_trimmed, file = file.path(dir, "model.tex"), model_view = "full")
```

A fold specification is an ordinary data frame; save it with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) to reproduce the
transformation on another scenario of the same model.

## Limitations

- Folding is all-or-nothing per parameter: either every group is
  constant across the dimension or the parameter is left alone.
- Folding needs validation mappings that reflect the model’s structure;
  for a non-energyRt model, name the mappings that link each entity to
  the dimension.
- Folding loads the parameter data into memory while it runs.
- Trimming only flags elements; the data stays on disk and in the
  object.
- Trimming is conservative by design: it never removes an element a
  surviving equation references, so unused variables inside non-empty
  equations are kept.

## Appendix: the objective is unchanged

The transformed models must solve to the same objective as the original.
The check below uses the in-process HiGHS route; it is not run when the
vignette is built because it needs the `highs` package and takes a few
seconds.

``` r

r0 <- solve_highs(mod, attach = FALSE)
r1 <- solve_highs(mod_folded, attach = FALSE)
r2 <- solve_highs(mod_trimmed, attach = FALSE)
c(original = r0$objective, folded = r1$objective, trimmed = r2$objective)
all.equal(r0$objective, r1$objective)
all.equal(r0$objective, r2$objective)
```
