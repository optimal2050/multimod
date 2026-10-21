# Two models from the field

``` r

library(multimod)
```

The toy model of the other articles fits on one screen. This article
runs two published models written independently of multimod: OSeMOSYS,
which is read from its public source, and the energyRt reference model,
which ships with multimod. Both are energy system models; neither was
written with multimod in mind. Chunks that need a solver or the OSeMOSYS
files run only where those are present; the numbers quoted in the text
are from a run with everything installed.

## OSeMOSYS

[OSeMOSYS](http://www.osemosys.org/), the Open Source Energy Modelling
System, is written in GNU MathProg. Its repository
[OSeMOSYS/OSeMOSYS_GNU_MathProg](https://github.com/OSeMOSYS/OSeMOSYS_GNU_MathProg)
(Apache License 2.0, copyright 2010-2015 OSeMOSYS Forum steering
committee) holds the model file `src/osemosys.txt` and the UTOPIA test
case `tests/utopia.txt`. Nothing of it is bundled with multimod; this
section reads a local clone whose path is in the environment variable
`MULTIMOD_OSEMOSYS_DIR`. The reference result for UTOPIA is the
objective glpsol reports on those two files, 29446.86.

The reference for OSeMOSYS is Howells, M., Rogner, H., Strachan, N.,
Heaps, C., Huntington, H., Kypreos, S., Hughes, A., Silveira, S.,
DeCarolis, J., Bazilian, M. and Roehrl, A. (2011). OSeMOSYS: The Open
Source Energy Modeling System. An introduction to its ethos, structure
and development. *Energy Policy*, 39(10), 5850-5870.
<https://doi.org/10.1016/j.enpol.2011.06.033>

``` r

ose <- read_gmpl(file.path(osemosys_dir, "src", "osemosys.txt"),
                 file.path(osemosys_dir, "tests", "utopia.txt"))
ose
```

The reader returns 11 sets, 61 parameters, 68 variables and 95
equations. Twenty-six parameters carry UTOPIA data; six are formulas
over other parameters (discount factors, the capital recovery factor,
the annuity), and the rest take their declared defaults, among them a
*parametric* default, `DiscountRateIdv` defaulting to `DiscountRate[r]`.

``` r

validate(ose, stop_on_error = FALSE)$valid
as_gams(ose$parameters$CapitalRecoveryFactor$formula$expr)
cat(as_latex(ose$equations$CAa1_TotalNewCapacity$rhs))
```

### JuMP

Formula parameters become Julia dictionaries computed from the data at
run time, and the parametric default becomes a nested
[`get()`](https://rdrr.io/r/base/get.html):

``` julia
# Each variable is declared over a full set product
@variable(model, RateOfDemand[REGION, TIMESLICE, FUEL, YEAR] >= 0)
@variable(model, Demand[REGION, TIMESLICE, FUEL, YEAR] >= 0)
@variable(model, StorageLevelYearStart[REGION, STORAGE, YEAR] >= 0)

@objective(model, Min, sum(TotalDiscountedCost[r, y] for r in REGION, y in YEAR))

optimize!(model)
println("Termination status: ", termination_status(model))
println("Objective value: ", objective_value(model))
```

``` r

ws <- tempfile("osemosys-")
dir.create(ws)
write_jump(ose, model_dir = ws)
data_jl <- readLines(file.path(ws, "solvers", "jump", "data.jl"))
cat(substr(grep("^CapitalRecoveryFactor = Dict", data_jl, value = TRUE), 1, 160), "...\n")
```

``` r

j <- solve_jump(ose, ws, verbose = FALSE)
j$objective
j$status
```

The objective matches glpsol’s 29446.86.

### Solved in R, from the matrix

The same model goes through the matrix route without any generated code.
OSeMOSYS declares every symbol over a set product with no domain
mapping, restricts 34 of its 95 equations with data-dependent
conditions, references lagged years (`y-1`, `y+1`) and defines six
parameters by formula; the matrix route expands the declared sets,
evaluates the conditions, resolves the lags and computes the formulas
itself.

``` r

h <- solve_highs(ose, attach = FALSE)
h$objective
nrow(build_row_index(ose))   # glpsol generates exactly this many rows
```

### Known limitations

Four storage-bracket equations (`SC2_*`, `SC4_*`) assemble with no
coefficients on this case; UTOPIA’s storage is inactive, so the
objective is unaffected, and
[`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md)
reports them in a warning.
[`write_gams()`](https://optimal2050.github.io/multimod/reference/write_gams.md)
output of this model is readable but not yet runnable without edits.
There is no Pyomo reader.

## energyRt

[energyRt](https://github.com/optimal2050/energyRt) is an R package for
energy system models that writes its own GAMS, MathProg, JuMP and Pyomo
code. `example_models$energyRt` holds one of its reference scenarios,
the “TOPIA” kit (seven regions, seasonal timeslices, a base horizon),
parsed by
[`read_gams()`](https://optimal2050.github.io/multimod/reference/read_gams.md)
from the GAMS template energyRt generates and populated with the
scenario’s data.

``` r

str(example_models$energyRt$metadata)
#> List of 8
#>  $ source          : chr "energyRt TOPIA kit"
#>  $ scenario        : chr "R7 / topia_seasons / base horizon"
#>  $ repository      : chr "https://github.com/optimal2050/energyRt"
#>  $ license         : chr "Apache-2.0"
#>  $ energyRt_version: chr "0.90.0.9012"
#>  $ gams_source     : chr "gams/energyRt.gms"
#>  $ date_imported   : Date[1:1], format: "2026-10-04"
#>  $ note            : chr "Model structure parsed from energyRt's GAMS template with read_gams(include = FALSE); data attached from an int"| __truncated__
```

The dataset is derived from energyRt, Apache License 2.0, copyright the
energyRt development team. The GAMS source is kept beside the parsed
model:

``` r

gms <- example_models$energyRt$gams$model
length(gms)
#> [1] 3005
i <- grep("^eqTechCap", gms)[1]
cat(gms[i:(i + 4)], sep = "\n")
#> eqTechCap(tech, region, year)        Technology capacity
#> eqTechCapLo(tech, region, year)      Technology capacity lower bound
#> eqTechCapUp(tech, region, year)      Technology capacity upper bound
#> eqTechNewCapLo(tech, region, year)   Lower bound on new capacity
#> eqTechNewCapUp(tech, region, year)   Upper bound on new capacity
m <- example_models$energyRt$multimod
m
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

### The matrix route

``` r

lp <- model_to_lp(m)
dim(lp$A)
#> [1] 19083 19218
r <- solve_highs(m, attach = FALSE)
r$objective
#> [1] 46398.42
```

The model has about nineteen thousand rows and columns.

### The code-generation routes

The same model goes through each generated language; the objective is
the one to compare.

``` r

ws <- tempfile("energyrt-")
dir.create(ws)
```

``` r

write_gmpl(m, model_dir = ws)
g <- solve_gmpl(m, ws, verbose = FALSE)
c(objective = g$objective, status = g$status)
#>          objective             status 
#> "46398.4234209741"          "OPTIMAL"
```

``` r

write_jump(m, model_dir = ws)
j <- solve_jump(m, ws, verbose = FALSE)
#> Loading solutions from: solvers/jump/solution
c(objective = j$objective, status = j$status)
#>          objective             status 
#> "46398.4234209742"          "OPTIMAL"
```

The generated Julia, one constraint:

``` r

jl <- readLines(file.path(ws, "solvers", "jump", "model.jl"))
i <- grep("@constraint", jl, fixed = TRUE)[1]
cat("```julia", jl[i:(i + 4)], "```", sep = "\n")
```

``` julia
@constraint(
    model,
    eqTechSng2Sng[(h, r, c, cp, y, t) in meqTechSng2Sng],
    vTechOut[(h, cp, r, y, t)] == get(pTechCinp2use, (h, c, r, y, t), pTechCinp2useDef) * get(pTechUse2cact, (h, cp, r, y, t), pTechUse2cactDef) * get(pTechCact2cout, (h, cp, r, y, t), pTechCact2coutDef) * vTechInp[(h, c, r, y, t)]
)
```

``` r

write_pyomo(m, model_dir = ws, export_vars = TRUE)
p <- solve_pyomo(m, ws, solver = "highs", verbose = FALSE, load_results = TRUE)
c(objective = p$objective, status = p$status$termination)
#>          objective             status 
#> "46398.4234209742"          "optimal"
```

With everything installed the four routes return 46398.42.

The GAMS source this model came from can also be regenerated:

``` r

out <- write_gams(m)
length(out)
#> [1] 2069
```

Any energyRt scenario enters the same way:
[`energyRt::multimod_from_energyRt()`](https://energyRt.org/reference/multimod_from_energyRt.html)
builds the model from an interpolated scenario, with the structure from
the GAMS template and the data from the scenario’s tables.
