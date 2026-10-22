# Solving and reading results

``` r

library(multimod)
```

The [Writing a model in
R](https://optimal2050.github.io/multimod/r/articles/ast.md) article
builds a model in R. This article takes that model to a solution and
back: what the model object contains once it has data, where its files
go, how multimod turns it into a matrix or into code for another
toolchain, how to run that code, how the results come back, and what to
look at when two routes disagree. The example is `example_models$toy`,
three technologies supplying two regions. The chunks that need a solver
run where it is installed; the text quotes the numbers from a run with
all of them present.

## The model as an object

``` r

toy <- example_models$toy
toy
#> Model: toy
#> Description: two-region capacity expansion toy model
#> Language:  multimod 
#> Sets:  2 
#> Mappings:  2 
#> Aliases:  0 
#> Parameters:  4 
#> Variables:  3 
#> Equations:  3
```

A model is a list of named components. It holds two kinds of content,
which are best kept apart:

- **Declarations** say what exists. A parameter has dimensions, a
  variable has dimensions and a *domain*, an equation has sides, a
  relation and a domain. Rendering to any language only needs
  declarations.
- **Data** says how much of it exists. Sets carry their members,
  mappings their tuples, parameters their values. Solving needs data.

The domain connects the two. A variable declared over
`c("tech", "region")` with domain `"mTR"` has one instance per row of
the mapping `mTR`, not one per combination of the sets. That is how a
sparse model stays sparse, and it is also the first thing to check when
an equation produces no rows.

``` r

toy$variables$vGen$domain
#> [1] "mTR"
nrow(toy$mappings$mTR$data)
#> [1] 6
toy$equations$eqDemand$domain$name
#> [1] "mR"
toy$parameters$pDemand$data
#>   region value
#> 1  north  1000
#> 2  south   600
```

## The workspace

Everything written for a solver goes into one directory, the
*workspace*. Its layout is fixed so that the generated code can find the
data and so that results land where multimod expects to read them:

    <model_dir>/
      model.rds, metadata.json, format.txt    the structure and the data format
      sets/<name>/data.csv                    one file per set
      mappings/<name>/data.csv                one file per mapping
      parameters/<name>/data.csv              one file per parameter
      solvers/gmpl/   model.mod, data.dat     MathProg, written by write_gmpl()
      solvers/jump/   model.jl,  data.jl      JuMP,     written by write_jump()
      solvers/pyomo/  model.py,  data.py      Pyomo,    written by write_pyomo()
      solvers/<backend>/solution/             what the solver run leaves behind
      variables/<name>/data.csv               solutions loaded back into the model

[`save_model()`](https://optimal2050.github.io/multimod/r/reference/save_model.md)
creates a workspace explicitly (see “Model workspace and data” for
formats and lazy loading). The writers create one themselves when the
directory they are given is empty, which is what this article does:

``` r

ws <- tempfile("toy-")
dir.create(ws)
```

## Solving in R

The matrix route does not generate code.
[`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md)
walks the equations and the data and assembles the LP arrays directly.

``` r

lp <- model_to_lp(toy)
names(lp)
#>  [1] "A"                "obj"              "row_lo"           "row_up"          
#>  [5] "col_lo"           "col_up"           "maximum"          "objective_symbol"
#>  [9] "row_index"        "col_index"        "zeroed"
dim(lp$A)
#> [1]  9 13
```

Each equation contributes one row per tuple of its domain mapping: six
for `eqCapLim` (every tuple of `mTR`), two for `eqDemand` (every region
in `mR`), one for the scalar `eqCost`. Each variable contributes one
column per tuple of its domain: six, six, and one for `vCost`. The two
index tables are the dictionary between matrix positions and model
symbols, and every result table is built on them:

``` r

lp$row_index
#>        i   symbol     i1     i2  sense   rhs
#>    <int>   <char> <char> <char> <char> <num>
#> 1:     1 eqCapLim   coal  north     <=    NA
#> 2:     2 eqCapLim    gas  north     <=    NA
#> 3:     3 eqCapLim   wind  north     <=    NA
#> 4:     4 eqCapLim   coal  south     <=    NA
#> 5:     5 eqCapLim    gas  south     <=    NA
#> 6:     6 eqCapLim   wind  south     <=    NA
#> 7:     7 eqDemand  north   <NA>     >=    NA
#> 8:     8 eqDemand  south   <NA>     >=    NA
#> 9:     9   eqCost   <NA>   <NA>     ==    NA
lp$col_index[c(1, 7, 13), ]
#>        j symbol     i1     i2    lo    up      vtype
#>    <int> <char> <char> <char> <num> <num>     <char>
#> 1:     1   vCap   coal  north     0   Inf   positive
#> 2:     7   vGen   coal  north     0   Inf   positive
#> 3:    13  vCost   <NA>   <NA>  -Inf   Inf continuous
lp$objective_symbol
#> [1] "vCost"
```

`row_lo`/`row_up` and `col_lo`/`col_up` hold the bounds; an equality row
has both equal, a `<=` row has `-Inf` below. Before solving a large
model,
[`check_matrix_numbers()`](https://optimal2050.github.io/multimod/r/reference/check_matrix_numbers.md)
reports the magnitude spectrum of the coefficients, right-hand sides and
bounds. Coefficients spanning more than about eight orders of magnitude,
or infinite entries, are the usual cause of a solver reporting
“unbounded” or “infeasible” for a model that is neither, and the
`offenders_*` tables name the equations and variables responsible.

``` r

cm <- check_matrix_numbers(lp)
#>    section entries zeros nonfinite min_abs max_abs orders below_small
#>    COLUMNS      29     0         0    0.25     120    2.7           0
#>  OBJECTIVE       1     0         0    1.00       1    0.0           0
#>        RHS      10     8         0  600.00    1000    0.2           0
#>     BOUNDS       0     0         0      NA      NA     NA           0
#>  above_large
#>            0
#>            0
#>            0
#>            0
names(cm)
#>  [1] "summary"           "decades"           "offenders_row"    
#>  [4] "offenders_col"     "nonfinite_row"     "nonfinite_col"    
#>  [7] "structure"         "empty_col_symbols" "empty_row_symbols"
#> [10] "small"             "large"
```

[`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.md)
passes the arrays to the HiGHS solver in the ‘highs’ package.
`attach = FALSE` returns the result as a list rather than storing it in
the model.

``` r

r <- solve_highs(toy, attach = FALSE)
r$objective
#> [1] 198000
r$status_message
#> [1] "Optimal"
```

`status` is HiGHS’s model-status code and `status_message` its name; 7
is “Optimal”. The primal table is the column index with the solution and
the reduced cost added, the dual table the row index with the row
activity and the marginal. Here the marginal on `eqDemand` is the cost
of one more unit of demand in each region:

``` r

r$primal[r$primal$value > 0, c("symbol", "i1", "i2", "value", "reduced_cost")]
#>    symbol     i1     i2       value reduced_cost
#>    <char> <char> <char>       <num>        <num>
#> 1:   vCap   wind  north   1111.1111            0
#> 2:   vCap   coal  south    666.6667            0
#> 3:   vGen   wind  north   1000.0000            0
#> 4:   vGen   coal  south    600.0000            0
#> 5:  vCost   <NA>   <NA> 198000.0000            0
r$dual[r$dual$symbol == "eqDemand", c("symbol", "i1", "value", "marginal")]
#>      symbol     i1 value marginal
#>      <char> <char> <num>    <num>
#> 1: eqDemand  north  1000 100.0000
#> 2: eqDemand  south   600 163.3333
```

The arrays can also leave R:
[`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.md)
writes them as an MPS file for any solver that reads one, and
[`solve_mps()`](https://optimal2050.github.io/multimod/r/reference/solve_mps.md)
solves such a file locally with HiGHS and writes a solution file
[`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md)
understands.
[`mm_session()`](https://optimal2050.github.io/multimod/r/reference/mm_session.md)
keeps the assembled matrix between repeated solves of the same structure
with changed data.

## Generating code

The code-generation routes render the model in another language and
leave the solve to that language’s toolchain. MathProg is shown first
and in full, because the generated model is short; the JuMP and Pyomo
code is described by how it differs from it.

### MathProg, in full

``` r

f <- write_gmpl(toy, model_dir = ws)
mod <- readLines(f$model)
cat(mod[1:grep("^solve;", mod)], sep = "\n")
#> # Model: toy
#> # two-region capacity expansion toy model
#> 
#> set tech;  # generation technologies
#> set region;  # model regions
#> 
#> 
#> set _IF_;  # Singleton set for GAMS $ operator emulation
#> 
#> set mTR dimen 2;  # technology x region pairs
#> set mR dimen 1;  # regions
#> 
#> param pCapCost{tech};  # annualized capacity cost, EUR/kW
#> param pVarCost{tech};  # variable cost, EUR/MWh
#> param pAvail{tech,region};  # availability factor
#> param pDemand{region};  # annual demand, MWh
#> 
#> var vCap{t in tech, r in region} >= 0;  # installed capacity, kW
#> var vGen{t in tech, r in region} >= 0;  # generation, MWh
#> 
#> minimize vCost: sum{t in tech, r in region} (pCapCost[t] * vCap[t,r] + pVarCost[t] * vGen[t,r]);
#> 
#> # generation is limited by available capacity
#> s.t. eqCapLim{(h,r) in mTR}: vGen[h,r] <= pAvail[h,r] * vCap[h,r];
#> 
#> # demand is met in every region
#> s.t. eqDemand{r in mR}: sum{t in tech} vGen[t,r] >= pDemand[r];
#> 
#> 
#> solve;
```

Reading top to bottom:

- Sets are declared without members, mappings as sets of tuples with
  their dimension (`dimen 2`). Members and tuples come from the data
  file.
- `_IF_` is a one-element set multimod uses to express GAMS-style `$`
  conditions as MathProg set filters; the toy has no conditions, so it
  is declared and not used.
- Parameters are declared with their index sets only; values come from
  the data file.
- Variables are declared over the full set product with iterator names
  taken from the model’s index aliases (`t`, `r`). Equations are
  declared over their *domain mapping*, `{(h,r) in mTR}`, with the
  iterator names the equation itself carries. This is where the domain
  shows up in generated code: a tuple absent from `mTR` yields no
  constraint.
- The objective equation `eqCost` is not written as a constraint. The
  objective statement takes its right-hand side, because
  `toy$objectives` names `eqCost` as the equation defining `vCost`.

After `solve;` the file continues with `printf` statements, in two
groups. The first writes `constraint_stats.csv` and
`variable_stats.csv`, one line per equation and variable with the number
of instances. The second writes the solution: one CSV per variable with
the index columns and the value, filtered to non-zero entries, plus a
log. These files are what
[`solve_gmpl()`](https://optimal2050.github.io/multimod/r/reference/solve_gmpl.md)
reads back.

``` r

cat(mod[grep("^printf \"tech,region,value", mod)[1] + 0:3], sep = "\n")
#> printf "tech,region,value\n" > "solution/vCap.csv";
#> for{t in tech, r in region : vCap[t,r] <> 0} {
#>   printf "%s,%s,%f\n", t,r,vCap[t,r] >> "solution/vCap.csv";
#> }
```

The data file is a MathProg data section:

``` r

dat <- readLines(f$data)
cat(dat[grep("^param pAvail", dat) + 0:7], sep = "\n")
#> param pAvail default 0 := 
#> [coal,north] 0.84999999999999998
#> [gas,north] 0.84999999999999998
#> [wind,north] 0.90000000000000002
#> [coal,south] 0.90000000000000002
#> [gas,south] 0.34999999999999998
#> [wind,south] 0.25;
```

Two rules of the format matter when a model misbehaves. MathProg has no
implicit default: a parameter referenced for a tuple that is not in the
data is an error unless the declaration carries `default`, so every
parameter is written with one (the model’s `defVal`, else 0). And every
declared set, mapping and parameter must appear in the data section even
when it is empty, so an empty mapping is written as `set name := ;`. A
model whose data lives on disk is written one parameter at a time, so
the data file for a large model never requires the whole model in
memory.

### JuMP

[`write_jump()`](https://optimal2050.github.io/multimod/r/reference/write_jump.md)
writes two files. `model.jl` holds the declarations and the constraints;
`data.jl` loads the data from the workspace at run time, so data can
change without regenerating the model.

``` r

jl <- write_jump(toy, model_dir = ws)
cat(grep("^@variable|^@objective", jl, value = TRUE), sep = "\n")
#> @variable(model, vCap[mTR] >= 0)  # installed capacity, kW
#> @variable(model, vGen[mTR] >= 0)  # generation, MWh
#> @objective(model, Min, sum(get(pCapCost, h, pCapCostDef) * vCap[(h, r)] + get(pVarCost, h, pVarCostDef) * vGen[(h, r)] for h in tech, r in region))
i <- grep("eqCapLim\\[", jl)[1]
cat(jl[(i - 2):(i + 2)], sep = "\n")
#> _t0 = time()
#> # generation is limited by available capacity
#> @constraint(
#>     model,
#>     eqCapLim[(h, r) in mTR],
#>     vGen[(h, r)] <= get(pAvail, (h, r), pAvailDef) * vCap[(h, r)]
#> )
#> _log_constraint(constraint_stats, "eqCapLim", eqCapLim, length(eqCapLim), time() - _t0)
```

Three properties of this code matter:

- **Variables are indexed by their domain mapping**, `vCap[mTR]`, and
  constraints iterate over it, `eqCapLim[(h, r) in mTR]`. The mapping is
  loaded as a Julia `Set` of tuples (or of strings when it has one
  dimension), so membership tests are constant time.
- **Parameters are dictionaries read through
  [`get()`](https://rdrr.io/r/base/get.html) with a default**,
  `get(pCapCost, h, pCapCostDef)`. A tuple missing from the data yields
  the declared default rather than an error, which mirrors the GAMS
  semantics the AST follows.
- **Every constraint block is timed and counted.** The generated code
  writes `constraint_stats.csv` (instances, dimension, size of the
  domain mapping, build time, non-zeros per row) and
  `variable_stats.csv` (instances, bound types). On a translated model
  these two files are the first thing to read: an equation with zero
  instances or a variable with no finite bound is visible there before
  any solve.

`data.jl` finds the workspace relative to its own location and detects
the format from `format.txt`, so the same file works for CSV, Arrow IPC
and Parquet workspaces. With `data_mode = "embedded"` the data is
written into the Julia source instead, which makes the model file
self-contained but means it has to be regenerated when the data changes.

### Pyomo

[`write_pyomo()`](https://optimal2050.github.io/multimod/r/reference/write_pyomo.md)
also writes two files, `model.py` and `data.py`. The model is a Pyomo
`AbstractModel`: declarations only, instantiated with data by `data.py`
at run time.

``` r

py <- write_pyomo(toy, model_dir = ws, export_vars = TRUE)
mp <- readLines(py)
cat(grep("^model\\.(tech|mTR|pAvail|vGen|eqCapLim) ", mp, value = TRUE), sep = "\n")
#> model.tech = Set(doc="generation technologies")
#> model.mTR = Set(dimen=2, doc="technology x region pairs")
#> model.pAvail = Param(model.tech, model.region, default=0, domain=Reals, doc="availability factor")
#> model.vGen = Var(model.mTR, within=NonNegativeReals, bounds=(0, None))  # generation, MWh
#> model.eqCapLim = Constraint(model.tech, model.region, rule=_eqCapLim_rule)
```

Sets and parameters are declared with their index sets and defaults;
variables over their domain mapping; constraints through a rule function
per equation. The runner at the end of `model.py` builds the instance,
picks the solver named in the `PYOMO_SOLVER` environment variable (set
by `solve_pyomo(solver = )`), solves, and writes `solution/status.json`.
With `export_vars = TRUE` it also writes `solution/variables.json`,
which is what `load_results` reads.

## Running the code

Each `solve_*()` function runs the toolchain on the workspace and
returns a list with `success`, `objective`, `status`, timing, the raw
solver output, and, when a model is passed, the model with solutions
attached under `$model`.

``` r

g <- solve_gmpl(toy, ws, verbose = FALSE)
g$objective
#> [1] 198000
g$status
#> [1] "OPTIMAL"
```

``` r

j <- solve_jump(toy, ws, verbose = FALSE)
#> Loading solutions from: solvers/jump/solution
j$objective
#> [1] 198000
j$status
#> [1] "OPTIMAL"
```

``` r

p <- solve_pyomo(toy, ws, solver = "highs", verbose = FALSE, load_results = TRUE)
p$objective
#> [1] 198000
p$status$termination
#> [1] "optimal"
```

The command each route runs, which also runs the code by hand from the
backend’s directory for closer inspection:

| Route | Command | Leaves behind in `solvers/<backend>/` |
|----|----|----|
| GLPK | `glpsol -m model.mod -d data.dat -o solution.sol --log solver.log` | `solution.sol`, `solver.log`, `solution/*.csv`, the two stats files |
| JuMP | `julia model.jl` | `solution/*.csv`, the two stats files, the Julia output |
| Pyomo | `python model.py` | `solution/status.json`, `solution/variables.json` |

The first Julia run installs and compiles the packages the model needs,
which makes it much slower than later runs.
[`solve_jump()`](https://optimal2050.github.io/multimod/r/reference/solve_jump.md)
and
[`solve_gmpl()`](https://optimal2050.github.io/multimod/r/reference/solve_gmpl.md)
accept `timeout =`; `solve_pyomo(ensure_env = TRUE)` checks the Python
environment with
[`mm_check_python()`](https://optimal2050.github.io/multimod/r/reference/mm_check_python.md)
before running.

## Results back in R

With `load_results = TRUE` (the default when a model is passed) every
route stores each variable’s values as a data frame under
`model$variables[[name]]$solution`, with the dimension names as columns
and `value` last, and copies the files to `variables/` in the workspace.

``` r

g$model$variables$vGen$solution
#>   tech region value
#> 1 coal  south   600
#> 2 wind  north  1000
g$model$solution_metadata$variables_loaded
#> [1] "vCap"  "vCost" "vGen"
```

Because every table has the same shape, results from different routes
line up directly. The objective is the simplest comparison:

``` r

objectives <- c(highs = r$objective)
if (has_glpsol) objectives["gmpl"]  <- g$objective
if (has_julia)  objectives["jump"]  <- j$objective
if (has_python) objectives["pyomo"] <- p$objective
objectives
#>  highs   gmpl   jump  pyomo 
#> 198000 198000 198000 198000
```

and one variable across routes is a merge on the index columns:

``` r

v_highs <- r$primal[r$primal$symbol == "vGen", c("i1", "i2", "value")]
names(v_highs) <- c("tech", "region", "highs")
v_gmpl <- g$model$variables$vGen$solution
names(v_gmpl)[3] <- "gmpl"
merge(v_highs, v_gmpl, all = TRUE)
#> Key: <tech, region>
#>      tech region highs  gmpl
#>    <char> <char> <num> <num>
#> 1:   coal  north     0    NA
#> 2:   coal  south   600   600
#> 3:    gas  north     0    NA
#> 4:    gas  south     0    NA
#> 5:   wind  north  1000  1000
#> 6:   wind  south     0    NA
```

The GLPK table lists only non-zero entries (the generated `printf` loop
filters them), so the merge fills the zeros with `NA`; the matrix route
reports every column.

## When routes disagree

Same model, same data, different objective: the translation has read
something differently. The cases below are the common causes, in the
order in which they are checked.

1.  **An equation produces no rows.** Its domain mapping has no tuples,
    or the tuples do not match the data. In R,
    `table(lp$row_index$symbol)` shows the row count per equation, and
    `model_to_lp(on_empty_row = )` decides whether an empty row is an
    error (the default for user constraints), a warning or ignored. In
    JuMP, `constraint_stats.csv` shows `count` 0; in MathProg the same
    file shows the cardinality product, which is only an upper bound.
2.  **A parameter’s default differs.** The matrix route and JuMP use the
    declared `defVal` for a tuple missing from the data; MathProg uses
    the `default` written in the data file, which is `defVal` or 0. A
    parameter whose `defVal` is `NULL` and whose data is incomplete is
    read as 0 by every route, which may not be what the model meant.
    [`validate()`](https://optimal2050.github.io/multimod/r/reference/validate.md)
    lists parameters without data.
3.  **Infinite values.** `Inf` in parameter data cannot enter a matrix.
    The matrix route drops the affected rows and maps an infinite
    default to 0, as the energyRt writers do; MathProg receives `1e20`
    (`write_gmpl(INF = )`); JuMP and Pyomo receive `Inf` as a bound
    where the value is a bound and as a coefficient otherwise, where the
    solver decides. Search the data for `Inf` before comparing.
4.  **Missing index aliases.** The code generators write iterator names
    from the model’s index aliases. A model built in R has none until
    [`add_index_aliases()`](https://optimal2050.github.io/multimod/r/reference/add_index_aliases.md)
    adds them; without them a `sum{}` is written with no iterator and
    glpsol rejects the file.
    [`has_index_aliases()`](https://optimal2050.github.io/multimod/r/reference/has_index_aliases.md)
    reports whether they are present.
5.  **Row counts, not objectives.** When the objectives differ, compare
    per-equation instance counts first (`table(lp$row_index$symbol)`
    against `constraint_stats.csv`), then per-variable bounds
    (`lp$col_index` against `variable_stats.csv`). A count that differs
    identifies the equation; an objective that differs does not.

## Choosing a route

| Route | Needs | Returns | Use when |
|----|----|----|----|
| [`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.md) | the ‘highs’ R package | primal, duals, reduced costs, the LP arrays | the model fits in memory and duals or repeated solves ([`mm_session()`](https://optimal2050.github.io/multimod/r/reference/mm_session.md)) are needed |
| [`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.md) | the ‘highs’ R package | an MPS file | the solver runs elsewhere |
| [`solve_gmpl()`](https://optimal2050.github.io/multimod/r/reference/solve_gmpl.md) | `glpsol` | primal values | a small or medium LP, or when a readable model file is the point |
| [`solve_jump()`](https://optimal2050.github.io/multimod/r/reference/solve_jump.md) | Julia | primal values, build statistics | a large model, or a solver only JuMP wraps (`optimizer =`) |
| [`solve_pyomo()`](https://optimal2050.github.io/multimod/r/reference/solve_pyomo.md) | Python with pyomo | primal values, status | the results continue in a Python pipeline |
| [`write_gams()`](https://optimal2050.github.io/multimod/r/reference/write_gams.md) | GAMS elsewhere | GAMS source | hand-over to a GAMS installation or NEOS |

Whatever the route, the same model object, data and workspace are behind
it, which is what makes the comparison in the previous section possible.
