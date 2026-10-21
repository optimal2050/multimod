# Get started with multimod

multimod represents a linear optimization model as an abstract syntax
tree (AST) of sets, mappings, parameters, variables and equations,
independent of any modeling language. This vignette takes one small
model through the main operations: reading and translating, analyzing
structure and redundancy, rendering to LaTeX, exporting, and solving.
Each step is a single call on the same model object.

``` r

library(multimod)
```

## A model

The package ships two example models. `example_models$toy` is a small
capacity-expansion LP built with the constructors: three technologies
supply two regions at least cost.

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

## Reading and translating

[`read_gmpl()`](https://optimal2050.github.io/multimod/reference/read_gmpl.md)
reads a GNU MathProg model, with its data section, from a file or from
text. The result is a multimod model of the same kind as `toy`.

``` r

src <- c(
  "set REGION;",
  "set TECH;",
  "param Demand{r in REGION};",
  "param Cost{t in TECH};",
  "var gen{t in TECH, r in REGION} >= 0;",
  "minimize total: sum{t in TECH, r in REGION} Cost[t] * gen[t,r];",
  "s.t. meet{r in REGION}: sum{t in TECH} gen[t,r] >= Demand[r];",
  "data;",
  "set REGION := north south;",
  "set TECH := coal wind;",
  "param Demand := north 10 south 5;",
  "param Cost := coal 3 wind 1;",
  "end;"
)
m <- read_gmpl(src)
m
#> Model: gmpl_model
#> Description: GMPL model from in-memory source
#> Source:  <in-memory> 
#> Language:  gmpl 
#> Sets:  2 
#> Mappings:  0 
#> Aliases:  2 
#> Parameters:  2 
#> Variables:  2 
#> Equations:  2
```

Any equation of the model renders to GAMS, JuMP and Pyomo:

``` r

meet <- m$equations$meet
cat(as_gams(meet))
#> meet[r]..
#>   sum(t, gen[t,r]) =g=
#>   Demand[r];
cat(as_jump(meet, model = m))
#> @constraint(
#>     model,
#>     meet[r in REGION],
#>     sum(gen[t, r] for t in TECH) >= get(Demand, r, DemandDef)
#> )
cat(as_pyomo(meet, model = m))
#> def _meet_rule(model, r):
#>     _expr = sum(model.gen[t, r] for t in model.TECH) >= model.Demand[r]
#>     if _expr is True or _expr is False:
#>         return Constraint.Feasible if _expr else Constraint.Infeasible
#>     return _expr
#> 
#> model.meet = Constraint(model.REGION, rule=_meet_rule)
```

[`read_gams()`](https://optimal2050.github.io/multimod/reference/read_gams.md)
reads the linear subset of GAMS in the same way. A single equation can
also be written as MathProg, GAMS or JuMP text with
[`mm_equation()`](https://optimal2050.github.io/multimod/reference/mm_equation.md),
which resolves its names against the model’s declarations:

``` r

cap <- mm_equation(
  toy,
  "eqCapTotal.. sum((tech, region), vCap(tech, region)) =l= 2000;",
  lang = "gams"
)
cat(as_gmpl(cap, model = toy))
#> s.t. eqCapTotal: sum{t in tech, r in region}(vCap[t,r]) <= 2000;
```

## Structure and redundancy

[`validate()`](https://optimal2050.github.io/multimod/reference/validate.md)
checks that every symbol an equation refers to is declared and that the
dimensions agree.
[`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md)
assembles the constraint matrix, and
[`check_matrix_numbers()`](https://optimal2050.github.io/multimod/reference/check_matrix_numbers.md)
reports the range of its coefficients, which indicates how well scaled
the problem is for a solver.

``` r

validate(toy)
#> Model Validation Results
#> ========================
#> 
#> Status: VALID ✓
#> 
#> INFO:
#>   ℹ Objective function: minimize vCost 
#>   ℹ Found 2 index_aliases covering 2 sets/aliases
lp <- model_to_lp(toy)
dim(lp$A)
#> [1]  9 13
chk <- check_matrix_numbers(lp, verbose = FALSE)
chk$summary[, c("section", "entries", "min_abs", "max_abs", "orders")]
#>     section entries min_abs max_abs orders
#> 1   COLUMNS      29    0.25     120    2.7
#> 2 OBJECTIVE       1    1.00       1    0.0
#> 3       RHS      10  600.00    1000    0.2
#> 4    BOUNDS       0      NA      NA     NA
```

Redundancy is easier to see on a larger model. `example_models$energyRt`
holds the energyRt energy system model, parsed from its GAMS source with
data attached.
[`analyze_parameter_redundancy()`](https://optimal2050.github.io/multimod/reference/analyze_parameter_redundancy.md)
measures how many of each parameter’s values repeat across a dimension,
which is what folding removes, and
[`trim_model()`](https://optimal2050.github.io/multimod/reference/trim_model.md)
marks the symbols that the data leaves unused:

``` r

big <- example_models$energyRt$multimod
red <- analyze_parameter_redundancy(big, verbose = FALSE)
head(red[order(-red$orig_redundancy_pct),
         c("param_name", "orig_rows", "orig_unique", "orig_redundancy_pct")])
#>        param_name orig_rows orig_unique orig_redundancy_pct
#> 3   pTechCinp2use      1344           3                99.8
#> 30    pTradeIrEff       576           1                99.8
#> 8       pTechAfLo       336           1                99.7
#> 23 pStorageInpEff       336           1                99.7
#> 24 pStorageOutEff       336           1                99.7
#> 20  pTimesliceAgg       180           1                99.4

trimmed <- trim_model(big, verbose = FALSE)
get_trim_summary(trimmed, format = "data.frame")
#>   element_type total trimmed remaining percent_trimmed
#> 1         sets    27       6        21            22.2
#> 2   parameters   218       1       217             0.5
#> 3     mappings   324     160       164            49.4
#> 4    variables    93      10        83            10.8
#> 5    equations   153     103        50            67.3
```

The “Folding and trimming” article on the website applies both
reductions and shows that the optimum is unchanged.

## LaTeX

[`as_latex()`](https://optimal2050.github.io/multimod/reference/as_latex.md)
renders any node as LaTeX math, and
[`write_latex()`](https://optimal2050.github.io/multimod/reference/write_latex.md)
writes the whole model as a LaTeX document. The demand balance of `toy`:

``` r

eq <- toy$equations$eqDemand
lhs <- as_latex(eq$lhs)
rhs <- as_latex(eq$rhs)
lhs
#> [1] "\\sum_{\\texttt{tech}} \\bm{\\mathit{vGen}}_{\\texttt{tech},\\texttt{region}}"
```

``` math
 \sum_{\texttt{tech}} \boldsymbol{\mathit{vGen}}_{\texttt{tech},\texttt{region}} \ge \mathsf{pDemand}_{\texttt{region}} 
```

## Exporting

[`write_gmpl()`](https://optimal2050.github.io/multimod/reference/write_gmpl.md),
[`write_jump()`](https://optimal2050.github.io/multimod/reference/write_jump.md),
[`write_pyomo()`](https://optimal2050.github.io/multimod/reference/write_pyomo.md)
and
[`write_gams()`](https://optimal2050.github.io/multimod/reference/write_gams.md)
render the whole model in one language. Without a file they return the
code; the declarations, objective and constraints of `toy` in MathProg:

``` r

code <- write_gmpl(toy)
cat(code[13:27], sep = "\n")
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
```

[`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md)
stores the model and its data as a workspace of CSV, Arrow IPC or
Parquet files, which the generated JuMP and Pyomo code reads at run
time.
[`model_to_json()`](https://optimal2050.github.io/multimod/reference/model_to_json.md)
serializes the structure for other tools.

``` r

ws <- tempfile("toy-")
toy <- save_model(toy, ws, format = "csv", verbose = FALSE)
list.files(ws, recursive = TRUE)
#>  [1] "format.txt"                   "mappings/mappings.csv"       
#>  [3] "mappings/mR/data.csv"         "mappings/mTR/data.csv"       
#>  [5] "metadata.json"                "model.rds"                   
#>  [7] "parameters/parameters.csv"    "parameters/pAvail/data.csv"  
#>  [9] "parameters/pCapCost/data.csv" "parameters/pDemand/data.csv" 
#> [11] "parameters/pVarCost/data.csv" "sets/region/data.csv"        
#> [13] "sets/sets.csv"                "sets/tech/data.csv"

json <- model_to_json(toy)
substr(json, 1, 120)
#> [1] "{\n  \"multimod_schema\": \"1.0\",\n  \"node\": \"model\",\n  \"data_inlined\": false,\n  \"name\": \"toy\",\n  \"desc\": \"two-region capacit"
```

## Solving

[`solve_highs()`](https://optimal2050.github.io/multimod/reference/solve_highs.md)
assembles the LP and solves it in R with the ‘highs’ package:

``` r

result <- solve_highs(toy, attach = FALSE)
result$objective
#> [1] 198000
```

[`solve_gmpl()`](https://optimal2050.github.io/multimod/reference/solve_gmpl.md),
[`solve_jump()`](https://optimal2050.github.io/multimod/reference/solve_jump.md)
and
[`solve_pyomo()`](https://optimal2050.github.io/multimod/reference/solve_pyomo.md)
solve the generated code with GLPK, Julia or Python instead, and load
the solution back into the model.

## Further reading

The articles on the package website,
<https://optimal2050.github.io/multimod/>, cover each topic in detail:

- “Writing a model in R”: the constructors, building `toy` step by step.
- “Solving and reading results”: every solve route and its output.
- “Folding and trimming”: the two reductions and their effect.
- “Two models from the field”: the energyRt model and a published
  MathProg model, read and solved.
- “Model workspace and data”, “Configuring solver paths”, “LaTeX
  rendering”.
