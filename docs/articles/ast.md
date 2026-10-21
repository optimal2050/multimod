# Writing a model in R

``` r

library(multimod)
```

multimod models are usually read from a source file:
[`read_gams()`](https://optimal2050.github.io/multimod/reference/read_gams.md)
and
[`read_gmpl()`](https://optimal2050.github.io/multimod/reference/read_gmpl.md)
turn a GAMS or MathProg model into the structure described here. This
article builds the same kind of object directly in R, which is the way
to write a new model, to generate one programmatically, or to understand
what the readers produce. The model is a small capacity-expansion
problem: three technologies supply electricity to two regions at least
cost. It ships with the package as `example_models$toy`; the code below
reproduces it.

## The parts of a model

A model is a list of named components, each a small S3 object:

| Component | Constructor | Holds |
|----|----|----|
| sets | [`new_set()`](https://optimal2050.github.io/multimod/reference/new_set.md) | the index values of a dimension |
| mappings | [`new_mapping()`](https://optimal2050.github.io/multimod/reference/new_mapping.md) | tuples over several sets, used as domains |
| parameters | [`new_parameter()`](https://optimal2050.github.io/multimod/reference/new_parameter.md) | data indexed by dimensions |
| variables | [`new_variable()`](https://optimal2050.github.io/multimod/reference/new_variable.md) | decision variables, with a domain and bounds |
| equations | [`new_equation()`](https://optimal2050.github.io/multimod/reference/new_equation.md) | left side, relation, right side, over a domain |

The sides of an equation are *expressions*: trees of nodes of the
abstract syntax tree (AST), built with
[`ast_variable()`](https://optimal2050.github.io/multimod/reference/ast_variable.md),
[`ast_parameter()`](https://optimal2050.github.io/multimod/reference/ast_parameter.md),
[`ast_expression()`](https://optimal2050.github.io/multimod/reference/ast_expression.md),
[`ast_sum()`](https://optimal2050.github.io/multimod/reference/ast_sum.md)
and their relatives. Every renderer
([`as_gams()`](https://optimal2050.github.io/multimod/reference/as_gams.md),
[`as_latex()`](https://optimal2050.github.io/multimod/reference/as_latex.md),
the LP matrix) is a walk over that tree.

## Sets

A set names a dimension and lists its values.

``` r

techs   <- c("coal", "gas", "wind")
regions <- c("north", "south")

sets <- list(
  tech   = new_set("tech",   desc = "generation technologies", data = techs),
  region = new_set("region", desc = "model regions",           data = regions)
)
sets$tech
#> <AST set> tech
```

## Mappings

A mapping is a table of tuples over one or more sets. Mappings serve as
*domains*: an indexed variable or equation exists for exactly the tuples
of its domain mapping, which is how sparse models stay sparse. Here
every technology may be built in every region, so the domain is the full
grid.

``` r

tuples <- expand.grid(tech = techs, region = regions, stringsAsFactors = FALSE)

mappings <- list(
  mTR = new_mapping("mTR", desc = "technology x region pairs",
                    dims = c("tech", "region"), data = tuples),
  mR  = new_mapping("mR",  desc = "regions",
                    dims = "region", data = data.frame(region = regions))
)
mappings$mTR$data
#>   tech region
#> 1 coal  north
#> 2  gas  north
#> 3 wind  north
#> 4 coal  south
#> 5  gas  south
#> 6 wind  south
```

## Parameters

A parameter declares its dimensions and carries a data frame with one
column per dimension plus `value`.

``` r

parameters <- list(
  pCapCost = new_parameter("pCapCost", desc = "annualized capacity cost, EUR/kW",
    dims = "tech",
    data = data.frame(tech = techs, value = c(120, 70, 90))),
  pVarCost = new_parameter("pVarCost", desc = "variable cost, EUR/MWh",
    dims = "tech",
    data = data.frame(tech = techs, value = c(30, 55, 0))),
  pAvail = new_parameter("pAvail", desc = "availability factor",
    dims = c("tech", "region"),
    data = transform(tuples, value = c(0.85, 0.85, 0.90, 0.90, 0.35, 0.25))),
  pDemand = new_parameter("pDemand", desc = "annual demand, MWh",
    dims = "region",
    data = data.frame(region = regions, value = c(1000, 600)))
)
parameters$pAvail
#> <AST parameter> pAvail
#>   dims:  [tech,region]
```

## Variables

Variables declare dimensions, a domain mapping, a type and bounds. A
scalar variable has `dims = character(0)`.

``` r

variables <- list(
  vCap = new_variable("vCap", desc = "installed capacity, kW",
                      dims = c("tech", "region"), domain = "mTR",
                      vtype = "positive", bounds = list(lo = 0)),
  vGen = new_variable("vGen", desc = "generation, MWh",
                      dims = c("tech", "region"), domain = "mTR",
                      vtype = "positive", bounds = list(lo = 0)),
  vCost = new_variable("vCost", desc = "total system cost, EUR",
                       dims = character(0))
)
```

## Expressions

Inside an equation, symbols are *referenced* rather than declared:
[`ast_variable()`](https://optimal2050.github.io/multimod/reference/ast_variable.md)
and
[`ast_parameter()`](https://optimal2050.github.io/multimod/reference/ast_parameter.md)
take the name and the index symbols, given as
[`ast_dims()`](https://optimal2050.github.io/multimod/reference/ast_dims.md).
[`ast_expression()`](https://optimal2050.github.io/multimod/reference/ast_expression.md)
combines two nodes with an operator, and
[`ast_sum()`](https://optimal2050.github.io/multimod/reference/ast_sum.md)
sums an expression over index symbols.

``` r

gen   <- ast_variable("vGen", dims = ast_dims("tech", "region"))
avail <- ast_parameter("pAvail", dims = ast_dims("tech", "region"))
cap   <- ast_variable("vCap", dims = ast_dims("tech", "region"))

limit <- ast_expression("*", avail, cap)
node_type(limit)
#> [1] "expression"
limit
#> <AST expression> *
#>   lhs :
#> <AST parameter> pAvail
#>   dims:  [tech,region] 
#>   rhs:
#> <AST variable> vCap
#>   dims:  [tech,region]
```

Rendering is a method on the node, so an expression can be checked
before it is used:

``` r

as_gams(limit)
#> [1] "pAvail[tech,region] * vCap[tech,region]"
as_latex(limit)
#> [1] "\\mathsf{pAvail}_{\\texttt{tech},\\texttt{region}}  \\cdot  \\bm{\\mathit{vCap}}_{\\texttt{tech},\\texttt{region}}"
```

## Equations

An equation has a left side, a relation (`"=="`, `"<="`, `">="`), a
right side and a domain. The domain mapping decides for which tuples the
equation is generated.

``` r

eqCapLim <- new_equation("eqCapLim", desc = "generation is limited by capacity",
  dims = c("tech", "region"),
  lhs = gen, rhs = limit, relation = "<=",
  domain = ast_mapping("mTR", dims = ast_dims("tech", "region")))

eqDemand <- new_equation("eqDemand", desc = "demand is met in every region",
  dims = "region",
  lhs = ast_sum(ast_dims("tech"), ast_variable("vGen", dims = ast_dims("tech", "region"))),
  rhs = ast_parameter("pDemand", dims = ast_dims("region")),
  relation = ">=",
  domain = ast_mapping("mR", dims = ast_dims("region")))

eqCost <- new_equation("eqCost", desc = "total cost",
  dims = character(0),
  lhs = ast_variable("vCost", dims = ast_dims()),
  rhs = ast_sum(ast_dims("tech", "region"),
    ast_expression("+",
      ast_expression("*", ast_parameter("pCapCost", dims = ast_dims("tech")),
                          ast_variable("vCap", dims = ast_dims("tech", "region"))),
      ast_expression("*", ast_parameter("pVarCost", dims = ast_dims("tech")),
                          ast_variable("vGen", dims = ast_dims("tech", "region"))))),
  relation = "==")

eqDemand
#> <AST equation> eqDemand
#>   domain:  mR[region] 
#>   relation:  >= 
#>   lhs:  sum([tech]vGen[tech,region]) 
#>   rhs:  pDemand[region]
as_latex(eqCost)
#> [1] "\\begin{flushleft}\n\\subsection*{\\textbf{eqCost} -- \\textit{total cost}} \n\\quad$\\textbf{eqCost}$\n\\begin{equation*}\n\\begin{adjustbox}{max width=\\textwidth}\n$\\begin{aligned}\n&\\bm{\\mathit{vCost}}_{} = \\sum_{\\texttt{tech}, \\texttt{region}} \\mathsf{pCapCost}_{\\texttt{tech}}  \\cdot  \\bm{\\mathit{vCap}}_{\\texttt{tech},\\texttt{region}}  +  \\mathsf{pVarCost}_{\\texttt{tech}}  \\cdot  \\bm{\\mathit{vGen}}_{\\texttt{tech},\\texttt{region}} \\\\\n\\end{aligned}$\n\\end{adjustbox}\n\\end{equation*}\n\\vspace{1em}\n\\end{flushleft}"
```

## The model

[`new_model()`](https://optimal2050.github.io/multimod/reference/new_model.md)
assembles the components. The objective is declared as the equation that
defines it, the variable it defines and the sense.

``` r

toy <- new_model(
  name = "toy", desc = "two-region capacity expansion toy model",
  sets = sets, mappings = mappings, parameters = parameters,
  variables = variables,
  equations = list(eqCapLim = eqCapLim, eqDemand = eqDemand, eqCost = eqCost),
  objectives = list(list(equation = "eqCost", variable = "vCost",
                         sense = "minimize"))
)
toy
#> Model: toy
#> Description: two-region capacity expansion toy model
#> Sets:  2 
#> Mappings:  2 
#> Aliases:  0 
#> Parameters:  4 
#> Variables:  3 
#> Equations:  3
```

Every constructor validates its node against the AST schema as it is
built
([`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md)
lists the node types and their fields), so a malformed node is refused
where it is written.
[`validate()`](https://optimal2050.github.io/multimod/reference/validate.md)
checks the assembled model: every symbol referenced in an equation must
be declared, with matching dimensions.

``` r

v <- validate(toy)
v$valid
#> [1] TRUE
```

The code generators write indexed expressions with short iterator names
(`sum{t in tech}` in MathProg, `for t in tech` in Julia). A model built
in R has none until
[`add_index_aliases()`](https://optimal2050.github.io/multimod/reference/add_index_aliases.md)
derives them from the set names:

``` r

toy <- add_index_aliases(toy)
unlist(toy$index_aliases)
#>   tech region 
#>    "h"    "r"
```

## Solving

The matrix route needs no external software:
[`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md)
builds the constraint matrix from the equations and the data, and
[`solve_highs()`](https://optimal2050.github.io/multimod/reference/solve_highs.md)
passes it to the HiGHS solver (the ‘highs’ package).

``` r

lp <- model_to_lp(toy)
dim(lp$A)
#> [1]  9 13
```

``` r

r <- solve_highs(toy, attach = FALSE)
r$objective
#> [1] 198000
head(r$primal[r$primal$value > 0, ], 10)
#>        j symbol     i1     i2    lo    up      vtype       value reduced_cost
#>    <int> <char> <char> <char> <num> <num>     <char>       <num>        <num>
#> 1:     3   vCap   wind  north     0   Inf   positive   1111.1111            0
#> 2:     4   vCap   coal  south     0   Inf   positive    666.6667            0
#> 3:     9   vGen   wind  north     0   Inf   positive   1000.0000            0
#> 4:    10   vGen   coal  south     0   Inf   positive    600.0000            0
#> 5:    13  vCost   <NA>   <NA>  -Inf   Inf continuous 198000.0000            0
```

The code-generation routes render the same model for an external solver:
[`write_gmpl()`](https://optimal2050.github.io/multimod/reference/write_gmpl.md)
for GLPK,
[`write_jump()`](https://optimal2050.github.io/multimod/reference/write_jump.md)
for JuMP and
[`write_pyomo()`](https://optimal2050.github.io/multimod/reference/write_pyomo.md)
for Pyomo; the [Solving and reading
results](https://optimal2050.github.io/multimod/articles/solving.md)
article runs each with its `solve_*()` counterpart.

``` r

mod_file <- tempfile(fileext = ".mod")
write_gmpl(toy, file = mod_file)
cat(head(readLines(mod_file), 12), sep = "\n")
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
```

## Saving and exchanging the model

[`model_to_json()`](https://optimal2050.github.io/multimod/reference/model_to_json.md)
serializes the whole model, including the AST, in the language-neutral
JSON format defined by
[`ast_json_schema()`](https://optimal2050.github.io/multimod/reference/ast_json_schema.md);
[`model_from_json()`](https://optimal2050.github.io/multimod/reference/model_from_json.md)
reads it back. This is the interchange format for other implementations
of the model structure.

``` r

json <- model_to_json(toy)
back <- model_from_json(json)
identical(names(back$equations), names(toy$equations))
#> [1] TRUE
as_gams(back$equations$eqDemand)
#> [1] "eqDemand[region]$mR[region]..\n  sum(tech, vGen[tech,region]) =g=\n  pDemand[region];"
```

[`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md)
and
[`load_model()`](https://optimal2050.github.io/multimod/reference/load_model.md)
write a model as a workspace directory with its data tables in CSV or
Arrow files; see the [Model workspace and
data](https://optimal2050.github.io/multimod/articles/model_workspace.md)
article.

## Further reading

- [Get
  started](https://optimal2050.github.io/multimod/articles/multimod.md):
  reading, translating, analyzing and solving a model in a few calls.
- [Folding and
  trimming](https://optimal2050.github.io/multimod/articles/folding.md):
  reducing a model to the size its data needs.
- [Solving and reading
  results](https://optimal2050.github.io/multimod/articles/solving.md)
  and [Configuring solver
  paths](https://optimal2050.github.io/multimod/articles/configuration.md):
  the code-generation routes (GLPK, JuMP, Pyomo) and their setup.
