
<!-- README.md is generated from README.Rmd. Edit this file, then run:
     devtools::build_readme() -->

# multimod [<img src="man/figures/logo.png" align="right" height="130" alt="multimod website" />](https://optimal2050.github.io/multimod/)

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: Apache
2.0](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
<!-- badges: end -->

> **A unified framework for representing, transforming, and solving
> mathematical optimization models across languages and solvers.**

**multimod** is an R package for representing, analyzing, transforming,
and solving mathematical optimization models. It stores sets, mappings,
parameters, variables, and equations in a structured abstract syntax
tree (AST). Models can be constructed with R functions or imported from
the supported subsets of GAMS and GNU MathProg.

The package validates and visualizes model structure, folds redundant
dimensions, trims unused symbols, and stores models as JSON or in CSV-
and Arrow-based workspaces. It renders models and equations as GAMS, GNU
MathProg, JuMP, Pyomo, or LaTeX.

For continuous linear optimization, multimod can generate an LP matrix
for HiGHS or cuOpt, write an MPS file, or run generated model code with
the corresponding external toolchain. It also reads solver results into
R.

multimod originated in energy system modeling and includes an energy
model among its examples. Its model representation is not specific to
that domain.

## Architecture: from formulation to solution

A model can enter multimod through R constructors, supported equation
syntax, or a supported source-code reader. Once represented as an AST,
it can be inspected, reduced, saved, translated into another modeling
language, or compiled directly into an LP matrix. Each output route
operates on the same model representation.

``` text
┌─────────────────────────────┐
│        Model inputs         │
├─────────────────────────────┤
│ R constructors and DSL      │ → new_*(), ast_*(), mm_equation()
│ GAMS    (basic/linear only) │ → read_gams()
│ MathProg (limited features) │ → read_gmpl()
│ JuMP    (constraints only)  │ → mm_equation()
│ {{Pyomo, AMPL, MPS, ...}}   │ → {{...}}
└─────────────────────────────┘
              ↓
   ┌────────────────────────┐
   │   multimod Core        │        Analysis & Manipulation:
   │ (Abstract Syntax Tree) │         • fold_model()        [x]
   ├────────────────────────┤         • trim_model()        [x]
   │ • Sets & Aliases       │  →      • save_model()        [x]
   │ • Parameters           │         • as_visNetwork()     [x]
   │ • Variables            │  ←      • model statistics    [x]
   │ • Equations            │         • model_to_json()     [x]
   │ • Mappings             │         {{auto-mapping}}      [ ]
   └────────────────────────┘         {{symbolic simplify}} [ ]
                 ↓                                     ↓
┌──────────────────────────────────┐  ┌──────────────────────────────────┐
│ Output: model code               │  │ Output: LP matrix                │
├──────────────────────────────────┤  ├──────────────────────────────────┤
│ LaTeX    write_latex()       [x] │  │ model_to_lp()               [x]  │
│ MathProg write_gmpl()        [x] │  │ native C++ executor         [x]  │
│ JuMP     write_jump()        [x] │  │ write_mps(stream=TRUE)      [x]  │
│ Pyomo    write_pyomo()       [x] │  │ native presolve/remap       [x]  │
│ GAMS     write_gams() +      [x] │  │ solve_highs()               [x]  │
│          write_gams_data()       │  │ solve_mps()                 [x]  │
│ {{AMPL, ...}}                [ ] │  │ read_mps_solution()         [x]  │
│                                  │  │                                  │
│ then an external solver:         │  │ direct matrix construction       │
│ glpsol, Julia, Python, NEOS      │  │ no generated model source        │
└──────────────────────────────────┘  └──────────────────────────────────┘

Status: [x] implemented | [~] and { } developing | [ ] and {{ }} considering
```

<!-- ![multimod architecture](man/figures/multimod-architecture.svg) -->

The diagram summarizes the architecture; the table under
[Capabilities](#capabilities) lists the implemented functions.

The current tested scope is **linear programming with continuous
variables**. Language importers cover documented subsets, and some
output routes remain experimental; see [Capabilities](#capabilities) and
the
[roadmap](https://optimal2050.github.io/multimod/r/articles/roadmap.html).

### Native matrix route

For performance on large models, the matrix route includes a C++17
executor for supported equation plans. Enable it with
`options(multimod.engine = "native")`; equations that cannot be lowered
to a native plan continue through the R engine. The executor is
multithreaded, slices large expansions, and is used by both
`model_to_lp()` and the streamed MPS route.

`write_mps(stream = TRUE)` sends coefficient blocks to a native spool
and MPS writer instead of materializing the complete sparse matrix in R.
Its native scan, presolve-map, and remap steps can eliminate doubleton
rows and proportional columns while the file is built. Build-time
definition substitution is applied on the same route. These paths are
intended to reduce matrix-build time and bound the memory used for
coefficient blocks.

Large-model runs indicate that the native streamed route can
substantially reduce matrix-construction and MPS-writing time relative
to the earlier in-memory route. Performance depends on model structure
and build options; detailed benchmarks will be reported separately.

## Installation

``` r
pak::pkg_install("optimal2050/multimod")
```

The matrix route requires the ‘highs’ R package. Other routes require
their corresponding toolchains: `glpsol` (the ‘glpsol’ R package ships
it), Julia with JuMP, or Python with Pyomo; see the “Configuring solver
paths” article.

## Quickstart: one model, several representations

``` r
library(multimod)

toy <- example_models$toy          # built in R, see "Writing a model in R"
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

as_latex(toy$equations$eqDemand) |> cat()
#> \begin{flushleft}
#> \subsection*{\textbf{eqDemand} -- \textit{demand is met in every region}}
#> \quad$\textbf{eqDemand}
#> _{\texttt{region}}$
#> $\mid\left\{\texttt{region}\right\} \in \mathit{mR}_{\texttt{region}}$ \\
#> \begin{equation*}
#> \begin{adjustbox}{max width=\textwidth}
#> $\begin{aligned}
#> &\sum_{\texttt{tech}} \bm{\mathit{vGen}}_{\texttt{tech},\texttt{region}} \ge \mathsf{pDemand}_{\texttt{region}} \\
#> \end{aligned}$
#> \end{adjustbox}
#> \end{equation*}
#> \vspace{1em}
#> \end{flushleft}
as_gams(toy$equations$eqDemand) |> cat()
#> eqDemand[region]$mR[region]..
#>   sum(tech, vGen[tech,region]) =g=
#>   pDemand[region];
```

An equation is written as text — MathProg by default, GAMS or JuMP by
choice — and parsed into the same tree the constructors build:

``` r
cap_total <- mm_equation(
  toy,
  "eqCapTotal: sum{t in tech, r in region} vCap[t,r] <= 2000",
  desc = "total installed capacity"
)
as_jump(cap_total, model = toy) |> cat()
#> @constraint(
#>     model,
#>     eqCapTotal,
#>     sum(vCap[(t, r)] for t in tech, r in region) <= 2000
#> )
```

The same model solved in R:

``` r
r <- solve_highs(toy, attach = FALSE)
r$objective
#> [1] 198000
```

or written for another toolchain and solved there:

``` r
ws <- tempfile("toy-"); dir.create(ws)
write_gmpl(toy, model_dir = ws);  solve_gmpl(toy, ws)$objective    # GLPK
write_jump(toy, model_dir = ws);  solve_jump(toy, ws)$objective    # Julia
write_pyomo(toy, model_dir = ws); solve_pyomo(toy, ws, load_results = TRUE)$objective
```

With the documented toolchains and example data, these routes return an
objective value of 198000. The “Solving and reading results” article
runs them side by side; “Two models from the field” does the same for
the energyRt reference model and for OSeMOSYS, read from its MathProg
source and solved both through JuMP and from the matrix.

## Capabilities

|  |  |
|----|----|
| Build a model in R | `new_set()`, `new_parameter()`, `new_variable()`, `new_equation()`, the `ast_*()` expression constructors, `validate()`; `mm_equation()` for an equation written as MathProg, GAMS or JuMP text |
| Read a model | `read_gams()` for the linear subset of GAMS; `read_gmpl()` for MathProg with data sections; `mm_equation(lang = "jump")` for a JuMP `@constraint()` statement |
| Render | `as_gams()`, `as_gmpl()`, `as_jump()`, `as_pyomo()`, `as_latex()` for any node; `write_gams()`, `write_gmpl()`, `write_jump()`, `write_pyomo()`, `write_latex()` for the model |
| Solve in R | `model_to_lp()`, `solve_highs()`, `write_mps()`, `solve_mps()`, `check_matrix_numbers()`, `mm_session()` for repeated solves |
| Solve through generated code | `solve_gmpl()`, `solve_jump()`, `solve_pyomo()`, each loading the solution back |
| Reduce | `fold_model()` drops dimensions a parameter does not vary over; `trim_model()` drops unused symbols |
| Store and exchange | `save_model()`, `load_model()`, `get_data()` (CSV, Arrow IPC, Parquet); `model_to_json()`, `ast_schema()` |

Linear programs with continuous variables are the tested class.
Structural model reduction (`fold_model()`, `trim_model()`) is distinct
from numerical solver presolving. GAMS output is readable but not yet
runnable without edits, and there is no Pyomo reader; the
[roadmap](https://optimal2050.github.io/multimod/r/articles/roadmap.html)
lists what else is not done yet.

## Documentation and next steps

- `vignette("multimod")`: reading, translating, analyzing, exporting and
  solving a model in a few calls.
- [Writing a model in
  R](https://optimal2050.github.io/multimod/r/articles/ast.html): the
  constructors, step by step.
- [Model
  redundancy](https://optimal2050.github.io/multimod/r/articles/folding.html):
  folding and trimming a model to the size its data needs.
- Further articles on the
  [website](https://optimal2050.github.io/multimod/r/): solving and
  reading results, the two field models, the model workspace,
  configuration, LaTeX.

## Development direction

The long-term architecture gives the native core Python and Julia
interfaces beside the R one. Further research directions include
AI-assisted formulation and support for nonlinear and stochastic
optimization; these are planned, not implemented.

The repository is laid out for that architecture: the R package at the
root, `core/` holding the native core (copied into `src/` for the R
build) and `python/` reserved for its Python binding, `specs/` for the
cross-language goldens, and one documentation site under `docs/` with
the R reference at [`/r/`](https://optimal2050.github.io/multimod/r/)
beside the future language sections.

## Integration with energyRt

[energyRt](https://github.com/optimal2050/energyRt) uses multimod as its
model compiler: `energyRt::multimod_from_energyRt()` builds the model
from an interpolated scenario, and the LP or MPS multimod produces is
what energyRt hands to a solver.

## License

Apache License 2.0; see `LICENSE.md`. The bundled `example_models`
dataset is derived from energyRt and remains under energyRt’s Apache
License 2.0; `inst/COPYRIGHTS` has the details, and the attribution for
OSeMOSYS, which the documentation reads from its public repository and
does not bundle.
