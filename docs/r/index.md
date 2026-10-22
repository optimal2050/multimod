# multimod

> **A unified framework for mathematical model representation, analysis,
> transformation, and optimization.**

**multimod** is a unified framework for working with mathematical
optimization models independently of the languages and solvers used to
implement them. Its current implementation is an R package. It
represents a model as a structured abstract syntax tree (AST) of sets,
mappings, parameters, variables, and equations. That representation can
be constructed in R, parsed from supported modeling-language syntax,
inspected and transformed, or translated into other representations.

**Define the mathematics once; choose how to express, simplify, and
solve it.** Rather than maintaining separate implementations of the same
formulation, multimod provides a common model representation and
multiple routes from formulation to solution.

## Why multimod?

- **Separate mathematical meaning from implementation.** Work with a
  structured formulation instead of tying model logic to one programming
  language, modeling system, or solver.
- **Move between modeling environments.** Generate MathProg, GAMS, JuMP,
  and Pyomo code, or render equations as LaTeX for inspection and
  documentation. Import supported subsets of existing GAMS and MathProg
  models.
- **Avoid unnecessary model-building overhead.** Assemble the
  linear-programming matrix directly in R and pass it to HiGHS, or
  export MPS, without first generating and executing another
  modeling-language implementation.
- **Reduce models to the information their data contains.** Fold
  dimensions with no variation and trim unused symbols, helping make
  formulations and their generated representations more compact.
- **Inspect and reuse model structure.** Validate and visualize the AST,
  examine model statistics, exchange structures as JSON, and maintain
  data in CSV or Arrow-based workspaces.
- **Support reproducible modeling workflows.** Keep formulations, data,
  generated code, and solution routes connected through a common model
  representation.

multimod originated in energy system modeling and includes an energy
model among its examples, but **its core is domain-independent**. It can
represent linear optimization models in other fields as well.

## Architecture: from formulation to solution

A model can enter multimod through R constructors, supported equation
syntax, or a supported source-code reader. Once represented as an AST,
it can be inspected, reduced, saved, translated into another modeling
language, or compiled directly into an LP matrix. These are alternative
routes from the *same mathematical formulation*, not separate models to
maintain.

``` text
┌─────────────────────────────┐
│      Input Formats          │
├─────────────────────────────┤
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
│ MathProg write_gmpl()        [x] │  │ write_mps()                 [x]  │
│ JuMP     write_jump()        [x] │  │ solve_highs()               [x]  │
│ Pyomo    write_pyomo()       [x] │  │ solve_mps()                 [x]  │
│ GAMS     write_gams() +      [x] │  │ read_mps_solution()         [x]  │
│          write_gams_data()       │  │                                  │
│ {{AMPL, ...}}                [ ] │  │ no generated model text, so      │
│                                  │  │ no model-build step — the        │
│ then an external solver:         │  │ time and memory wall on          │
│ glpsol, Julia, Python, NEOS      │  │ large models                     │
└──────────────────────────────────┘  └──────────────────────────────────┘

Status: [x] implemented | [~] and { } developing | [ ] and {{ }} considering
```

The diagram summarizes the architecture; the table under
[Capabilities](#capabilities) lists the implemented functions.

The current tested scope is **linear programming with continuous
variables**. Language importers cover documented subsets, and some
output routes remain experimental; see [Capabilities](#capabilities) and
the
[roadmap](https://optimal2050.github.io/multimod/r/articles/roadmap.html).

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

as_latex(toy$equations$eqDemand)
#> [1] "\\begin{flushleft}\n\\subsection*{\\textbf{eqDemand} -- \\textit{demand is met in every region}} \n\\quad$\\textbf{eqDemand} \n_{\\texttt{region}}$ \n$\\mid\\left\\{\\texttt{region}\\right\\} \\in \\mathit{mR}_{\\texttt{region}}$ \\\\\n\\begin{equation*}\n\\begin{adjustbox}{max width=\\textwidth}\n$\\begin{aligned}\n&\\sum_{\\texttt{tech}} \\bm{\\mathit{vGen}}_{\\texttt{tech},\\texttt{region}} \\ge \\mathsf{pDemand}_{\\texttt{region}} \\\\\n\\end{aligned}$\n\\end{adjustbox}\n\\end{equation*}\n\\vspace{1em}\n\\end{flushleft}"
as_gams(toy$equations$eqDemand)
#> [1] "eqDemand[region]$mR[region]..\n  sum(tech, vGen[tech,region]) =g=\n  pDemand[region];"
```

An equation is written as text — MathProg by default, GAMS or JuMP by
choice — and parsed into the same tree the constructors build:

``` r

cap_total <- mm_equation(
  toy,
  "eqCapTotal: sum{t in tech, r in region} vCap[t,r] <= 2000",
  desc = "total installed capacity"
)
as_jump(cap_total, model = toy)
#> [1] "@constraint(\n    model,\n    eqCapTotal,\n    sum(vCap[(t, r)] for t in tech, r in region) <= 2000\n)"
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
| Build a model in R | [`new_set()`](https://optimal2050.github.io/multimod/r/reference/new_set.md), [`new_parameter()`](https://optimal2050.github.io/multimod/r/reference/new_parameter.md), [`new_variable()`](https://optimal2050.github.io/multimod/r/reference/new_variable.md), [`new_equation()`](https://optimal2050.github.io/multimod/r/reference/new_equation.md), the `ast_*()` expression constructors, [`validate()`](https://optimal2050.github.io/multimod/r/reference/validate.md); [`mm_equation()`](https://optimal2050.github.io/multimod/r/reference/mm_equation.md) for an equation written as MathProg, GAMS or JuMP text |
| Read a model | [`read_gams()`](https://optimal2050.github.io/multimod/r/reference/read_gams.md) for the linear subset of GAMS; [`read_gmpl()`](https://optimal2050.github.io/multimod/r/reference/read_gmpl.md) for MathProg with data sections; `mm_equation(lang = "jump")` for a JuMP `@constraint()` statement |
| Render | [`as_gams()`](https://optimal2050.github.io/multimod/r/reference/as_gams.md), [`as_gmpl()`](https://optimal2050.github.io/multimod/r/reference/as_gmpl.md), [`as_jump()`](https://optimal2050.github.io/multimod/r/reference/as_jump.md), [`as_pyomo()`](https://optimal2050.github.io/multimod/r/reference/as_pyomo.md), [`as_latex()`](https://optimal2050.github.io/multimod/r/reference/as_latex.md) for any node; [`write_gams()`](https://optimal2050.github.io/multimod/r/reference/write_gams.md), [`write_gmpl()`](https://optimal2050.github.io/multimod/r/reference/write_gmpl.md), [`write_jump()`](https://optimal2050.github.io/multimod/r/reference/write_jump.md), [`write_pyomo()`](https://optimal2050.github.io/multimod/r/reference/write_pyomo.md), [`write_latex()`](https://optimal2050.github.io/multimod/r/reference/write_latex.md) for the model |
| Solve in R | [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md), [`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.md), [`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.md), [`solve_mps()`](https://optimal2050.github.io/multimod/r/reference/solve_mps.md), [`check_matrix_numbers()`](https://optimal2050.github.io/multimod/r/reference/check_matrix_numbers.md), [`mm_session()`](https://optimal2050.github.io/multimod/r/reference/mm_session.md) for repeated solves |
| Solve through generated code | [`solve_gmpl()`](https://optimal2050.github.io/multimod/r/reference/solve_gmpl.md), [`solve_jump()`](https://optimal2050.github.io/multimod/r/reference/solve_jump.md), [`solve_pyomo()`](https://optimal2050.github.io/multimod/r/reference/solve_pyomo.md), each loading the solution back |
| Reduce | [`fold_model()`](https://optimal2050.github.io/multimod/r/reference/fold_model.md) drops dimensions a parameter does not vary over; [`trim_model()`](https://optimal2050.github.io/multimod/r/reference/trim_model.md) drops unused symbols |
| Store and exchange | [`save_model()`](https://optimal2050.github.io/multimod/r/reference/save_model.md), [`load_model()`](https://optimal2050.github.io/multimod/r/reference/load_model.md), [`get_data()`](https://optimal2050.github.io/multimod/r/reference/get_data.md) (CSV, Arrow IPC, Parquet); [`model_to_json()`](https://optimal2050.github.io/multimod/r/reference/model_to_json.md), [`ast_schema()`](https://optimal2050.github.io/multimod/r/reference/ast_schema.md) |

Linear programs with continuous variables are the tested class.
Structural model reduction
([`fold_model()`](https://optimal2050.github.io/multimod/r/reference/fold_model.md),
[`trim_model()`](https://optimal2050.github.io/multimod/r/reference/trim_model.md))
is distinct from numerical solver presolving. GAMS output is readable
but not yet runnable without edits, and there is no Pyomo reader; the
[roadmap](https://optimal2050.github.io/multimod/r/articles/roadmap.html)
lists what else is not done yet.

## Documentation and next steps

- [`vignette("multimod")`](https://optimal2050.github.io/multimod/r/articles/multimod.md):
  reading, translating, analyzing, exporting and solving a model in a
  few calls.
- [Writing a model in
  R](https://optimal2050.github.io/multimod/r/articles/ast.html): the
  constructors, step by step.
- [Folding and
  trimming](https://optimal2050.github.io/multimod/r/articles/folding.html):
  reducing a model to the size its data needs.
- Further articles on the
  [website](https://optimal2050.github.io/multimod/r/): solving and
  reading results, the two field models, the model workspace,
  configuration, LaTeX.

## Development direction

The long-term architecture envisages a native C++ or Rust computational
core with R and Python interfaces. Future research directions include
AI-assisted formulation and support for nonlinear and stochastic
optimization. These are planned directions, not implemented features.

The repository is laid out for that architecture: the R package at the
root, `core/` and `python/` reserved for the native core and its Python
binding, `specs/` for the cross-language goldens, and one documentation
site under `docs/` with the R reference at
[`/r/`](https://optimal2050.github.io/multimod/r/) beside the future
language sections.

## Integration with energyRt

[energyRt](https://github.com/optimal2050/energyRt) uses multimod as its
model compiler:
[`energyRt::multimod_from_energyRt()`](https://energyRt.org/reference/multimod_from_energyRt.html)
builds the model from an interpolated scenario, and the LP or MPS
multimod produces is what energyRt hands to a local solver or,
experimentally, to cloud hardware through the mmcloud package.

## License

Apache License 2.0; see `LICENSE.md`. The bundled `example_models`
dataset is derived from energyRt and remains under energyRt’s Apache
License 2.0; `inst/COPYRIGHTS` has the details, and the attribution for
OSeMOSYS, which the documentation reads from its public repository and
does not bundle.
