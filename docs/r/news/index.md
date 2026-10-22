# Changelog

## multimod 0.5.0

First CRAN release. The changes are listed against 0.0.1, the GitHub
release of June 2025, which read GAMS and rendered models to LaTeX and
back to GAMS.

### Breaking changes

- The S3 classes of model objects carry an `mm_` prefix: `mm_model`,
  `mm_set`, `mm_mapping`, `mm_parameter`, `mm_variable`, `mm_equation`.
  Code testing `inherits(x, "model")` must use the new names; models
  saved with 0.0.1 must be rebuilt.
- The constructors validate each node against the schema when it is
  built, so a malformed node is refused there.
  `options(multimod.validate = FALSE)` disables the check.
- `arrow` and `visNetwork` are suggested rather than imported; the
  functions that need them fail with an installation hint.

### New features

#### Building and reading models

- [`mm_equation()`](https://optimal2050.github.io/multimod/r/reference/mm_equation.md)
  builds an equation from MathProg, GAMS or JuMP text.
- [`read_gmpl()`](https://optimal2050.github.io/multimod/r/reference/read_gmpl.md)
  reads MathProg models with their data sections, computed parameters
  and parametric defaults.
- [`parse_jump_equation()`](https://optimal2050.github.io/multimod/r/reference/parse_jump_equation.md)
  reads a JuMP `@constraint()` statement.
- [`read_gams()`](https://optimal2050.github.io/multimod/r/reference/read_gams.md)
  and
  [`read_gmpl()`](https://optimal2050.github.io/multimod/r/reference/read_gmpl.md)
  accept text as well as file paths.
- Lags and leads are linear by default and wrap with `circular = TRUE`;
  GAMS reads and writes both forms (`y-1`, `y--1`).
- `example_models$toy` is a small capacity-expansion model built with
  the constructors.

#### Rendering

- [`as_gmpl()`](https://optimal2050.github.io/multimod/r/reference/as_gmpl.md),
  [`as_pyomo()`](https://optimal2050.github.io/multimod/r/reference/as_pyomo.md)
  and
  [`write_gmpl()`](https://optimal2050.github.io/multimod/r/reference/write_gmpl.md),
  [`write_jump()`](https://optimal2050.github.io/multimod/r/reference/write_jump.md),
  [`write_pyomo()`](https://optimal2050.github.io/multimod/r/reference/write_pyomo.md)
  render a model to MathProg, JuMP and Pyomo, with its data embedded or
  read from CSV, Arrow IPC or Parquet files.
- [`write_gams_data()`](https://optimal2050.github.io/multimod/r/reference/write_gams_data.md)
  (experimental) writes a model’s data to a GDX file, and
  `write_gams(gdx =, include_solve =)` emits the matching load and solve
  statements. Parameters defined by a formula are written as
  assignments.

#### Solving

- [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md)
  assembles the LP matrix directly from the model, including symbols
  declared over sets, equation conditions, lags and leads, and formula
  parameters.
- [`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.md)
  solves the assembled LP in R and returns primal and dual values;
  [`mm_session()`](https://optimal2050.github.io/multimod/r/reference/mm_session.md)
  keeps the matrix between repeated solves.
- [`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.md)
  writes the LP as MPS, with an optional presolve
  ([`lp_presolve()`](https://optimal2050.github.io/multimod/r/reference/lp_presolve.md))
  that eliminates proportional columns;
  [`solve_mps()`](https://optimal2050.github.io/multimod/r/reference/solve_mps.md)
  solves an MPS file and
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md),
  [`read_highs_solution()`](https://optimal2050.github.io/multimod/r/reference/read_highs_solution.md),
  [`read_cuopt_solution()`](https://optimal2050.github.io/multimod/r/reference/read_cuopt_solution.md)
  and
  [`read_solver_solution()`](https://optimal2050.github.io/multimod/r/reference/read_solver_solution.md)
  read solutions back.
- [`check_matrix_numbers()`](https://optimal2050.github.io/multimod/r/reference/check_matrix_numbers.md)
  reports the coefficient range of an assembled LP.
- [`solve_gmpl()`](https://optimal2050.github.io/multimod/r/reference/solve_gmpl.md),
  [`solve_jump()`](https://optimal2050.github.io/multimod/r/reference/solve_jump.md)
  and
  [`solve_pyomo()`](https://optimal2050.github.io/multimod/r/reference/solve_pyomo.md)
  run the generated code and load the solution into the model.
  [`solve_pyomo()`](https://optimal2050.github.io/multimod/r/reference/solve_pyomo.md)
  uses the configured Python interpreter; `ensure_env = TRUE` creates
  the environment with
  [`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md)
  when it is not usable.

#### Workspace, reduction and exchange

- [`save_model()`](https://optimal2050.github.io/multimod/r/reference/save_model.md),
  [`load_model()`](https://optimal2050.github.io/multimod/r/reference/load_model.md)
  and
  [`get_data()`](https://optimal2050.github.io/multimod/r/reference/get_data.md)
  store a model and its data as CSV, Arrow IPC or Parquet files that the
  generated code reads directly.
- [`fold_model()`](https://optimal2050.github.io/multimod/r/reference/fold_model.md)
  drops dimensions a parameter does not vary over;
  [`trim_model()`](https://optimal2050.github.io/multimod/r/reference/trim_model.md)
  drops unused symbols.
  [`unfold_model()`](https://optimal2050.github.io/multimod/r/reference/unfold_model.md)
  and
  [`untrim_model()`](https://optimal2050.github.io/multimod/r/reference/untrim_model.md)
  reverse them.
- [`model_to_json()`](https://optimal2050.github.io/multimod/r/reference/model_to_json.md)
  and
  [`model_from_json()`](https://optimal2050.github.io/multimod/r/reference/model_from_json.md)
  serialise a model;
  [`ast_schema()`](https://optimal2050.github.io/multimod/r/reference/ast_schema.md)
  and
  [`ast_json_schema()`](https://optimal2050.github.io/multimod/r/reference/ast_json_schema.md)
  describe the node shapes, and
  [`validate_ast()`](https://optimal2050.github.io/multimod/r/reference/validate_ast.md)
  checks a node against them.
- CamelCase constructors
  ([`Set()`](https://optimal2050.github.io/multimod/r/reference/multimod-constructors.md),
  [`Parameter()`](https://optimal2050.github.io/multimod/r/reference/multimod-constructors.md),
  [`Equation()`](https://optimal2050.github.io/multimod/r/reference/multimod-constructors.md),
  …) are provided as aliases of `new_*()` and `ast_*()`.

#### Configuration

- Interpreter and solver paths resolve from R options, environment
  variables, a configuration file and auto-detection, in that order; see
  [`multimod_config_write()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md)
  and
  [`multimod_config_show()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md).
- [`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md)
  creates a Python environment for Pyomo, and
  [`mm_check_python()`](https://optimal2050.github.io/multimod/r/reference/mm_check_python.md)
  reports whether the configured one is usable.
- The `glpsol` executable is taken from the ‘glpsol’ package when it is
  installed.

#### Documentation

- [`vignette("multimod")`](https://optimal2050.github.io/multimod/r/articles/multimod.md)
  takes a small model through reading, translation, redundancy analysis,
  LaTeX, export and solving. Website articles cover writing a model in
  R, folding and trimming, solving and reading results, two published
  models, the model workspace, configuration and LaTeX rendering.
- The website moved to <https://optimal2050.github.io/multimod/r/>; the
  root is a landing page for the languages the repository is laid out
  for. Links to the old addresses are forwarded.

### Bug fixes

- [`read_gams()`](https://optimal2050.github.io/multimod/r/reference/read_gams.md)
  parses a sum over several indices, numbers in scientific notation with
  a signed exponent, and decimal literals in equation bodies.
- [`read_gams()`](https://optimal2050.github.io/multimod/r/reference/read_gams.md)
  reads several symbols declared on one line (`set tech, region;`), and
  an equation header without a `$` condition.
- `^` no longer distributes over the terms of its base, so `(1 + r)^n`
  is evaluated as written.
- [`as_gams()`](https://optimal2050.github.io/multimod/r/reference/as_gams.md)
  output compiles: conditions are bracketed, function arguments and
  multi-index sums are rendered, and iterated minima and maxima are
  written as `smin()` and `smax()`.
- [`write_gams()`](https://optimal2050.github.io/multimod/r/reference/write_gams.md)
  declares each equation over its sets, as GAMS requires, and writes no
  empty `sets` block.
- [`as_latex()`](https://optimal2050.github.io/multimod/r/reference/as_latex.md)
  renders an equation’s index names in its header.
- [`ast_dims()`](https://optimal2050.github.io/multimod/r/reference/ast_dims.md)
  accepts a single node, and returns a `dims` node unchanged.

------------------------------------------------------------------------

## multimod 0.0.1 (2025-06-01)

- Initial GitHub release (June 1, 2025).
