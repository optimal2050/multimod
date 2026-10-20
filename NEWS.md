# multimod 0.5.0

First CRAN release. The changes are listed against 0.0.1, the GitHub release
of June 2025, which read GAMS and rendered models to LaTeX and back to GAMS.

## Breaking changes

* The S3 classes of model objects carry an `mm_` prefix: `mm_model`,
  `mm_set`, `mm_mapping`, `mm_parameter`, `mm_variable`, `mm_equation`.
  Code testing `inherits(x, "model")` must use the new names; models saved
  with 0.0.1 must be rebuilt.
* The constructors validate each node against the schema when it is built,
  so a malformed node is refused there. `options(multimod.validate = FALSE)`
  disables the check.
* `arrow` and `visNetwork` are suggested rather than imported; the functions
  that need them fail with an installation hint.

## New features

### Building and reading models

* `mm_equation()` builds an equation from MathProg, GAMS or JuMP text.
* `read_gmpl()` reads MathProg models with their data sections, computed
  parameters and parametric defaults.
* `parse_jump_equation()` reads a JuMP `@constraint()` statement.
* `read_gams()` and `read_gmpl()` accept text as well as file paths.
* Lags and leads are linear by default and wrap with `circular = TRUE`;
  GAMS reads and writes both forms (`y-1`, `y--1`).
* `example_models$toy` is a small capacity-expansion model built with the
  constructors.

### Rendering

* `as_gmpl()`, `as_pyomo()` and `write_gmpl()`, `write_jump()`,
  `write_pyomo()` render a model to MathProg, JuMP and Pyomo, with its data
  embedded or read from CSV, Arrow IPC or Parquet files.
* `write_gams_data()` writes a model's data to a GDX file, and
  `write_gams(gdx =, include_solve =)` emits the matching load and solve
  statements. Parameters defined by a formula are written as assignments.

### Solving

* `model_to_lp()` assembles the LP matrix directly from the model, including
  symbols declared over sets, equation conditions, lags and leads, and
  formula parameters.
* `solve_highs()` solves the assembled LP in R and returns primal and dual
  values; `mm_session()` keeps the matrix between repeated solves.
* `write_mps()` writes the LP as MPS, with an optional presolve
  (`lp_presolve()`) that eliminates proportional columns; `solve_mps()`
  solves an MPS file and `read_mps_solution()`, `read_highs_solution()`,
  `read_cuopt_solution()` and `read_solver_solution()` read solutions back.
* `check_matrix_numbers()` reports the coefficient range of an assembled LP.
* `solve_gmpl()`, `solve_jump()` and `solve_pyomo()` run the generated code
  and load the solution into the model.

### Workspace, reduction and exchange

* `save_model()`, `load_model()` and `get_data()` store a model and its data
  as CSV, Arrow IPC or Parquet files that the generated code reads directly.
* `fold_model()` drops dimensions a parameter does not vary over;
  `trim_model()` drops unused symbols. `unfold_model()` and `untrim_model()`
  reverse them.
* `model_to_json()` and `model_from_json()` serialise a model;
  `ast_schema()` and `ast_json_schema()` describe the node shapes, and
  `validate_ast()` checks a node against them.
* CamelCase constructors (`Set()`, `Parameter()`, `Equation()`, ...) are
  provided as aliases of `new_*()` and `ast_*()`.

### Configuration

* Interpreter and solver paths resolve from R options, environment
  variables, a configuration file and auto-detection, in that order; see
  `multimod_config_write()` and `multimod_config_show()`.
* `mm_setup_python()` creates a Python environment for Pyomo, and
  `mm_check_python()` reports whether the configured one is usable.
* The `glpsol` executable is taken from the 'glpsol' package when it is
  installed.

### Documentation

* Vignettes: an overview, writing a model in R, and folding and trimming.
  Website articles cover solving and reading results, two published models,
  the model workspace, configuration and LaTeX rendering.

## Bug fixes

* `read_gams()` parses a sum over several indices, numbers in scientific
  notation with a signed exponent, and decimal literals in equation bodies.
* `^` no longer distributes over the terms of its base, so `(1 + r)^n` is
  evaluated as written.
* `as_gams()` output compiles: conditions are bracketed, function arguments
  and multi-index sums are rendered, and iterated minima and maxima are
  written as `smin()` and `smax()`.
* `as_latex()` renders an equation's index names in its header.
* `ast_dims()` accepts a single node.

---

# multimod 0.0.1 (2025-06-01)

* Initial GitHub release (June 1, 2025).
