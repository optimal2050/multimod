# Changelog

## multimod 0.6.0

First CRAN release. The changes are listed against 0.0.1, the GitHub
release of June 2025, which read GAMS and rendered models to LaTeX and
back to GAMS.

### Breaking changes

- multimod requires R \>= 4.1.0, and compiles C++17 code
  (`LinkingTo: cpp11`).
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
- A mapping can be defined by a rule instead of a table:
  [`mapping_rule()`](https://optimal2050.github.io/multimod/r/reference/mapping_rule.md)
  names the tables whose natural join, gated and projected, gives the
  tuples, and `new_mapping(rule = )` carries it.
  [`materialise_rule()`](https://optimal2050.github.io/multimod/r/reference/mapping_rule.md)
  enumerates the tuples;
  [`get_data()`](https://optimal2050.github.io/multimod/r/reference/get_data.md),
  the writers and the matrix route do so on demand.
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

#### The matrix route

- [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md)
  assembles the LP matrix directly from the model, including symbols
  declared over sets, equation conditions, lags and leads, and formula
  parameters. Index members are joined as integer codes of one
  dictionary per build.
- [`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.md)
  solves the assembled LP in R and returns primal and dual values;
  [`mm_session()`](https://optimal2050.github.io/multimod/r/reference/mm_session.md)
  keeps the matrix between repeated solves.
- [`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.md)
  writes the LP as MPS, with an optional presolve
  ([`lp_presolve()`](https://optimal2050.github.io/multimod/r/reference/lp_presolve.md))
  that eliminates doubleton rows and proportional columns. With
  `stream = TRUE` it builds the matrix block by block and writes the
  file itself, spooling the entries to bucket files by column range, so
  the matrix is never held in memory; the presolve is then applied to
  the spool.
- `write_mps(substitute = )` substitutes definition equations while the
  matrix is built: the defined variable has no columns and its
  definition replaces every reference to it;
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md)
  rebuilds it.
  [`objective_chain()`](https://optimal2050.github.io/multimod/r/reference/objective_chain.md)
  lists the definitions the objective depends on.
- For performance on large models, the matrix route includes C++17
  implementations of the supported equation executor, streamed
  coefficient spool and MPS writer, and the scan, presolve-map and remap
  steps used by streamed presolve. Native equation execution is opt-in
  with `options(multimod.engine = "native")`; unsupported equations fall
  back to the R engine individually. The executor runs on two threads by
  default (`options(multimod.threads = )`), slices large expansions with
  `options(multimod.slice_rows = )`, and can cap its index cache with
  `options(multimod.cache_gb = )`.
- [`lp_bounds()`](https://optimal2050.github.io/multimod/r/reference/lp_bounds.md)
  propagates the row and column bounds of an LP to a fixed point and
  returns the implied interval of every variable.
- [`check_matrix_numbers()`](https://optimal2050.github.io/multimod/r/reference/check_matrix_numbers.md)
  reports the coefficient range of an assembled LP.
- [`solve_mps()`](https://optimal2050.github.io/multimod/r/reference/solve_mps.md)
  solves an MPS file with HiGHS in this process, HiGHS through Julia, or
  cuOpt on this machine or inside WSL. cuOpt runs under a watchdog that
  stops a diverging or stalled solve
  ([`cuopt_watchdog()`](https://optimal2050.github.io/multimod/r/reference/cuopt_watchdog.md)),
  and can use its Python interface, which writes the solution at any
  status and saves and continues from PDLP’s warm-start data.
- [`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md),
  [`read_highs_solution()`](https://optimal2050.github.io/multimod/r/reference/read_highs_solution.md),
  [`read_cuopt_solution()`](https://optimal2050.github.io/multimod/r/reference/read_cuopt_solution.md)
  and
  [`read_solver_solution()`](https://optimal2050.github.io/multimod/r/reference/read_solver_solution.md)
  read solutions back against the index tables, by position or, for a
  matrix written with names, by label.

#### Solving through other languages

- [`solve_gmpl()`](https://optimal2050.github.io/multimod/r/reference/solve_gmpl.md),
  [`solve_jump()`](https://optimal2050.github.io/multimod/r/reference/solve_jump.md)
  and
  [`solve_pyomo()`](https://optimal2050.github.io/multimod/r/reference/solve_pyomo.md)
  run the generated code and load the solution into the model. None of
  them installs software: the generated JuMP code stops and names
  missing Julia packages, and
  [`solve_pyomo()`](https://optimal2050.github.io/multimod/r/reference/solve_pyomo.md)
  stops when the interpreter cannot import Pyomo.
  [`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md)
  creates a Python environment for Pyomo when called.

#### Workspace, reduction and exchange

- [`save_model()`](https://optimal2050.github.io/multimod/r/reference/save_model.md),
  [`load_model()`](https://optimal2050.github.io/multimod/r/reference/load_model.md)
  and
  [`get_data()`](https://optimal2050.github.io/multimod/r/reference/get_data.md)
  store a model and its data as Parquet (default), Arrow IPC or CSV
  files that the generated code reads directly. The structure is written
  as `model.rds`, `model.json` or both (`structure =`);
  `load_model(canonical = TRUE)` orders set members and mapping rows so
  the matrix does not depend on who wrote the directory.
- [`get_data()`](https://optimal2050.github.io/multimod/r/reference/get_data.md)
  keeps the tables it reads from disk in a session memo within
  `options(multimod.data_cache_gb = 8)`;
  [`clear_data_cache()`](https://optimal2050.github.io/multimod/r/reference/clear_data_cache.md)
  and
  [`data_cache_info()`](https://optimal2050.github.io/multimod/r/reference/clear_data_cache.md)
  manage it, and
  [`load_model_data()`](https://optimal2050.github.io/multimod/r/reference/load_model_data.md)
  brings a model’s tables into memory through it.
- Writing functions write only where they are told:
  [`write_pyomo()`](https://optimal2050.github.io/multimod/r/reference/write_pyomo.md)
  needs `model_dir`,
  [`write_gmpl()`](https://optimal2050.github.io/multimod/r/reference/write_gmpl.md)
  writes a directory only when `model_dir` is given, and
  [`import_gmpl_data()`](https://optimal2050.github.io/multimod/r/reference/import_gmpl_data.md)
  needs `model_path` to save CSV or Arrow data.
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
- [`compare_jump_stats()`](https://optimal2050.github.io/multimod/r/reference/compare_jump_stats.md)
  compares the constraint statistics of two JuMP runs, and writes the
  comparison when given a `file`.

#### Configuration

- Interpreter and solver paths resolve from R options, environment
  variables, a configuration file and detection, in that order; see
  [`multimod_config_show()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md).
  [`multimod_config_write()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md)
  writes a configuration file to the path it is given.
- [`mm_check_python()`](https://optimal2050.github.io/multimod/r/reference/mm_check_python.md)
  reports whether the configured interpreter is usable.
- The comparison helpers,
  [`multimod_config_show()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md)
  and
  [`populate_sets_from_data()`](https://optimal2050.github.io/multimod/r/reference/populate_sets_from_data.md)
  print only when `verbose` is `TRUE`; its default is the option
  `multimod.verbose` (`TRUE`).
- The `glpsol` executable is taken from the ‘glpsol’ package when it is
  installed.

#### Documentation

- [`vignette("multimod")`](https://optimal2050.github.io/multimod/r/articles/multimod.md)
  takes a small model through reading, translation, redundancy analysis,
  LaTeX, export and solving. Website articles cover writing a model in
  R, model redundancy, solving and reading results, two published
  models, the model workspace, configuration and LaTeX rendering.
- The website moved to <https://optimal2050.github.io/multimod/r/>; the
  root is a landing page for the languages the repository is laid out
  for. Links to the old addresses are forwarded.

### Bug fixes

- Arrow IPC files are written with `write_ipc_file()` and read with
  `read_ipc_file()`, so arrow \>= 26 no longer warns that
  `write_feather()` and `read_feather()` are superseded; arrow \>= 9.0.0
  is required. The format is called `"ipc"` throughout, and `"feather"`
  is no longer a format value.
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
