# Changelog

## multimod 0.5.0

### Breaking changes

- The S3 classes of model objects are now prefixed: `mm_model`,
  `mm_set`, `mm_mapping`, `mm_parameter`, `mm_variable`, `mm_equation`
  (declarations and references alike). The bare names collided with
  other packages’ classes (energyRt’s `model` and `variable`), so that
  methods of one package were dispatched on objects of the other. Code
  testing `inherits(x, "model")` and friends must use the new names;
  [`is_declaration()`](https://optimal2050.github.io/multimod/reference/is_declaration.md)
  and the `is_*()` helpers are unchanged. Saved models carry their
  class, so `example_models` was re-stamped and models saved with
  earlier versions need the same (`class<-` on the nested nodes). The
  JSON schema tags and the CamelCase constructors (`Model()`,
  [`Set()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md),
  …) are unchanged.
- `arrow` and `visNetwork` moved from Imports to Suggests; the functions
  that need them check and fail with an installation hint.
- `multimod_config_write(global = TRUE)` and its readers use the
  per-user configuration directory returned by
  `tools::R_user_dir("multimod", "config")` instead of `~/.multimod/`.

### CRAN preparation

- The four articles that need energyRt or an installed solver (folding,
  GMPL, JuMP, Pyomo) are website articles rather than package vignettes.

- Tests run under `R CMD check` (a test runner was missing).

- `setup_python_environment()` and `setup_multimod_python()` are now
  [`mm_setup_python()`](https://optimal2050.github.io/multimod/reference/mm_setup_python.md),
  with no aliases. Its `packages` default gains `pyarrow` and `pandas`,
  so the environment can read the Arrow data
  `save_model(format = "ipc")` writes, and `python_version` defaults to
  3.14. `solve_pyomo(packages = )` now defaults to `NULL` and defers to
  it, so the environment contents have one owner. `conda_packages`
  defaults to `"glpk"`, installed with conda rather than `pip`, because
  the MathProg route needs the `glpsol` executable;
  [`get_multimod_glpsol()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  now also looks inside the environment.

- [`analyze_fold_opportunities()`](https://optimal2050.github.io/multimod/reference/analyze_fold_opportunities.md):
  `fold_slice` is now `fold_timeslice`, and the `slice` dimension is
  spelled `timeslice` throughout, matching energyRt. The old spelling
  was dead code, so the timeslice-coverage check never ran.

- Example models are consolidated into `example_models`: use
  `example_models$energyRt$multimod` in place of the removed
  `utopia_multimod` and its siblings.

### Bug fixes

- A symbol declared as a subset (`sup(process)`) is registered as both a
  set and a mapping, and only one side carries its members. Every writer
  now emits it from the side that has them. Previously the MathProg data
  section and the GDX took the empty side, so gates such as `mLoComm`
  were written empty and the equations using them were silently inert.

- Pyomo: a gated `sum` kept its condition, a mapping in condition
  position is a membership test rather than a truthy `Set`,
  `ord()`/`card()` resolve to a position parameter, and rule iterators
  are taken from the equation’s domain mapping, so an alias such as
  `dst` is bound.
  [`solve_pyomo()`](https://optimal2050.github.io/multimod/reference/solve_pyomo.md)
  passes environment variables through
  [`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html) rather than
  `system2(env =)`, which Windows ignores, so `solver =` is no longer
  silently dropped.

- `save_model(format = "ipc")` now works with Pyomo: the generated
  loader reads each file directly instead of through `DataPortal`, which
  has no reader for Arrow or Parquet.

- GAMS output compiles: `$` conditions are bracketed, descriptions
  containing a comma are quoted, a dims tuple is not double-wrapped,
  equation headers bind the domain’s dims, and a unary operator reads
  its operand from `$rhs`.

- [`fold_model()`](https://optimal2050.github.io/multimod/reference/fold_model.md)
  refuses to fold a dimension when the parameter never indexes some of
  its members and no validation mapping shows they are unreachable.
  Folding one broadcast a stored value where the default applied.

- [`fold_equations()`](https://optimal2050.github.io/multimod/reference/fold_equations.md)
  now rewrites parameter references inside a unary operator.
  `vExportRowCost = -sum{...}` and two of the three terms of
  `eqSubsCost` kept the unfolded arity, which MathProg rejects and which
  JuMP and Pyomo accepted while silently substituting the default
  weight.

- [`fold_model()`](https://optimal2050.github.io/multimod/reference/fold_model.md)
  refuses a dimension the parameter carries more than once, such as
  `timeslice` in `pTimesliceAgg(year, timeslice, timeslicep)` or
  `region` in the `pTradeIr*` family. Both the declaration rewrite and
  the reference rewrite drop every matching dimension, so folding
  collapsed the pair into one subscript.

### New features

#### GAMS

- [`write_gams_data()`](https://optimal2050.github.io/multimod/reference/write_gams_data.md)
  writes a model’s data to a GDX, and
  `write_gams(gdx =, include_solve =)` emits the matching `$load` block
  and the `model` / `solve` statements, so a GAMS job can be assembled
  without a GAMS licence and solved elsewhere. Zero-valued parameter
  records are written as `EPS`: a GDX stores no zeros, and GAMS has no
  per-parameter default, so the default assigned after loading would
  otherwise overwrite a stored zero. This route is exercised against an
  older GDX library and gdxtools 0.7, which does not install on R \>=
  4.6, with the solve run on NEOS; contributions testing newer pipelines
  are welcome.

- [`mm_check_python()`](https://optimal2050.github.io/multimod/reference/mm_check_python.md)
  reports the interpreter multimod would use and whether it carries the
  needed packages.
  [`solve_pyomo()`](https://optimal2050.github.io/multimod/reference/solve_pyomo.md)
  calls it before bootstrapping, so a usable environment is no longer
  reinstalled on every solve.

#### Direct matrix / MPS backend

- Assemble a model straight into LP arrays, bypassing the symbolic
  layer:
  [`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md),
  [`build_col_index()`](https://optimal2050.github.io/multimod/reference/build_col_index.md),
  [`build_row_index()`](https://optimal2050.github.io/multimod/reference/build_row_index.md),
  [`build_triplets()`](https://optimal2050.github.io/multimod/reference/build_triplets.md).
- [`write_mps()`](https://optimal2050.github.io/multimod/reference/write_mps.md),
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/reference/read_mps_solution.md),
  [`write_energyrt_output()`](https://optimal2050.github.io/multimod/reference/write_energyrt_output.md)
  for the file round trip;
  [`solve_highs()`](https://optimal2050.github.io/multimod/reference/solve_highs.md)
  for an in-process solve returning duals.
- New
  [`solve_mps()`](https://optimal2050.github.io/multimod/reference/solve_mps.md):
  solve a written MPS file locally with HiGHS and emit a solution file
  that
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/reference/read_mps_solution.md)
  consumes – the local counterpart of the cloud solve leg.
- [`multimod_from_energyRt()`](https://energyRt.org/reference/multimod_from_energyRt.html)
  builds a model from an interpolated energyRt scenario in one call,
  resolving the variable domains the GAMS `*@` comments do not carry.
- energyRt user constraints and costs
  ([`newConstraint()`](https://energyRt.org/reference/newConstraint.html),
  [`newCosts()`](https://energyRt.org/reference/newCosts.html)) are
  supported.
- [`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md)
  gains `on_empty_row`: an LP in which a user constraint carries no
  coefficients is refused by default, and any other empty row warns.
  `"all"`, `"warn"` and `"ignore"` select other policies.
- The matrix evaluator supports the GAMS `<set>.val` intrinsic, and
  `val()`/`ord()` resolve short index aliases (`y` -\> `year`) and alias
  groups.
- [`check_matrix_numbers()`](https://optimal2050.github.io/multimod/reference/check_matrix_numbers.md)
  reports the magnitude spectrum of an assembled LP, with offender
  tables by equation and variable.
- [`read_cuopt_solution()`](https://optimal2050.github.io/multimod/reference/read_cuopt_solution.md)
  reads cuOpt’s solution format, and
  [`read_solver_solution()`](https://optimal2050.github.io/multimod/reference/read_solver_solution.md)
  dispatches on the file itself (`solver = "auto"`).
- [`write_energyrt_output()`](https://optimal2050.github.io/multimod/reference/write_energyrt_output.md)
  also emits `raw_data_set.csv` and `log.csv`, the files
  [`energyRt::read_solution()`](https://energyRt.org/reference/read.html)
  requires beside the per-variable tables.

See `dev/mps-pipeline-and-issues.md` for the pipeline, measurements and
open items.

#### Data integration and storage

- [`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md)
  /
  [`load_model()`](https://optimal2050.github.io/multimod/reference/load_model.md)
  store a model workspace as Arrow IPC, Parquet or CSV, with lazy
  loading for large datasets. See
  [`vignette("model_workspace")`](https://optimal2050.github.io/multimod/articles/model_workspace.md).
- `import_energyRt_data()`, `populate_sets_from_scenario()` and
  `link_scenario_data()` import energyRt structures and link
  Arrow/Parquet files for lazy loading.
- Generated code carries data either embedded in the code (small models)
  or read from an Arrow repository (large models).
- Constructors are also available under the cross-language names used by
  the planned C++ and Python bindings:
  [`Set()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md),
  [`Parameter()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md),
  [`Equation()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md),
  [`SetRef()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  and so on. These are aliases; the `new_*()` and `ast_*()` names are
  unchanged.
  [`is_declaration()`](https://optimal2050.github.io/multimod/reference/is_declaration.md)
  replaces `inherits(x, "multimod")`.
- AST nodes are validated against the schema when constructed, so a
  malformed node is refused where it is built rather than surfacing
  later as a renderer that emits nothing. Disable with
  `options(multimod.validate = FALSE)`.
- Language-neutral serialisation of the AST:
  [`model_to_json()`](https://optimal2050.github.io/multimod/reference/model_to_json.md)
  /
  [`model_from_json()`](https://optimal2050.github.io/multimod/reference/model_from_json.md),
  with the node shapes declared once in
  [`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md)
  and a JSON Schema emitted by
  [`ast_json_schema()`](https://optimal2050.github.io/multimod/reference/ast_json_schema.md).
  [`validate_ast()`](https://optimal2050.github.io/multimod/reference/validate_ast.md)
  checks a node against the schema. Previously structure was only ever
  written as `.rds`.

#### Code generation

- Pyomo code generation: abstract models with external data loading, and
  any solver Pyomo supports. See `vignette("pyomo")`.
- [`write_jump()`](https://optimal2050.github.io/multimod/reference/write_jump.md)
  generates modern JuMP syntax: named constraints, tuple indexing, short
  index aliases, variable bounds from GAMS types, and CSV diagnostics.
- [`write_gmpl()`](https://optimal2050.github.io/multimod/reference/write_gmpl.md)
  /
  [`write_gmpl_data()`](https://optimal2050.github.io/multimod/reference/write_gmpl_data.md)
  generate GMPL/MathProg, writing data lazily from Arrow/Parquet
  datasets.

#### Configuration and usability

- Solver paths resolve through R options, environment variables, a YAML
  config and auto-detection, in that order:
  [`get_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
  [`get_multimod_julia()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
  [`get_multimod_glpsol()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
  with
  [`multimod_config_write()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
  [`multimod_config_read()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  and
  [`multimod_config_show()`](https://optimal2050.github.io/multimod/reference/multimod-config.md).
  See
  [`vignette("configuration")`](https://optimal2050.github.io/multimod/articles/configuration.md).
- Reader functions
  ([`read_gams()`](https://optimal2050.github.io/multimod/reference/read_gams.md),
  [`read_gmpl()`](https://optimal2050.github.io/multimod/reference/read_gmpl.md),
  [`read_gmpl_data()`](https://optimal2050.github.io/multimod/reference/read_gmpl_data.md),
  [`import_gmpl_data()`](https://optimal2050.github.io/multimod/reference/import_gmpl_data.md))
  accept character vectors as well as file paths, so a model can be
  parsed from an R object without a temporary file.

#### Model optimization

- [`fold_model()`](https://optimal2050.github.io/multimod/reference/fold_model.md)
  reduces the dimensionality of uniform parameters;
  [`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md)
  and
  [`get_fold_summary()`](https://optimal2050.github.io/multimod/reference/get_fold_summary.md)
  support it. See `vignette("folding")`.
- [`trim_model()`](https://optimal2050.github.io/multimod/reference/trim_model.md)
  removes unused sets, parameters, variables and equations, and
  [`get_trim_summary()`](https://optimal2050.github.io/multimod/reference/get_trim_summary.md)
  reports what went.

### Bug fixes

- A constraint summing over more than one index was silently dropped
  from the model: the index list in `sum((region, timeslice)$map, ...)`
  kept the comma’s whitespace, so the second index matched no set and
  the sum bound nothing. The row still assembled, with its bounds and no
  coefficients, and solved to Optimal.
- Numeric literals in scientific notation with a signed exponent
  (`1e-20`, `3.6888e+08`) failed to parse.
- A decimal literal in an equation body (`=l= 3201.976`) was read as
  dot-access and became `val(y)`, building a wrong matrix.
- Folded scenarios (`fold = TRUE`) silently produced a wrong model: an
  `NA` wildcard matched nothing on join and fell back to the parameter
  default. Wildcards are now expanded on import, and one reaching the
  matrix errors.
- An empty or unlinked gating map could trigger a dense expansion of a
  summation index; emptiness is now tested before arity, and an
  undeclared mapping errors.
- Parameter values of `Inf` became infinite coefficients. multimod now
  drops those rows and maps an infinite default to 0, as every energyRt
  writer does.
- `import_energyRt_data()` reported “0 rows” for a fully readable
  on-disk scenario, and an import where nothing is reachable is now an
  error rather than a warning.
- [`get_data()`](https://optimal2050.github.io/multimod/reference/get_data.md)
  treated a legitimately empty on-disk table as unreadable and aborted
  the build. An empty table whose recorded path resolves nowhere now
  imports as empty.
- JuMP: 1-dimensional mappings loaded as `Set{Tuple{String}}`, so
  membership checks silently failed and constraints lost terms. They now
  load as `Set{String}`, and generated models reproduce the reference
  results.
- [`export_model_source()`](https://optimal2050.github.io/multimod/reference/export_model_source.md)
  and `export_energyrt()` work with the consolidated `example_models`.
- `DESCRIPTION` no longer carries a `Collate` field: it had fallen out
  of date, and files absent from it are silently not sourced.

### Documentation

- New vignettes: `configuration`, `pyomo`, `model_workspace`.
- Updated vignettes: `jump`, `gmpl`, `latex`.

------------------------------------------------------------------------

## multimod 0.0.1 (2025-06-01)

- Initial GitHub release (June 1, 2025).
