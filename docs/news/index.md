# Changelog

## multimod (development version)

### Breaking changes

- [`analyze_fold_opportunities()`](https://optimal2050.github.io/multimod/reference/analyze_fold_opportunities.md):
  `fold_slice` is now `fold_timeslice`, and the `slice` dimension is
  spelled `timeslice` throughout, matching energyRt. The old spelling
  was dead code, so the timeslice-coverage check never ran.
- Example models are consolidated into `example_models`: use
  `example_models$energyRt$multimod` in place of the removed
  `utopia_multimod` and its siblings.

### New features

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
  Arrow/Parquet files for lazy loading; OSeMOSYS example data is
  supported.
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
  any solver Pyomo supports. See
  [`vignette("pyomo")`](https://optimal2050.github.io/multimod/articles/pyomo.md).
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

- `fold_model()` reduces the dimensionality of uniform parameters;
  [`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md)
  and
  [`get_fold_summary()`](https://optimal2050.github.io/multimod/reference/get_fold_summary.md)
  support it. See
  [`vignette("folding")`](https://optimal2050.github.io/multimod/articles/folding.md).
- `trim_model()` removes unused sets, parameters, variables and
  equations, and
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
- [`export_model_source()`](https://optimal2050.github.io/multimod/reference/export_model_source.md),
  `export_osemosys()` and `export_energyrt()` work with the consolidated
  `example_models`.
- `DESCRIPTION` no longer carries a `Collate` field: it had fallen out
  of date, and files absent from it are silently not sourced.

### Documentation

- New vignettes: `configuration`, `pyomo`, `model_workspace`.
- Updated vignettes: `jump`, `gmpl`, `latex`.

------------------------------------------------------------------------

## multimod 0.0.1 (2025-06-01)

- Initial GitHub release (June 1, 2025).
