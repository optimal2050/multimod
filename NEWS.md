# multimod (development version)

## Breaking changes

* `analyze_fold_opportunities()`: `fold_slice` is now `fold_timeslice`, and the
  `slice` dimension is spelled `timeslice` throughout, matching energyRt. The
  old spelling was dead code, so the timeslice-coverage check never ran.
* Example models are consolidated into `example_models`: use
  `example_models$energyRt$multimod` in place of the removed `utopia_multimod`
  and its siblings.

## New features

### Direct matrix / MPS backend

* Assemble a model straight into LP arrays, bypassing the symbolic layer:
  `model_to_lp()`, `build_col_index()`, `build_row_index()`, `build_triplets()`.
* `write_mps()`, `read_mps_solution()`, `write_energyrt_output()` for the file
  round trip; `solve_highs()` for an in-process solve returning duals.
* New `solve_mps()`: solve a written MPS file locally with HiGHS and emit a
  solution file that `read_mps_solution()` consumes -- the local counterpart of
  the cloud solve leg.
* `multimod_from_energyRt()` builds a model from an interpolated energyRt
  scenario in one call, resolving the variable domains the GAMS `*@` comments
  do not carry.
* energyRt user constraints and costs (`newConstraint()`, `newCosts()`) are
  supported.
* `model_to_lp()` gains `on_empty_row`: an LP in which a user constraint
  carries no coefficients is refused by default, and any other empty row
  warns. `"all"`, `"warn"` and `"ignore"` select other policies.
* The matrix evaluator supports the GAMS `<set>.val` intrinsic, and
  `val()`/`ord()` resolve short index aliases (`y` -> `year`) and alias groups.
* `check_matrix_numbers()` reports the magnitude spectrum of an assembled LP,
  with offender tables by equation and variable.
* `read_cuopt_solution()` reads cuOpt's solution format, and
  `read_solver_solution()` dispatches on the file itself (`solver = "auto"`).
* `write_energyrt_output()` also emits `raw_data_set.csv` and `log.csv`, the
  files `energyRt::read_solution()` requires beside the per-variable tables.

See `dev/mps-pipeline-and-issues.md` for the pipeline, measurements and open
items.

### Data integration and storage

* `save_model()` / `load_model()` store a model workspace as Arrow IPC,
  Parquet or CSV, with lazy loading for large datasets. See
  `vignette("model_workspace")`.
* `import_energyRt_data()`, `populate_sets_from_scenario()` and
  `link_scenario_data()` import energyRt structures and link Arrow/Parquet
  files for lazy loading; OSeMOSYS example data is supported.
* Generated code carries data either embedded in the code (small models) or
  read from an Arrow repository (large models).

### Code generation

* Pyomo code generation: abstract models with external data loading, and any
  solver Pyomo supports. See `vignette("pyomo")`.
* `write_jump()` generates modern JuMP syntax: named constraints, tuple
  indexing, short index aliases, variable bounds from GAMS types, and CSV
  diagnostics.
* `write_gmpl()` / `write_gmpl_data()` generate GMPL/MathProg, writing data
  lazily from Arrow/Parquet datasets.

### Configuration and usability

* Solver paths resolve through R options, environment variables, a YAML config
  and auto-detection, in that order: `get_multimod_python()`,
  `get_multimod_julia()`, `get_multimod_glpsol()`, with
  `multimod_config_write()`, `multimod_config_read()` and
  `multimod_config_show()`. See `vignette("configuration")`.
* Reader functions (`read_gams()`, `read_gmpl()`, `read_gmpl_data()`,
  `import_gmpl_data()`) accept character vectors as well as file paths, so a
  model can be parsed from an R object without a temporary file.

### Model optimization

* `fold_model()` reduces the dimensionality of uniform parameters;
  `create_fold_spec()` and `get_fold_summary()` support it. See
  `vignette("folding")`.
* `trim_model()` removes unused sets, parameters, variables and equations, and
  `get_trim_summary()` reports what went.

## Bug fixes

* A constraint summing over more than one index was silently dropped from the
  model: the index list in `sum((region, timeslice)$map, ...)` kept the comma's
  whitespace, so the second index matched no set and the sum bound nothing. The
  row still assembled, with its bounds and no coefficients, and solved to
  Optimal.
* Numeric literals in scientific notation with a signed exponent (`1e-20`,
  `3.6888e+08`) failed to parse.
* A decimal literal in an equation body (`=l= 3201.976`) was read as dot-access
  and became `val(y)`, building a wrong matrix.
* Folded scenarios (`fold = TRUE`) silently produced a wrong model: an `NA`
  wildcard matched nothing on join and fell back to the parameter default.
  Wildcards are now expanded on import, and one reaching the matrix errors.
* An empty or unlinked gating map could trigger a dense expansion of a
  summation index; emptiness is now tested before arity, and an undeclared
  mapping errors.
* Parameter values of `Inf` became infinite coefficients. multimod now drops
  those rows and maps an infinite default to 0, as every energyRt writer does.
* `import_energyRt_data()` reported "0 rows" for a fully readable on-disk
  scenario, and an import where nothing is reachable is now an error rather
  than a warning.
* `get_data()` treated a legitimately empty on-disk table as unreadable and
  aborted the build. An empty table whose recorded path resolves nowhere now
  imports as empty.
* JuMP: 1-dimensional mappings loaded as `Set{Tuple{String}}`, so membership
  checks silently failed and constraints lost terms. They now load as
  `Set{String}`, and generated models reproduce the reference results.
* `export_model_source()`, `export_osemosys()` and `export_energyrt()` work
  with the consolidated `example_models`.
* `DESCRIPTION` no longer carries a `Collate` field: it had fallen out of date,
  and files absent from it are silently not sourced.

## Documentation

* New vignettes: `configuration`, `pyomo`, `model_workspace`.
* Updated vignettes: `jump`, `gmpl`, `latex`.

---

# multimod 0.0.1 (2025-06-01)

* Initial GitHub release (June 1, 2025).
