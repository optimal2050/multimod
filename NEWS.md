# multimod 0.5.0

## Breaking changes

* The S3 classes of model objects are now prefixed: `mm_model`, `mm_set`,
  `mm_mapping`, `mm_parameter`, `mm_variable`, `mm_equation` (declarations
  and references alike). The bare names collided with other packages'
  classes (energyRt's `model` and `variable`), so that methods of one package
  were dispatched on objects of the other. Code testing `inherits(x,
  "model")` and friends must use the new names; `is_declaration()` and the
  `is_*()` helpers are unchanged. Saved models carry their class, so
  `example_models` was re-stamped and models saved with earlier versions
  need the same (`class<-` on the nested nodes). The JSON schema tags and the
  CamelCase constructors (`Model()`, `Set()`, ...) are unchanged.
* `arrow` and `visNetwork` moved from Imports to Suggests; the functions
  that need them check and fail with an installation hint.
* `multimod_config_write(global = TRUE)` and its readers use the per-user
  configuration directory returned by `tools::R_user_dir("multimod",
  "config")` instead of `~/.multimod/`.

## New features

* `mm_equation()` builds an equation from text rather than nested
  constructors, in MathProg (the default), GAMS or JuMP notation:
  `mm_equation(m, "eqCapLim{(tech,region) in mTR}: vGen[tech,region] <=
  pAvail[tech,region] * vCap[tech,region]")`. Declarations stay in R.
* `parse_jump_equation()` reads a JuMP `@constraint()` statement: generators
  with several iterators, tuple iterators over a mapping, an `if` filter,
  `if`/`else` conditionals and `get(p, i, default)`.
* `example_models$toy`: a small, fully populated capacity-expansion LP built
  with the constructors, used step by step in `vignette("ast")`.
* `vignette("ast")` is the guide to writing a model in R, from sets through
  validation to the matrix, file and JSON routes.

## Bug fixes

* MathProg: `*` and `/` belong inside an iterated operator, so
  `sum{l in L} x[l] * w[l]` sums the products; `+` and `-` end its scope.
* MathProg: `{(i,j) in S}` reads as one indexing specification, so
  `sum{(i,j) in S}` keeps both indices and an equation picks up its domain
  mapping.
* `as_gams()` renders the arguments of a function call, and brackets a
  multi-set sum index as `sum((tech,region), ...)`.
* `as_latex()` renders an equation's index names in its header, which came
  out as `\{\} \in mTR`.
* `as_gmpl()` names the iterators of a sum over an unnamed `ast_dims()`,
  renders a one-dimensional domain as `{r in mR}` as MathProg requires, and
  falls back to default iterators instead of erroring.
* The matrix backend evaluates the `sum`/`prod` nodes `ast_sum()` and
  `ast_prod()` build, and `ast_sum(domain = )` gates the summation.
* `untrim_model()` accepts `mm_model` objects.
* `get_active_dims()` returned no dimensions for a parameter read from a
  source file (an empty `active_dims`); it now returns the declared dims, and
  `folded = TRUE` returns `NULL` unless the parameter is folded.
* `unfold_model()` left `folded_equations` behind, so `is_folded()` stayed
  `TRUE`.
* `get_data(type = "set")` returned `NULL`: sets keep their members in `data`.
  It also reads them from a saved workspace.
* `export_model_source()` failed for every format: the dataset holds only the
  GAMS source. It now writes a model workspace with the GAMS source and
  MathProg, JuMP and Pyomo code rendered from the parsed model under
  `solvers/`.
* `write_pyomo()` loses its `file` argument, which was ignored. A `model_dir`
  that holds no workspace is written with `save_model()` first, so one call
  yields a runnable scaffold.
* `solve_pyomo(load_results = TRUE)` is implemented: variable values exported
  with `write_pyomo(export_vars = TRUE)` are read into the model, and the
  objective value is recorded in `status.json` and returned.
* `read_gmpl()` works again under schema validation: a parenthesised
  reference no longer carries a `brackets` field. The OSeMOSYS model (UTOPIA)
  reads, validates, and solves through JuMP to its published objective.
* `write_gmpl()` emitted `end;` before the solution-export block, so glpsol
  never wrote the per-variable CSVs and `solve_gmpl()` loaded nothing. Export
  loops over two dimensions starting with the same letter (`tech`,
  `timeslice`) produced a repeated iterator, which MathProg rejects.
* `write_jump()` and `write_pyomo()` create the workspace the generated code
  reads when `model_dir` is empty or holds only earlier solver output.
* JuMP: a sum's own iterator now shadows an outer iterator over the same set
  (`rr` next to `r`); formula parameters are keyed by their declared
  iterators; a parametric default (`default DiscountRate[r]`) is rendered
  inline from parameter references as well as from symbols.
* `solve_jump()` reads the `solution/<variable>.csv` layout `model.jl`
  writes and names the key columns after the variable's dimensions.
* Five `stop()` calls in `read_gams()` had a trailing comma that masked the
  message.
* `solve_gmpl()` reads glpsol's printable solution correctly when a column
  name is long enough to be wrapped onto two lines; it used to misread those
  rows and warn once per row.

## Documentation

* The GMPL, JuMP and Pyomo articles are replaced by "Solving and reading
  results", one narrative from the model object through the workspace, the
  generated code, the solve and the results to the checks for disagreeing
  routes, on `example_models$toy`; and by "Two models from the field", which
  runs the energyRt reference model and OSeMOSYS (read from its repository;
  see `LICENSE.note`). The configuration,
  workspace and LaTeX articles are rewritten on the toy model with evaluated
  code; the README describes what is tested, and the reference index groups
  the untested exports under "Experimental".

## CRAN preparation

* The four articles that need energyRt or an installed solver (folding,
  GMPL, JuMP, Pyomo) are website articles rather than package vignettes.
* Tests run under `R CMD check` (a test runner was missing).

* `setup_python_environment()` and `setup_multimod_python()` are now
  `mm_setup_python()`, with no aliases. Its `packages` default gains `pyarrow`
  and `pandas`, so the environment can read the Arrow data
  `save_model(format = "ipc")` writes, and `python_version` defaults to 3.14.
  `solve_pyomo(packages = )` now defaults to `NULL` and defers to it, so the
  environment contents have one owner.
  `conda_packages` defaults to `"glpk"`, installed with conda rather than
  `pip`, because the MathProg route needs the `glpsol` executable;
  `get_multimod_glpsol()` now also looks inside the environment.
* `analyze_fold_opportunities()`: `fold_slice` is now `fold_timeslice`, and the
  `slice` dimension is spelled `timeslice` throughout, matching energyRt. The
  old spelling was dead code, so the timeslice-coverage check never ran.
* Example models are consolidated into `example_models`: use
  `example_models$energyRt$multimod` in place of the removed `utopia_multimod`
  and its siblings.

## Bug fixes

* A symbol declared as a subset (`sup(process)`) is registered as both a set
  and a mapping, and only one side carries its members. Every writer now
  emits it from the side that has them. Previously the MathProg data section
  and the GDX took the empty side, so gates such as `mLoComm` were written
  empty and the equations using them were silently inert.

* Pyomo: a gated `sum` kept its condition, a mapping in condition position is
  a membership test rather than a truthy `Set`, `ord()`/`card()` resolve to a
  position parameter, and rule iterators are taken from the equation's domain
  mapping, so an alias such as `dst` is bound. `solve_pyomo()` passes
  environment variables through `Sys.setenv()` rather than `system2(env =)`,
  which Windows ignores, so `solver =` is no longer silently dropped.

* `save_model(format = "ipc")` now works with Pyomo: the generated loader
  reads each file directly instead of through `DataPortal`, which has no
  reader for Arrow or Parquet.

* GAMS output compiles: `$` conditions are bracketed, descriptions containing
  a comma are quoted, a dims tuple is not double-wrapped, equation headers
  bind the domain's dims, and a unary operator reads its operand from `$rhs`.

* `fold_model()` refuses to fold a dimension when the parameter never indexes
  some of its members and no validation mapping shows they are unreachable.
  Folding one broadcast a stored value where the default applied.

* `fold_equations()` now rewrites parameter references inside a unary
  operator. `vExportRowCost = -sum{...}` and two of the three terms of
  `eqSubsCost` kept the unfolded arity, which MathProg rejects and which
  JuMP and Pyomo accepted while silently substituting the default weight.

* `fold_model()` refuses a dimension the parameter carries more than once,
  such as `timeslice` in `pTimesliceAgg(year, timeslice, timeslicep)` or
  `region` in the `pTradeIr*` family. Both the declaration rewrite and the
  reference rewrite drop every matching dimension, so folding collapsed the
  pair into one subscript.

## New features

### GAMS

* `write_gams_data()` writes a model's data to a GDX, and
  `write_gams(gdx =, include_solve =)` emits the matching `$load` block and
  the `model` / `solve` statements, so a GAMS job can be assembled without a
  GAMS licence and solved elsewhere. Zero-valued parameter records are written
  as `EPS`: a GDX stores no zeros, and GAMS has no per-parameter default, so
  the default assigned after loading would otherwise overwrite a stored zero.
  This route is exercised against an older GDX library and gdxtools 0.7,
  which does not install on R >= 4.6, with the solve run on NEOS;
  contributions testing newer pipelines are welcome.

* `mm_check_python()` reports the interpreter multimod would use and whether
  it carries the needed packages. `solve_pyomo()` calls it before
  bootstrapping, so a usable environment is no longer reinstalled on every
  solve.

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
  files for lazy loading.
* Generated code carries data either embedded in the code (small models) or
  read from an Arrow repository (large models).
* Constructors are also available under the cross-language names used by the
  planned C++ and Python bindings: `Set()`, `Parameter()`, `Equation()`,
  `SetRef()` and so on. These are aliases; the `new_*()` and `ast_*()` names
  are unchanged. `is_declaration()` replaces `inherits(x, "multimod")`.
* AST nodes are validated against the schema when constructed, so a malformed
  node is refused where it is built rather than surfacing later as a renderer
  that emits nothing. Disable with `options(multimod.validate = FALSE)`.
* Language-neutral serialisation of the AST: `model_to_json()` /
  `model_from_json()`, with the node shapes declared once in `ast_schema()` and
  a JSON Schema emitted by `ast_json_schema()`. `validate_ast()` checks a node
  against the schema. Previously structure was only ever written as `.rds`.

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
* `export_model_source()` and `export_energyrt()` work with the consolidated
  `example_models`.
* `DESCRIPTION` no longer carries a `Collate` field: it had fallen out of date,
  and files absent from it are silently not sourced.

## Documentation

* New vignettes: `configuration`, `pyomo`, `model_workspace`.
* Updated vignettes: `jump`, `gmpl`, `latex`.

---

# multimod 0.0.1 (2025-06-01)

* Initial GitHub release (June 1, 2025).
