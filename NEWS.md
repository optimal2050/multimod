# multimod (development version)

* The matrix route joins index members as integer codes of one dictionary
  per build, and prepares each parameter and mapping lookup once instead of
  per chunk. The assembled LP is identical; a 4.4M-row model builds about
  30 % faster.
* `model_to_lp()` fills the `rhs` column of the row index.
* An experimental native executor for the matrix route, opt-in with
  `options(multimod.engine = "native")`: equations are lowered to block plans
  run by compiled code; an equation outside the covered subset stays on the
  R engine. `options(multimod.threads = )` sets its threads (default: the
  machine's cores). The package now compiles C++17 code (`LinkingTo: cpp11`).
* `write_mps(stream = TRUE)` builds the matrix block by block and writes the
  MPS file itself, spooling the entries to bucket files by column range, so
  the matrix is never held in memory. The file is equivalent to the one
  HiGHS writes. With `presolve = TRUE` the doubleton rows are collected
  while the entries are spooled, and the substitution is applied to the
  spool before the file is written; the substitution is the one
  `lp_presolve()` makes, and the bundle decodes with `read_mps_solution()`
  unchanged.
* A mapping can be defined by a rule instead of a table: `mapping_rule()`
  names the tables whose natural join, gated and projected, gives the
  tuples, and `new_mapping(rule = )` carries it. `get_data()`,
  `load_model_data()` and the writers enumerate the tuples on demand
  (`materialise_rule()`); the matrix route enumerates them once per build
  in integer codes; `save_model()` writes the rule as `rule.json` beside
  the mapping and `trim_model()` never trims one.
* `lp_bounds()` propagates the row and column bounds of an LP to a fixed
  point and returns the implied interval of every variable (`lo_impl`,
  `up_impl` on the column index), with an optional objective cut from a
  known feasible value.
* The native executor reads parameters with wildcard (`NA`) index values,
  as a folded table from energyRt carries them, so an equation using one
  no longer falls back to the R engine.
* The native executor runs a term's expansion in slices when it would
  exceed `options(multimod.slice_rows = 16e6)` rows, so an equation that
  sums over a large set no longer holds the whole expansion at once.
* The streamed route holds less: the substitution map stays in compiled
  code and the writer takes the model's rows and columns as they are, the
  scan keeps its entries in fixed blocks, the hash indexes are a third
  smaller, and the build collects its intermediates as it goes.
  `options(multimod.cache_gb = )` caps the executor's index cache, dropping
  the least recently used indexes (rebuilt when needed again) for a lower
  peak at some cost in time.
* `specs/goldens/matrix/` holds the LP of the two example models as plain
  tables; `tests/testthat/test-matrix-goldens.R` checks the assembly against
  them coefficient for coefficient.
* `build_col_index()` and `build_row_index()` gain `codes =`: the index
  members are stored as integer codes of one dictionary instead of labels.
  The streaming writer builds its tables this way, so no member is held as
  a string during the build; the parquet and feather index tables it writes
  store the members dictionary-encoded, and `read_mps_solution()` reads
  both encodings.
* The substitution of the streamed presolve is composed in compiled code,
  from the equality rows only.
* `save_model()` writes parquet by default, at zstd level 3 (was CSV, and
  zstd level 15 for the Arrow formats): on model data parquet is an order
  of magnitude smaller than Arrow IPC at the same speed, and levels above 3
  gain nothing. The index tables of `write_mps()` use the same level;
  `read_mps_solution()` reads a bundle of another format when that is what
  the directory holds.
* `load_arrow_data()` reads a directory holding the parts of a table
  directly, as a version of a dataset store does.
* `get_data()` keeps the tables it reads from disk in a session memo keyed
  by the table's content hash or path, within
  `options(multimod.data_cache_gb = 8)`; a mapping several symbols share
  is read once per session. `clear_data_cache()` and `data_cache_info()`
  manage it. `load_model_data()` brings a model's tables into memory
  through it; the GAMS, JuMP and Pyomo writers, `fold_model()` and
  `mark_empty_data()` call it, so a model whose tables are references on
  disk renders the same as one holding them.
* The index tables of a streamed bundle store the members as integer codes
  of one dictionary, written beside them as `<base>_codes.csv` (CSV bundles
  keep the labels); they read back in a fraction of the time. The decode
  holds no copy of the index tables, takes each variable's rows from the
  runs of the symbol column instead of a scan per variable, and parses the
  solution's values in C. `read_cuopt_solution()` reads a gzipped file
  through a block-wise decompression instead of through lines in R.

# multimod 0.5.0

First CRAN release. The changes are listed against 0.0.1, the GitHub release
of June 2025, which read GAMS and rendered models to LaTeX and back to GAMS.

## Breaking changes

* multimod requires R >= 4.1.0.
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
* `write_gams_data()` (experimental) writes a model's data to a GDX file, and
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
  and load the solution into the model. `solve_pyomo()` uses the configured
  Python interpreter; `ensure_env = TRUE` creates the environment with
  `mm_setup_python()` when it is not usable.

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

* `vignette("multimod")` takes a small model through reading, translation,
  redundancy analysis, LaTeX, export and solving. Website articles cover
  writing a model in R, folding and trimming, solving and reading results,
  two published models, the model workspace, configuration and LaTeX
  rendering.
* The website moved to <https://optimal2050.github.io/multimod/r/>; the
  root is a landing page for the languages the repository is laid out for.
  Links to the old addresses are forwarded.

## Bug fixes

* `read_gams()` parses a sum over several indices, numbers in scientific
  notation with a signed exponent, and decimal literals in equation bodies.
* `read_gams()` reads several symbols declared on one line
  (`set tech, region;`), and an equation header without a `$` condition.
* `^` no longer distributes over the terms of its base, so `(1 + r)^n` is
  evaluated as written.
* `as_gams()` output compiles: conditions are bracketed, function arguments
  and multi-index sums are rendered, and iterated minima and maxima are
  written as `smin()` and `smax()`.
* `write_gams()` declares each equation over its sets, as GAMS requires,
  and writes no empty `sets` block.
* `as_latex()` renders an equation's index names in its header.
* `ast_dims()` accepts a single node, and returns a `dims` node unchanged.

---

# multimod 0.0.1 (2025-06-01)

* Initial GitHub release (June 1, 2025).
