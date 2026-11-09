# multimod 0.6.0

First CRAN release. The changes are listed against 0.0.1, the GitHub release
of June 2025, which read GAMS and rendered models to LaTeX and back to GAMS.

## Breaking changes

* multimod requires R >= 4.1.0, and compiles C++17 code (`LinkingTo: cpp11`).
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
* A mapping can be defined by a rule instead of a table: `mapping_rule()`
  names the tables whose natural join, gated and projected, gives the
  tuples, and `new_mapping(rule = )` carries it. `materialise_rule()`
  enumerates the tuples; `get_data()`, the writers and the matrix route do
  so on demand.
* `example_models$toy` is a small capacity-expansion model built with the
  constructors.

### Rendering

* `as_gmpl()`, `as_pyomo()` and `write_gmpl()`, `write_jump()`,
  `write_pyomo()` render a model to MathProg, JuMP and Pyomo, with its data
  embedded or read from CSV, Arrow IPC or Parquet files.
* `write_gams_data()` (experimental) writes a model's data to a GDX file, and
  `write_gams(gdx =, include_solve =)` emits the matching load and solve
  statements. Parameters defined by a formula are written as assignments.

### The matrix route

* `model_to_lp()` assembles the LP matrix directly from the model, including
  symbols declared over sets, equation conditions, lags and leads, and
  formula parameters. Index members are joined as integer codes of one
  dictionary per build.
* `solve_highs()` solves the assembled LP in R and returns primal and dual
  values; `mm_session()` keeps the matrix between repeated solves.
* `write_mps()` writes the LP as MPS, with an optional presolve
  (`lp_presolve()`) that eliminates doubleton rows and proportional columns.
  With `stream = TRUE` it builds the matrix block by block and writes the
  file itself, spooling the entries to bucket files by column range, so the
  matrix is never held in memory; the presolve is then applied to the spool.
* `write_mps(substitute = )` substitutes definition equations while the
  matrix is built: the defined variable has no columns and its definition
  replaces every reference to it; `read_mps_solution()` rebuilds it.
  `objective_chain()` lists the definitions the objective depends on.
* For performance on large models, the matrix route includes C++17
  implementations of the supported equation executor, streamed coefficient
  spool and MPS writer, and the scan, presolve-map and remap steps used by
  streamed presolve. Native equation execution is opt-in with
  `options(multimod.engine = "native")`; unsupported equations fall back to
  the R engine individually. The executor runs on two threads by default
  (`options(multimod.threads = )`), slices large expansions with
  `options(multimod.slice_rows = )`, and can cap its index cache with
  `options(multimod.cache_gb = )`.
* `lp_bounds()` propagates the row and column bounds of an LP to a fixed
  point and returns the implied interval of every variable.
* `check_matrix_numbers()` reports the coefficient range of an assembled LP.
* `solve_mps()` solves an MPS file with HiGHS in this process, HiGHS through
  Julia, or cuOpt on this machine or inside WSL. cuOpt runs under a watchdog
  that stops a diverging or stalled solve (`cuopt_watchdog()`), and can use
  its Python interface, which writes the solution at any status and saves
  and continues from PDLP's warm-start data.
* `read_mps_solution()`, `read_highs_solution()`, `read_cuopt_solution()`
  and `read_solver_solution()` read solutions back against the index tables,
  by position or, for a matrix written with names, by label.

### Solving through other languages

* `solve_gmpl()`, `solve_jump()` and `solve_pyomo()` run the generated code
  and load the solution into the model. None of them installs software: the
  generated JuMP code stops and names missing Julia packages, and
  `solve_pyomo()` stops when the interpreter cannot import Pyomo.
  `mm_setup_python()` creates a Python environment for Pyomo when called.

### Workspace, reduction and exchange

* `save_model()`, `load_model()` and `get_data()` store a model and its data
  as Parquet (default), Arrow IPC or CSV files that the generated code reads
  directly. The structure is written as `model.rds`, `model.json` or both
  (`structure =`); `load_model(canonical = TRUE)` orders set members and
  mapping rows so the matrix does not depend on who wrote the directory.
* `get_data()` keeps the tables it reads from disk in a session memo within
  `options(multimod.data_cache_gb = 8)`; `clear_data_cache()` and
  `data_cache_info()` manage it, and `load_model_data()` brings a model's
  tables into memory through it.
* Writing functions write only where they are told: `write_pyomo()` needs
  `model_dir`, `write_gmpl()` writes a directory only when `model_dir` is
  given, and `import_gmpl_data()` needs `model_path` to save CSV or Arrow
  data.
* `fold_model()` drops dimensions a parameter does not vary over;
  `trim_model()` drops unused symbols. `unfold_model()` and `untrim_model()`
  reverse them.
* `model_to_json()` and `model_from_json()` serialise a model;
  `ast_schema()` and `ast_json_schema()` describe the node shapes, and
  `validate_ast()` checks a node against them.
* CamelCase constructors (`Set()`, `Parameter()`, `Equation()`, ...) are
  provided as aliases of `new_*()` and `ast_*()`.
* `compare_jump_stats()` compares the constraint statistics of two JuMP
  runs, and writes the comparison when given a `file`.

### Configuration

* Interpreter and solver paths resolve from R options, environment
  variables, a configuration file and detection, in that order; see
  `multimod_config_show()`. `multimod_config_write()` writes a configuration
  file to the path it is given.
* `mm_check_python()` reports whether the configured interpreter is usable.
* The comparison helpers, `multimod_config_show()` and
  `populate_sets_from_data()` print only when `verbose` is `TRUE`; its
  default is the option `multimod.verbose` (`TRUE`).
* The `glpsol` executable is taken from the 'glpsol' package when it is
  installed.

### Documentation

* `vignette("multimod")` takes a small model through reading, translation,
  redundancy analysis, LaTeX, export and solving. Website articles cover
  writing a model in R, model redundancy, solving and reading results,
  two published models, the model workspace, configuration and LaTeX
  rendering.
* The website moved to <https://optimal2050.github.io/multimod/r/>; the
  root is a landing page for the languages the repository is laid out for.
  Links to the old addresses are forwarded.

## Bug fixes

* Arrow IPC files are written with `write_ipc_file()` and read with
  `read_ipc_file()`, so arrow >= 26 no longer warns that `write_feather()`
  and `read_feather()` are superseded; arrow >= 9.0.0 is required. The
  format is called `"ipc"` throughout, and `"feather"` is no longer a
  format value.
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
