## Resubmission

This is a resubmission of multimod, now as version 0.6.0. It answers the
review of 0.5.0, and adds a compiled engine for the LP matrix route (see
"Compiled code" below).

### Response to the previous review

> Please ensure that your functions do not write by default or in your
> examples/vignettes/tests in the user's home filespace (including the
> package directory and getwd()).

* `multimod_config_write()` takes a required `file`; it wrote to
  `getwd()` by default. Its example writes to `tempfile()`.
* `compare_jump_stats()` writes only to a `file` it is given; it wrote a
  CSV beside its input by default. Its example runs on files under
  `tempdir()`.
* `write_gmpl()` writes a directory only when `model_dir` is given;
  `write_pyomo()` requires `model_dir`; `import_gmpl_data()` requires
  `model_path` to save data. None falls back to a stored or default path.
* `solve_mps()` writes the scripts and parameter files a solver needs to
  temporary files and removes them; only the solution file is written, at
  the path given or beside the input file.
* Examples, tests and the vignette write only under `tempdir()`.

> Please do not install packages in your functions, examples or vignette.

* The generated JuMP code no longer installs Julia packages: it stops and
  names the missing ones.
* `solve_pyomo()` no longer sets up a Python environment; it stops when the
  interpreter cannot import Pyomo.
* `solve_jump(method = "juliacall")` starts Julia without installing
  JuliaCall's dependencies.
* The Python interpreter is looked up without starting Python, so no
  interpreter is provisioned as a side effect.
* `mm_setup_python()` is the only function that installs software. It is
  documented as such, runs only when called, is never called by another
  function, and installs Miniconda only with `ensure_miniconda = TRUE`
  (default `FALSE`).

## Compiled code

The package now compiles C++17 code (`LinkingTo: cpp11`, no external
libraries): a native executor for the matrix route and a streaming MPS
writer. It runs on two threads by default (`options(multimod.threads = )`),
and is used in the tests only through small models.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## External software

The package renders models to 'GAMS', 'GNU MathProg', 'Pyomo' and 'JuMP'
and can hand them to external solvers (Julia, Python, 'glpsol'). None of
these is required: every function that calls external software checks for
it and fails with an installation hint, such examples are marked
`\dontrun`, and the corresponding tests are skipped on CRAN. The examples,
tests and vignette that run on CRAN use the bundled `example_models` dataset
and, when it is installed, the 'highs' package.

`glpsol` (Suggests) ships the GLPK solver as an R package. `gdxtools`
(Enhances) is used only by the experimental `write_gams_data()`, behind a
`requireNamespace()` check.

## Bundled content

`data/example_models.rda` contains a model derived from the energyRt model
templates (Apache License 2.0, copyright the energyRt development team);
see `inst/COPYRIGHTS` and the `Copyright` field. The hash finalizer in
`src/core/include/multimod/keyhash.h` is MurmurHash3's (public domain).

## Test environments

* Windows 11, R 4.6.2 (local): 1 NOTE (new submission)
* win-builder: R-devel (2026-11-07 r90850), R-release (4.6.2): 1 NOTE (new
  submission)
* R-hub: linux (R-devel), macos-arm64 (R-devel): OK
* R-hub: gcc-asan, clang-asan (R-devel with AddressSanitizer and
  UndefinedBehaviorSanitizer): OK, no sanitizer reports
