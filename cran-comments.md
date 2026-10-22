## Resubmission

This resubmission fixes the test failure in the incoming pre-tests on
Debian: the 'arrow' build there has no 'zstd' codec, which `save_model()`
requested. Arrow files are now written with the first codec the installed
'arrow' build provides ('zstd', then 'lz4' or 'snappy', then none).

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
see `inst/COPYRIGHTS` and the `Copyright` field.

## Test environments

* Windows 11, R 4.6.1 and R 4.5.3 (local)
* win-builder: R-devel, R-release
* R-hub: linux, ubuntu-release, macos-arm64
