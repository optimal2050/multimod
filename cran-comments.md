## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## External software

The package renders models to 'GAMS', 'GNU MathProg', 'Pyomo' and 'JuMP'
and can hand them to external solvers (Julia, Python, 'glpsol'). None of
these is required: every function that calls external software checks for
it and fails with an installation hint, all such examples are marked
`\dontrun`, and the corresponding tests are skipped on CRAN. The checks and
examples that run on CRAN use the bundled `example_models` dataset and the
'highs' package only.

`glpsol` (Suggests) is a companion package submitted to CRAN on
2026-10-06; until it is published the check notes it as not in a
mainstream repository. `gdxtools` (Enhances) is GitHub-only and used in one
guarded code path (`write_gams_data()`, GDX export).

## Test environments

* Windows 11, R 4.5.3 and R 4.6.1 (local)

<!-- Add before submission: win-builder R-devel, R-hub linux + macos -->
