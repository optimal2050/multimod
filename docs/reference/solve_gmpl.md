# Solve GMPL model using glpsol command line tool

Solves an optimization model by running GLPK's `glpsol` executable.

## Usage

``` r
solve_gmpl(
  model = NULL,
  model_dir,
  method = "glpsol",
  verbose = TRUE,
  load_results = !is.null(model),
  glpsol_path = get_multimod_glpsol(),
  timeout = NULL
)
```

## Arguments

- model:

  Optional model object (for loading results back)

- model_dir:

  Directory containing model files (model.mod, data.dat, etc.)

- method:

  Retained for compatibility; only `"glpsol"` is supported. `"glpkAPI"`
  was retired when glpkAPI left CRAN - see `drafts/glpkAPI-retired.R`.

- verbose:

  Logical; print progress messages (default: TRUE)

- load_results:

  Logical; load solution CSV results back into model object (default:
  !is.null(model))

- glpsol_path:

  Path to the glpsol executable; defaults to
  [`get_multimod_glpsol()`](https://optimal2050.github.io/multimod/reference/multimod-config.md).

- timeout:

  Numeric; timeout in seconds (default: NULL, no timeout)

## Value

List with: model (if load_results=TRUE), success, objective, solve_time,
status

## Details

`glpsol` must be on the system:
[`get_multimod_glpsol()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
resolves it from `options("multimod.glpsol_path")`, `MULTIMOD_GLPSOL`,
the YAML config, the conda environment
[`mm_setup_python()`](https://optimal2050.github.io/multimod/reference/mm_setup_python.md)
installs GLPK into, and finally the bare name `"glpsol"` on `PATH`.
