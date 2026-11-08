# Set up the Python environment for multimod

Installs software: a Conda environment (named "multimod" by default)
with the Python packages the Pyomo route needs, using the
[reticulate](https://rstudio.github.io/reticulate/) toolkit. It runs
only when called; no other multimod function calls it. It creates the
environment when missing, installs the requested packages, and activates
the environment for the current R session via
[`reticulate::use_condaenv()`](https://rstudio.github.io/reticulate/reference/use_python.html).
A Conda installation is required; with `ensure_miniconda = TRUE`
Miniconda is installed first when none is found.

## Usage

``` r
mm_setup_python(
  env_name = "multimod",
  packages = c("pyomo", "highspy", "pyarrow", "pandas"),
  conda_packages = "glpk",
  python_version = "3.14",
  ensure_miniconda = FALSE,
  reinstall = FALSE,
  pip = TRUE,
  ...
)
```

## Arguments

- env_name:

  Name of the Conda environment to manage. Defaults to "multimod" so the
  generated Pyomo artifacts have a predictable runtime.

- packages:

  Character vector of Python packages to install once the environment
  exists. The default covers every route multimod writes: `pyomo` and
  `highspy` to build and solve, `pyarrow` and `pandas` to read the Arrow
  data `save_model(format = "ipc")` writes. Set to `NULL` or
  `character(0)` to skip installation.

- conda_packages:

  Character vector installed with conda rather than `pip`. `glpk` by
  default: the MathProg route needs the `glpsol` executable, which pip
  does not ship.

- python_version:

  Python version spec passed to
  [`reticulate::conda_create()`](https://rstudio.github.io/reticulate/reference/conda-tools.html).
  Use `NULL` for reticulate's default.

- ensure_miniconda:

  Logical; when `TRUE`,
  [`reticulate::install_miniconda()`](https://rstudio.github.io/reticulate/reference/install_miniconda.html)
  runs when no Conda binary is found. Default `FALSE`: without Conda the
  function stops.

- reinstall:

  Logical flag; when `TRUE` the environment is recreated even if it
  already exists. Defaults to `FALSE` to preserve prior installs.

- pip:

  Logical; when `TRUE` (default) packages are installed via `pip` inside
  the Conda environment (ensures `highspy` is available). Set to `FALSE`
  to install via conda instead.

- ...:

  Additional arguments forwarded to
  [`reticulate::install_miniconda()`](https://rstudio.github.io/reticulate/reference/install_miniconda.html)
  when `ensure_miniconda = TRUE`.

## Value

Invisibly returns the environment name once configured.

## Details

After setup, multimod uses this environment either when it is set with
[`set_multimod_python()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md)
or when
[`get_multimod_python()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md)
detects it through reticulate.

## See also

[`get_multimod_python()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md),
[`set_multimod_python()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md)
