# Set up the Python environment for multimod

This is the installer: one Conda environment, shared by the whole stack.
energyRt resolves its Python through
[`get_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
so there is one environment and one source of truth rather than a
per-package env.

## Usage

``` r
mm_setup_python(
  env_name = "multimod",
  packages = c("pyomo", "highspy", "pyarrow", "pandas"),
  conda_packages = "glpk",
  python_version = "3.14",
  ensure_miniconda = TRUE,
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

  Logical flag controlling whether
  [`reticulate::install_miniconda()`](https://rstudio.github.io/reticulate/reference/install_miniconda.html)
  should run when no Conda binary is detected on the system. Defaults to
  `TRUE` to minimize user setup.

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

It bootstraps a Conda environment (named "multimod" by default) using
the [reticulate](https://rstudio.github.io/reticulate/) toolkit. It
ensures
[`reticulate::install_miniconda()`](https://rstudio.github.io/reticulate/reference/install_miniconda.html)
has been invoked (unless disabled), creates the target environment when
missing, installs the requested Python packages, and activates the
environment for the current R session via
[`reticulate::use_condaenv()`](https://rstudio.github.io/reticulate/reference/use_python.html).

After setup, multimod uses this environment either when it is set with
[`set_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
or when
[`get_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
detects it through reticulate.

## See also

[`get_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
[`set_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
