# Configure a Python environment for multimod

This helper bootstraps a Conda environment (named "multimod" by default)
using the [reticulate](https://rstudio.github.io/reticulate/) toolkit.
It ensures
[`reticulate::install_miniconda()`](https://rstudio.github.io/reticulate/reference/install_miniconda.html)
has been invoked (unless disabled), creates the target environment when
missing, installs the requested Python packages, and activates the
environment for the current R session via
[`reticulate::use_condaenv()`](https://rstudio.github.io/reticulate/reference/use_python.html).

## Usage

``` r
setup_python_environment(
  env_name = "multimod",
  packages = c("pyomo", "highspy"),
  python_version = NULL,
  ensure_miniconda = TRUE,
  reinstall = FALSE,
  pip = TRUE,
  ...
)

setup_multimod_python(
  env_name = "multimod",
  packages = c("pyomo", "highspy"),
  python_version = NULL,
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
  exists. Defaults to both `"pyomo"` and `"highspy"` so a usable solver
  backend is available immediately. Set to `NULL` or `character(0)` to
  skip installation.

- python_version:

  Optional Python version spec passed to
  [`reticulate::conda_create()`](https://rstudio.github.io/reticulate/reference/conda-tools.html).
  If `NULL`, reticulate's default is used.

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

After setup, you can optionally configure multimod to use this
environment via
[`set_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
or by letting
[`get_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
detect it automatically through reticulate.

## See also

[`get_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
[`set_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
