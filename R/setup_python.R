#' Set up the Python environment for multimod
#'
#' This is the installer: one Conda environment, shared by the whole stack.
#' energyRt resolves its Python through [get_multimod_python()], so there is
#' one environment and one source of truth rather than a per-package env.
#'
#' It bootstraps a Conda environment (named "multimod" by default)
#' using the [reticulate](https://rstudio.github.io/reticulate/) toolkit. It
#' ensures `reticulate::install_miniconda()` has been invoked (unless disabled),
#' creates the target environment when missing, installs the requested Python
#' packages, and activates the environment for the current R session via
#' `reticulate::use_condaenv()`.
#'
#' After setup, you can optionally configure multimod to use this environment
#' via `set_multimod_python()` or by letting `get_multimod_python()` detect it
#' automatically through reticulate.
#'
#' @param env_name Name of the Conda environment to manage. Defaults to
#'   "multimod" so the generated Pyomo artifacts have a predictable runtime.
#' @param packages Character vector of Python packages to install once the
#'   environment exists. The default covers every route multimod writes:
#'   `pyomo` and `highspy` to build and solve, `pyarrow` and `pandas` to read
#'   the Arrow data `save_model(format = "ipc")` writes. Set to `NULL` or
#'   `character(0)` to skip installation.
#' @param python_version Python version spec passed to
#'   [reticulate::conda_create()]. Use `NULL` for reticulate's default.
#' @param ensure_miniconda Logical flag controlling whether
#'   [reticulate::install_miniconda()] should run when no Conda binary is
#'   detected on the system. Defaults to `TRUE` to minimize user setup.
#' @param reinstall Logical flag; when `TRUE` the environment is recreated even
#'   if it already exists. Defaults to `FALSE` to preserve prior installs.
#' @param pip Logical; when `TRUE` (default) packages are installed via
#'   `pip` inside the Conda environment (ensures `highspy` is available).
#'   Set to `FALSE` to install via conda instead.
#' @param ... Additional arguments forwarded to
#'   [reticulate::install_miniconda()] when `ensure_miniconda = TRUE`.
#'
#' @return Invisibly returns the environment name once configured.
#' @seealso [get_multimod_python()], [set_multimod_python()]
#' @export
mm_setup_python <- function(
    env_name = "multimod",
    packages = c("pyomo", "highspy", "pyarrow", "pandas"),
    python_version = "3.14",
    ensure_miniconda = TRUE,
    reinstall = FALSE,
  pip = TRUE,
    ...) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required; install it before continuing.")
  }

  ensure_conda <- function() {
    # Check whether a conda binary is available; if not, install miniconda when allowed
    conda_ok <- tryCatch({
      !is.na(reticulate::conda_binary())
    }, error = function(e) FALSE)

    if (!conda_ok) {
      if (!isTRUE(ensure_miniconda)) {
        stop("No Conda installation detected. Enable 'ensure_miniconda' or install Conda manually.")
      }
      reticulate::install_miniconda(...)
    }
  }

  ensure_conda()

  conda_envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character())
  env_exists <- env_name %in% conda_envs

  if (!env_exists || isTRUE(reinstall)) {
    if (env_exists && isTRUE(reinstall)) {
      reticulate::conda_remove(env_name)
    }
    
    # Create environment - catch JSON parsing errors that don't affect actual creation
    create_result <- tryCatch({
      reticulate::conda_create(env_name, python_version = python_version)
      TRUE
    }, error = function(e) {
      # Check if error is just JSON parsing issue
      if (grepl("lexical error|invalid character inside string", e$message, ignore.case = TRUE)) {
        # Verify environment was actually created despite the error
        envs <- tryCatch(reticulate::conda_list()$name, error = function(e2) character())
        if (env_name %in% envs) {
          message("Note: Environment '", env_name, "' created successfully (ignoring JSON parse error)")
          return(TRUE)
        }
      }
      stop(e)
    })
  }

  if (!is.null(packages) && length(packages) > 0) {
    # Install packages - also catch JSON parsing errors
    install_result <- tryCatch({
      reticulate::conda_install(env_name, packages = packages, pip = pip)
      TRUE
    }, error = function(e) {
      if (grepl("lexical error|invalid character inside string", e$message, ignore.case = TRUE)) {
        message("Note: Package installation completed (ignoring JSON parse error)")
        message("Verify packages with: reticulate::py_list_packages(envname='", env_name, "')")
        return(TRUE)
      }
      stop(e)
    })
  }

  reticulate::use_condaenv(env_name, required = TRUE)
  invisible(env_name)
}

