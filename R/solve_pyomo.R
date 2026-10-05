#' Solve a Pyomo model exported by `multimod`
#'
#' Executes the generated `solve.py` script inside `solvers/pyomo/` after
#' ensuring a Python environment exists. By default the helper
#' [mm_setup_python()] is invoked so that a Conda environment named
#' "multimod" (configurable) is created, populated with Pyomo, and activated via
#' the `reticulate` toolkit. Once the environment is ready the solver script is
#' executed with `system2()` or `processx` and basic status information is
#' returned. Result loading is not implemented yet but the metadata emitted by
#' `solve.py` (e.g. `solution/status.json`) is surfaced when available.
#'
#' @param model Optional multimod model; currently only used when
#'   `load_results = TRUE` to indicate where solutions should be loaded (not yet
#'   implemented).
#' @param model_dir Path to the saved model directory created by `save_model()`.
#'   Must contain `solvers/<solver_dir>/solve.py` (generated via `write_pyomo()`).
#' @param solver_dir Name of the solver sub-directory inside `model_dir/solvers`.
#'   Defaults to "pyomo" to match `write_pyomo()`.
#' @param env_name Name of the Conda environment handled by
#'   `mm_setup_python()`. Defaults to "multimod".
#' @param ensure_env Logical; when `TRUE` (default) the environment is checked
#'   with [mm_check_python()] and `mm_setup_python()` runs only if it is not
#'   usable. Set to `FALSE` if you want to manage activation yourself.
#' @param packages Character vector of Python packages to request when the
#'   helper runs. `NULL` (default) uses [mm_setup_python()]'s own list, so the
#'   environment contents have a single owner.
#' @param python_version Optional Python version passed through to
#'   `mm_setup_python()`.
#' @param ensure_miniconda Forwarded to `mm_setup_python()`.
#' @param reinstall_env When `TRUE`, asks the helper to recreate the Conda
#'   environment before solving.
#' @param python Optional path to a Python executable. When omitted the Python
#'   binary reported by `reticulate::py_config()` for the activated environment is
#'   used.
#' @param solver Optional solver name that is exported to the Python process via
#'   the `PYOMO_SOLVER` environment variable.
#' @param env Named character vector of additional environment variables to pass
#'   to the Python process.
#' @param load_results Logical; reserved for future work. When `TRUE` a warning
#'   is issued because loading solutions back into R is not yet implemented.
#' @param verbose Controls solver logging capture. Logical (`TRUE` = buffer
#'   output, `FALSE` = quiet) or one of `"buffer"`, `"stream"`, `"quiet"`.
#' @param timeout Optional timeout (in seconds). Requires the `processx`
#'   package.
#' @param ... Additional arguments forwarded to [mm_setup_python()].
#'
#' @return A list describing the solver run (exit code, success flag, elapsed
#'   time, raw output, parsed status metadata when available, and environment
#'   details).
#' @export
solve_pyomo <- function(model = NULL,
                        model_dir,
                        solver_dir = "pyomo",
                        env_name = "multimod",
                        ensure_env = TRUE,
                        packages = NULL,
                        python_version = NULL,
                        ensure_miniconda = TRUE,
                        reinstall_env = FALSE,
                        python = NULL,
                        solver = NULL,
                        env = NULL,
                        load_results = FALSE,
                        verbose = c("buffer", "stream", "quiet"),
                        timeout = NULL,
                        ...) {
  stopifnot(!missing(model_dir))
  verbose_mode <- if (is.logical(verbose)) {
    if (verbose) "buffer" else "quiet"
  } else {
    match.arg(verbose, c("buffer", "stream", "quiet"))
  }

  model_dir <- normalizePath(model_dir, mustWork = TRUE)
  solver_path <- file.path(model_dir, "solvers", solver_dir)
  solve_script <- file.path(solver_path, "solve.py")
  if (!file.exists(solve_script)) {
    solve_script <- file.path(solver_path, "model.py")
  }

  if (!file.exists(solve_script)) {
    stop("Pyomo solve entrypoint not found. Run write_pyomo() before solve_pyomo().")
  }

  if (ensure_env) {
    # Bootstrapping on every solve reinstalls an environment that already
    # works, which is slow and shells out to conda for nothing.
    usable <- isTRUE(mm_check_python(env_name = env_name, python = python)$ok)
    setup_args <- list(
      env_name = env_name,
      python_version = python_version,
      ensure_miniconda = ensure_miniconda,
      reinstall = reinstall_env,
      ...
    )
    # Omitted rather than passed as NULL: NULL means "install nothing".
    if (!is.null(packages)) setup_args$packages <- packages
    if (isTRUE(reinstall_env) || !usable) {
      do.call(mm_setup_python, setup_args)
    } else if (!is.null(env_name) && requireNamespace("reticulate", quietly = TRUE)) {
      reticulate::use_condaenv(env_name, required = FALSE)
    }
  } else if (!is.null(env_name) && requireNamespace("reticulate", quietly = TRUE)) {
    reticulate::use_condaenv(env_name, required = FALSE)
  }

  python_exec <- python
  if (is.null(python_exec)) {
    # Use configuration system to find Python
    python_exec <- get_multimod_python(env_name = env_name)
  }

  if (is.null(python_exec) || is.na(python_exec) || !nzchar(python_exec)) {
    stop("Unable to locate a Python executable. Provide the 'python' argument, configure via set_multimod_python(), or ensure reticulate is configured.")
  }

  extra_env <- character(0)
  if (!is.null(env)) {
    if (is.null(names(env)) || any(names(env) == "")) {
      stop("'env' must be a named character vector of environment variables.")
    }
    extra_env <- as.character(env)
    names(extra_env) <- names(env)
  }
  if (!is.null(solver)) {
    extra_env["PYOMO_SOLVER"] <- solver
  }

  result <- run_pyomo_process(
    python_exec = python_exec,
    script = solve_script,
    verbose = verbose_mode,
    timeout = timeout,
    env = extra_env
  )

  status_path <- file.path(model_dir, "solvers", solver_dir, "solution", "status.json")
  if (file.exists(status_path) && requireNamespace("jsonlite", quietly = TRUE)) {
    status_info <- tryCatch(jsonlite::read_json(status_path, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(status_info)) {
      result$status <- status_info
    }
  }

  result$model_dir <- model_dir
  result$solver_dir <- solver_dir
  result$python <- python_exec
  result$env_name <- env_name

  if (load_results && !is.null(model)) {
    warning("Loading Pyomo solutions back into R is not implemented yet; returning run metadata only.")
  }

  result
}

run_pyomo_process <- function(python_exec, script, verbose = c("buffer", "stream", "quiet"),
                              timeout = NULL, env = NULL) {
  verbose <- match.arg(verbose)
  args <- c(script)
  env_named <- env

  # `system2(env=)` is not supported on Windows: the name=value strings are
  # passed as ARGUMENTS, so python is handed "PYOMO_SOLVER=highs" as a file
  # to open and exits 2. Setting the variables in this process and restoring
  # them afterwards works on every platform.
  if (length(env_named)) {
    previous <- Sys.getenv(names(env_named), unset = NA_character_, names = TRUE)
    do.call(Sys.setenv, as.list(env_named))
    on.exit({
      keep <- previous[!is.na(previous)]
      if (length(keep)) do.call(Sys.setenv, as.list(keep))
      gone <- names(previous)[is.na(previous)]
      if (length(gone)) Sys.unsetenv(gone)
    }, add = TRUE)
  }

  use_processx <- !is.null(timeout) || identical(verbose, "stream")
  start_time <- Sys.time()
  output_lines <- character(0)
  exit_code <- 0

  if (use_processx) {
    if (!requireNamespace("processx", quietly = TRUE)) {
      stop("The 'processx' package is required for streaming output or timeouts.")
    }
    split_lines <- function(text) {
      if (is.null(text) || !nzchar(text)) return(character(0))
      strsplit(text, "\n", fixed = TRUE)[[1]]
    }
    px <- processx::run(
      command = python_exec,
      args = args,
      echo = identical(verbose, "stream"),
      echo_cmd = FALSE,
      spinner = FALSE,
      # processx requires a time interval; NULL fails its assertion, so the
      # streaming path (timeout unset) needs an explicit Inf.
      timeout = timeout %||% Inf,
      error_on_status = FALSE,
      # processx REPLACES the environment unless "current" is included.
      env = if (length(env_named)) c("current", env_named) else NULL
    )
    exit_code <- px$status %||% 0
    if (isTRUE(px$timeout)) {
      exit_code <- 124
    }
    output_lines <- c(split_lines(px$stdout), split_lines(px$stderr))
  } else {
    output <- system2(
      python_exec,
      args = args,
      stdout = TRUE,
      stderr = TRUE
    )
    exit_code <- attr(output, "status") %||% 0
    output_lines <- output %||% character(0)
    if (identical(verbose, "buffer") && length(output_lines)) {
      cat(paste(output_lines, collapse = "\n"), "\n")
    }
  }

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  list(
    exit_code = exit_code,
    success = exit_code == 0,
    elapsed_time = elapsed,
    output = if (!identical(verbose, "quiet")) output_lines else NULL
  )
}
