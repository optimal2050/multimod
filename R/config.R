#' Configuration management for multimod solver paths
#'
#' @description
#' These functions manage solver executable paths for multimod. Configuration
#' can be set via R options, environment variables, or a YAML config file.
#' 
#' **Priority order** (highest to lowest):
#' 1. R option: `getOption("multimod.<solver>_path")`
#' 2. Environment variable: `MULTIMOD_<SOLVER>`
#' 3. YAML config file: `~/.multimod/config.yml` or `.multimod.yml`
#' 4. Intelligent defaults (reticulate for Python, system PATH for others)
#'
#' @name multimod-config
#' @examples
#' \dontrun{
#' # Set paths via R options (session-persistent)
#' set_multimod_python("C:/Python310/python.exe")
#' set_multimod_julia("C:/Julia-1.10/bin/julia.exe")
#' set_multimod_glpsol("C:/glpk/bin/glpsol.exe")
#' 
#' # Get configured paths
#' get_multimod_python()
#' get_multimod_julia()
#' get_multimod_glpsol()
#' 
#' # Set in .Rprofile for persistence across sessions:
#' options(
#'   multimod.python_path = "C:/Users/me/.conda/envs/multimod/python.exe",
#'   multimod.julia_path = "C:/Julia-1.10/bin/julia.exe"
#' )
#' 
#' # Or use environment variables (system-wide):
#' Sys.setenv(MULTIMOD_PYTHON = "C:/Python310/python.exe")
#' 
#' # Or use YAML config file (~/.multimod/config.yml):
#' multimod_config_write(list(
#'   python = list(path = "C:/Python310/python.exe", env_name = "multimod"),
#'   julia = list(path = "C:/Julia-1.10/bin/julia.exe"),
#'   glpsol = list(path = "C:/glpk/bin/glpsol.exe")
#' ))
#' 
#' # View current configuration
#' multimod_config_show()
#' }
NULL

# Python configuration --------------------------------------------------------

#' @rdname multimod-config
#' @param path Character path to the solver executable. If `NULL`, clears the setting.
#' @export
set_multimod_python <- function(path = NULL) {
  if (!is.null(path) && nzchar(path) && !file.exists(path)) {
    warning("Python executable not found at: ", path, "\nSetting anyway.")
  }
  options(multimod.python_path = path)
  invisible(path)
}

#' @rdname multimod-config
#' @param env_name For Python: name of conda/virtual environment to use.
#'   Defaults to `"multimod"`.
#' @export
get_multimod_python <- function(env_name = "multimod") {
  # 1. Check R option
  opt_path <- getOption("multimod.python_path")
  if (!is.null(opt_path) && nzchar(opt_path)) return(normalizePath(opt_path, mustWork = FALSE))
  
  # 2. Check environment variable
  env_path <- Sys.getenv("MULTIMOD_PYTHON", unset = "")
  if (nzchar(env_path)) return(normalizePath(env_path, mustWork = FALSE))
  
  # 3. Check YAML config
  yaml_path <- .get_config_value("python", "path")
  if (!is.null(yaml_path)) return(normalizePath(yaml_path, mustWork = FALSE))
  
  # 4. Try reticulate for conda environment
  if (requireNamespace("reticulate", quietly = TRUE)) {
    if (!is.null(env_name) && nzchar(env_name)) {
      tryCatch({
        reticulate::use_condaenv(env_name, required = FALSE)
      }, error = function(e) NULL)
    }
    cfg <- tryCatch(reticulate::py_config(), error = function(e) NULL)
    if (!is.null(cfg$python) && file.exists(cfg$python)) {
      return(normalizePath(cfg$python))
    }
  }
  
  # 5. Fall back to system python
  "python"
}

# Julia configuration ---------------------------------------------------------

#' @rdname multimod-config
#' @export
set_multimod_julia <- function(path = NULL) {
  if (!is.null(path) && nzchar(path) && !file.exists(path)) {
    warning("Julia executable not found at: ", path, "\nSetting anyway.")
  }
  options(multimod.julia_path = path)
  invisible(path)
}

#' @rdname multimod-config
#' @export
get_multimod_julia <- function() {
  # 1. Check R option
  opt_path <- getOption("multimod.julia_path")
  if (!is.null(opt_path) && nzchar(opt_path)) return(normalizePath(opt_path, mustWork = FALSE))
  
  # 2. Check environment variable
  env_path <- Sys.getenv("MULTIMOD_JULIA", unset = "")
  if (nzchar(env_path)) return(normalizePath(env_path, mustWork = FALSE))
  
  # 3. Check YAML config
  yaml_path <- .get_config_value("julia", "path")
  if (!is.null(yaml_path)) return(normalizePath(yaml_path, mustWork = FALSE))
  
  # 4. Fall back to system julia
  "julia"
}

# GLPK/glpsol configuration ---------------------------------------------------

#' @rdname multimod-config
#' @export
set_multimod_glpsol <- function(path = NULL) {
  if (!is.null(path) && nzchar(path) && !file.exists(path)) {
    warning("glpsol executable not found at: ", path, "\nSetting anyway.")
  }
  options(multimod.glpsol_path = path)
  invisible(path)
}

#' @rdname multimod-config
#' @export
get_multimod_glpsol <- function() {
  # 1. Check R option
  opt_path <- getOption("multimod.glpsol_path")
  if (!is.null(opt_path) && nzchar(opt_path)) return(normalizePath(opt_path, mustWork = FALSE))
  
  # 2. Check environment variable
  env_path <- Sys.getenv("MULTIMOD_GLPSOL", unset = "")
  if (nzchar(env_path)) return(normalizePath(env_path, mustWork = FALSE))
  
  # 3. Check YAML config
  yaml_path <- .get_config_value("glpsol", "path")
  if (!is.null(yaml_path)) return(normalizePath(yaml_path, mustWork = FALSE))
  
  # 4. Fall back to system glpsol
  "glpsol"
}

# Generic solver configuration ------------------------------------------------

#' @rdname multimod-config
#' @param solver_name Name of the solver (e.g., "python", "julia", "glpsol", "highs")
#' @export
set_multimod_solver <- function(solver_name, path = NULL) {
  solver_name <- tolower(solver_name)
  if (!is.null(path) && nzchar(path) && !file.exists(path)) {
    warning(solver_name, " executable not found at: ", path, "\nSetting anyway.")
  }
  opt_name <- paste0("multimod.", solver_name, "_path")
  do.call(options, setNames(list(path), opt_name))
  invisible(path)
}

#' @rdname multimod-config
#' @export
get_multimod_solver <- function(solver_name) {
  solver_name <- tolower(solver_name)
  
  # 1. Check R option
  opt_name <- paste0("multimod.", solver_name, "_path")
  opt_path <- getOption(opt_name)
  if (!is.null(opt_path) && nzchar(opt_path)) return(normalizePath(opt_path, mustWork = FALSE))
  
  # 2. Check environment variable
  env_name <- paste0("MULTIMOD_", toupper(solver_name))
  env_path <- Sys.getenv(env_name, unset = "")
  if (nzchar(env_path)) return(normalizePath(env_path, mustWork = FALSE))
  
  # 3. Check YAML config
  yaml_path <- .get_config_value(solver_name, "path")
  if (!is.null(yaml_path)) return(normalizePath(yaml_path, mustWork = FALSE))
  
  # 4. Fall back to system PATH
  solver_name
}

# YAML configuration file support ---------------------------------------------

#' @rdname multimod-config
#' @param config Named list of configuration values to write to YAML file.
#' @param global Logical; if `TRUE`, writes to `~/.multimod/config.yml` (user-wide).
#'   If `FALSE` (default), writes to `.multimod.yml` in current working directory.
#' @export
multimod_config_write <- function(config, global = FALSE) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The 'yaml' package is required for config file support.\n",
         "Install it with: install.packages('yaml')")
  }
  
  config_file <- if (global) {
    file.path(Sys.getenv("HOME"), ".multimod", "config.yml")
  } else {
    file.path(getwd(), ".multimod.yml")
  }
  
  # Create directory if needed
  config_dir <- dirname(config_file)
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
  }
  
  yaml::write_yaml(config, config_file)
  message("Configuration written to: ", config_file)
  invisible(config_file)
}

#' @rdname multimod-config
#' @export
multimod_config_read <- function(global = FALSE) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    return(NULL)
  }
  
  # Try project-level first, then global
  config_files <- c(
    file.path(getwd(), ".multimod.yml"),
    file.path(Sys.getenv("HOME"), ".multimod", "config.yml")
  )
  
  if (global) {
    config_files <- rev(config_files)
  }
  
  for (config_file in config_files) {
    if (file.exists(config_file)) {
      return(yaml::read_yaml(config_file))
    }
  }
  
  NULL
}

#' @rdname multimod-config
#' @export
multimod_config_path <- function(global = FALSE) {
  config_file <- if (global) {
    file.path(Sys.getenv("HOME"), ".multimod", "config.yml")
  } else {
    file.path(getwd(), ".multimod.yml")
  }
  
  if (file.exists(config_file)) {
    return(normalizePath(config_file))
  }
  
  paste0(config_file, " (not found)")
}

#' @rdname multimod-config
#' @export
multimod_config_show <- function() {
  cat("Current multimod configuration:\n\n")
  
  cat("Python:\n")
  cat("  Path:    ", get_multimod_python(), "\n")
  cat("  Option:  ", getOption("multimod.python_path") %||% "(not set)", "\n")
  cat("  Env var: ", Sys.getenv("MULTIMOD_PYTHON", "(not set)"), "\n\n")
  
  cat("Julia:\n")
  cat("  Path:    ", get_multimod_julia(), "\n")
  cat("  Option:  ", getOption("multimod.julia_path") %||% "(not set)", "\n")
  cat("  Env var: ", Sys.getenv("MULTIMOD_JULIA", "(not set)"), "\n\n")
  
  cat("glpsol:\n")
  cat("  Path:    ", get_multimod_glpsol(), "\n")
  cat("  Option:  ", getOption("multimod.glpsol_path") %||% "(not set)", "\n")
  cat("  Env var: ", Sys.getenv("MULTIMOD_GLPSOL", "(not set)"), "\n\n")
  
  yaml_config <- multimod_config_read()
  if (!is.null(yaml_config)) {
    cat("YAML config file: ", multimod_config_path(), "\n")
  } else {
    cat("YAML config file: (none found)\n")
  }
  
  invisible(NULL)
}

# Internal helper functions ---------------------------------------------------

.get_config_value <- function(solver, key) {
  config <- multimod_config_read()
  if (is.null(config)) return(NULL)
  
  solver_config <- config[[solver]]
  if (is.null(solver_config)) return(NULL)
  
  solver_config[[key]]
}
