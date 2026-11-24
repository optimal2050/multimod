# =============================================================================
# Solving a written MPS file: one entry point, several solvers and places
# =============================================================================
#
# The matrix is a file; the solver reads it and writes a solution file that
# read_mps_solution() decodes against the index tables. Which solver and where
# it runs (in this process, a local executable, WSL) only
# changes how the solution file is produced, so every combination returns the
# same list and the decoding never depends on it.

#' Solve an MPS file and write a solution file
#'
#' Reads an MPS file written by [write_mps()], solves it and writes a solution
#' file that [read_mps_solution()] decodes against the index tables.
#'
#' Solvers and places:
#' * `"highs"`, `"local"`: HiGHS in this R process (the `highs` package).
#' * `"julia_highs"`, `"local"`: HiGHS through Julia (HiGHS.jl), whose build
#'   carries the HiPO interior point method. Julia is found by
#'   [get_multimod_julia()].
#' * `"cuopt"`, `"local"`: NVIDIA cuOpt's `cuopt_cli` on this machine (Linux).
#' * `"cuopt"`, `"wsl"`: `cuopt_cli` inside WSL on a local GPU (Windows); the
#'   solution is written inside WSL and copied out in one piece.
#'
#' `control` holds the solver's options. HiGHS (both places): named HiGHS
#' options, e.g. `list(solver = "ipm")`. cuOpt: `method` (default `1`, PDLP),
#' `time_limit` (seconds; `cuopt_cli`'s own default is short), `presolve`,
#' `options` (further `cuopt_cli` flags as one string or a character vector);
#' for `"wsl"` also `wsl_user` and `cuopt_cli` (path inside WSL), else the
#' environment variables `MULTIMOD_WSL_USER` and `MULTIMOD_CUOPT_CLI`, else
#' WSL's default user and `cuopt_cli` on its `PATH`.
#'
#' A cuOpt run (any place) can go through cuOpt's Python interface instead of
#' `cuopt_cli` (`control$api = "python"`). It writes the solution file whatever
#' the status: at a time limit the current iterate is kept (status
#' `TimeLimit`), where `cuopt_cli` writes no values. `control$save_warm_start
#' = TRUE` also returns PDLP's warm-start data as `warm_start.pkl` beside
#' `sol_file`; `control$warm_start` (a path to such a file) continues from it.
#' Both run PDLP in its `Stable2` mode, the one cuOpt warm-starts, and without
#' cuOpt's presolve (unless `control$presolve` is set): the warm-start data
#' has the size of the problem PDLP solved. Locally and
#' in WSL the Python is `control$cuopt_python`, else the environment variable
#' `MULTIMOD_CUOPT_PYTHON`, else `python3` beside `cuopt_cli`.
#'
#' Scripts and parameter files a solver needs go to temporary files and are
#' removed afterwards; the cuOpt log goes to `control$log_file`, else to a
#' temporary file. Only the solution (and, when asked, the warm-start data)
#' is written beside `sol_file`.
#'
#' @param file Path to the MPS file. HiGHS in this process reads it
#'   uncompressed; cuOpt reads `.gz` as well.
#' @param sol_file Path for the solution file; default `file` with a `.sol`
#'   extension, in the directory of `file`.
#' @param control Solver options; see Details.
#' @param require_optimal Logical; error unless the run ends optimal.
#' @param verbose Logical; report progress.
#' @param solver `"highs"`, `"julia_highs"` or `"cuopt"`.
#' @param where `"local"` or `"wsl"`.
#'
#' @return A list with `status_message`, `objective`, `sol_file` and
#'   `run_time` (seconds).
#' @examples
#' if (requireNamespace("highs", quietly = TRUE)) {
#'   dir <- tempfile("mps-")
#'   dir.create(dir)
#'   mps <- file.path(dir, "toy.mps")
#'   write_mps(example_models$toy, mps, format = "csv")
#'   s <- solve_mps(mps)
#'   s$objective
#'   sol <- read_mps_solution(s$sol_file, index_dir = dir, format = "csv")
#'   head(sol$primal)
#' }
#' @export
solve_mps <- function(file, sol_file = NULL, control = list(),
                      require_optimal = TRUE, verbose = FALSE,
                      solver = c("highs", "julia_highs", "cuopt"),
                      where = c("local", "wsl")) {
  solver <- match.arg(solver)
  where <- match.arg(where)
  if (!file.exists(file)) stop("MPS file not found: ", file)
  if (length(control) && (!is.list(control) || is.null(names(control)) ||
    any(!nzchar(names(control))))) {
    stop("`control` must be a named list of solver options.")
  }
  key <- paste(solver, where, sep = "/")
  if (is.null(sol_file)) {
    sol_file <- paste0(sub("\\.mps(\\.gz)?$", "", file, ignore.case = TRUE), ".sol")
  }
  switch(key,
    "highs/local" = .solve_mps_highs(file, sol_file, control, require_optimal, verbose),
    "julia_highs/local" = .solve_mps_julia(file, sol_file, control, require_optimal, verbose),
    "cuopt/local" = .solve_mps_cuopt_local(file, sol_file, control, require_optimal, verbose, wsl = FALSE),
    "cuopt/wsl" = .solve_mps_cuopt_local(file, sol_file, control, require_optimal, verbose, wsl = TRUE),
    stop(
      "solver = '", solver, "' does not run where = '", where, "'. Available: ",
      "highs/local, julia_highs/local, cuopt/local, cuopt/wsl."
    )
  )
}

# HiGHS in this process ------------------------------------------------------

.solve_mps_highs <- function(file, sol_file, control, require_optimal, verbose) {
  if (!requireNamespace("highs", quietly = TRUE)) {
    stop(
      "solve_mps() needs the 'highs' package. Install it with ",
      "install.packages(\"highs\")."
    )
  }
  if (grepl("\\.gz$", file, ignore.case = TRUE)) {
    stop("The HiGHS reader does not accept gzipped MPS; decompress first.")
  }

  s <- highs::hi_new_solver(highs::highs_model(L = 0, lower = 0, upper = 0))
  highs::hi_solver_read_model(s, file)
  if (length(control)) {
    for (nm in names(control)) highs::hi_solver_set_option(s, nm, control[[nm]])
  }
  # HiGHS writes its own solution file at the end of the run, with the names
  # the MPS carries: a matrix written with names by another writer is then
  # decoded by name
  if (file.exists(sol_file)) file.remove(sol_file)
  highs::hi_solver_set_option(s, "solution_file", normalizePath(sol_file, winslash = "/", mustWork = FALSE))
  highs::hi_solver_set_option(s, "write_solution_to_file", TRUE)
  highs::hi_solver_set_option(s, "write_solution_style", 0L)
  highs::hi_solver_run(s)

  msg <- highs::hi_solver_status_message(s)
  if (require_optimal && !grepl("optimal", msg, ignore.case = TRUE)) {
    stop(
      "HiGHS finished with status '", msg, "', not optimal.\n",
      "  Pass require_optimal = FALSE to write the solution anyway."
    )
  }

  sol <- highs::hi_solver_get_solution(s)
  objective <- sum(highs::hi_solver_get_lp_costs(s) * sol$col_value)
  n <- length(sol$col_value)
  m <- length(sol$row_value)
  if (file.exists(sol_file) && file.size(sol_file) > 0) {
    if (verbose) {
      message(
        "solved ", basename(file), ": ", msg, ", objective ",
        format(objective, digits = 10), "\n  -> ", sol_file
      )
    }
    return(list(
      status_message = msg, objective = objective, sol_file = sol_file,
      run_time = highs::hi_solver_get_run_time(s)
    ))
  }

  # A HiGHS that does not write the file itself: a raw-style solution file
  # (the format read_highs_solution() parses) with positional names, as
  # write_mps() emits them; dual sections only when the duals are valid.
  con <- file(sol_file, "wb") # binary: LF endings, byte-stable across OSes
  wr <- function(...) writeLines(c(...), con, sep = "\n")
  wr("Model status", msg, "")
  wr(
    "# Primal solution values", "Feasible",
    sprintf("Objective %.17g", objective)
  )
  wr(sprintf("# Columns %d", n))
  wr(sprintf("c%d %.17g", seq_len(n) - 1L, sol$col_value))
  wr(sprintf("# Rows %d", m))
  wr(sprintf("r%d %.17g", seq_len(m) - 1L, sol$row_value))
  if (isTRUE(sol$dual_valid)) {
    wr("", "# Dual solution values", "Feasible")
    wr(sprintf("# Columns %d", n))
    wr(sprintf("c%d %.17g", seq_len(n) - 1L, sol$col_dual))
    wr(sprintf("# Rows %d", m))
    wr(sprintf("r%d %.17g", seq_len(m) - 1L, sol$row_dual))
  }
  close(con)

  if (verbose) {
    message(
      "solved ", basename(file), ": ", msg, ", objective ",
      format(objective, digits = 10), "\n  -> ", sol_file
    )
  }
  list(
    status_message = msg, objective = objective, sol_file = sol_file,
    run_time = highs::hi_solver_get_run_time(s)
  )
}

# HiGHS through Julia --------------------------------------------------------

# HiGHS.jl's C API reads the file and writes the solution itself: no JuMP model
# is built. The script carries the options typed from R and is written to a
# temporary file.
.julia_highs_script <- function(control) {
  setter <- function(nm, v) {
    if (is.logical(v)) {
      sprintf('HiGHS.Highs_setBoolOptionValue(h, "%s", %s)', nm, if (isTRUE(v)) "1" else "0")
    } else if (is.integer(v)) {
      sprintf('HiGHS.Highs_setIntOptionValue(h, "%s", %d)', nm, v)
    } else if (is.numeric(v)) {
      sprintf('HiGHS.Highs_setDoubleOptionValue(h, "%s", %s)', nm, format(v, digits = 17))
    } else {
      sprintf('HiGHS.Highs_setStringOptionValue(h, "%s", "%s")', nm, as.character(v))
    }
  }
  c(
    "using HiGHS",
    "h = HiGHS.Highs_create()",
    "t0 = time()",
    "HiGHS.Highs_readModel(h, ARGS[1]) == 0 || error(\"HiGHS could not read \" * ARGS[1])",
    vapply(names(control), function(nm) setter(nm, control[[nm]]), ""),
    "HiGHS.Highs_run(h)",
    "t1 = time()",
    "HiGHS.Highs_writeSolution(h, ARGS[2])",
    "st = HiGHS.Highs_getModelStatus(h)",
    "res = string(\"MMJULIA status=\", st, \" objective=\", HiGHS.Highs_getObjectiveValue(h), \" seconds=\", round(t1 - t0, digits = 1))",
    # To a file when the caller gives one, so the result can be read without
    # capturing stdout -- capturing would buffer the solver's log until the
    # end. Only then is stdout the solver's alone.
    "if length(ARGS) >= 3; open(ARGS[3], \"w\") do io; println(io, res); end; else; println(res); end",
    "HiGHS.Highs_destroy(h)"
  )
}

# HiGHS' model status codes (HighsModelStatus), as Highs_getModelStatus returns them.
.highs_model_status <- function(code) {
  lab <- c(
    "0" = "Not Set", "1" = "Load error", "2" = "Model error",
    "3" = "Presolve error", "4" = "Solve error", "5" = "Postsolve error",
    "6" = "Empty", "7" = "Optimal", "8" = "Infeasible",
    "9" = "Primal infeasible or unbounded", "10" = "Unbounded",
    "11" = "Bound on objective reached", "12" = "Target for objective reached",
    "13" = "Time limit reached", "14" = "Iteration limit reached",
    "15" = "Unknown", "16" = "Solution limit reached", "17" = "Interrupted",
    "18" = "Memory limit reached"
  )
  out <- unname(lab[as.character(code)])
  ifelse(is.na(out), paste("status", code), out)
}

.solve_mps_julia <- function(file, sol_file, control, require_optimal, verbose) {
  jl <- tempfile("solve_mps_", fileext = ".jl")
  on.exit(unlink(jl), add = TRUE)
  writeLines(.julia_highs_script(control), jl)
  if (file.exists(sol_file)) file.remove(sol_file)
  res_file <- tempfile("mmjulia_", fileext = ".txt")
  on.exit(unlink(res_file), add = TRUE)
  args <- c(shQuote(jl), shQuote(file), shQuote(sol_file), shQuote(res_file))
  t0 <- Sys.time()
  if (isTRUE(verbose)) {
    # stdout is INHERITED so HiGHS' log streams as the solve runs; capturing it
    # buffers every line until the end, which is the opposite of watching a
    # long solve. The result is read from `res_file` instead.
    code <- suppressWarnings(system2(get_multimod_julia(), args))
    out <- character()
  } else {
    out <- suppressWarnings(system2(get_multimod_julia(), args,
                                    stdout = TRUE, stderr = TRUE))
    code <- attr(out, "status") %||% 0L
  }
  if (!file.exists(sol_file)) {
    stop(
      "HiGHS (Julia) wrote no solution (exit ", code, ").\n",
      if (length(out)) {
        paste(utils::tail(out, 15), collapse = "\n")
      } else {
        "  Its output was streamed above."
      }
    )
  }
  line <- if (file.exists(res_file)) {
    utils::tail(grep("^MMJULIA ", readLines(res_file, warn = FALSE),
                     value = TRUE), 1)
  } else {
    character()
  }
  if (!length(line)) line <- utils::tail(grep("^MMJULIA ", out, value = TRUE), 1)
  msg <- if (length(line)) .highs_model_status(sub(".*status=(\\S+) objective=.*", "\\1", line)) else NA_character_
  obj <- if (length(line)) suppressWarnings(as.numeric(sub(".*objective=(\\S+).*", "\\1", line))) else NA_real_
  if (require_optimal && !isTRUE(grepl("optimal", msg, ignore.case = TRUE))) {
    stop("HiGHS (Julia) finished with status '", msg, "', not optimal.")
  }
  list(
    status_message = msg, objective = obj, sol_file = sol_file,
    run_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

# cuOpt ----------------------------------------------------------------------

# `cuopt_cli`'s arguments after the input file: pure, so the command can be
# checked without a GPU. An unset time limit means cuopt_cli's own short one,
# so callers pass it.
.cuopt_options <- function(control) {
  opts <- c("--method", as.character(control$method %||% 1L))
  if (!is.null(control$time_limit)) opts <- c(opts, "--time-limit", as.character(control$time_limit))
  if (!is.null(control$presolve)) opts <- c(opts, "--presolve", as.character(as.integer(control$presolve)))
  # infeasibility detection: the solve's setting, else the session's, else cuOpt's default
  for (o in c("infeasibility_detection", "strict_infeasibility")) {
    v <- control[[o]] %||% getOption(paste0("multimod.cuopt_", o))
    if (!is.null(v)) opts <- c(opts, paste0("--", gsub("_", "-", o)), tolower(as.character(isTRUE(v))))
  }
  if (!is.null(control$options)) {
    opts <- c(opts, unlist(strsplit(paste(control$options, collapse = " "), "[[:space:]]+")))
  }
  opts[nzchar(opts)]
}

.cuopt_cli_args <- function(mps, sol, control) {
  c(mps, "--solution-file", sol, .cuopt_options(control))
}

# A Windows path as WSL sees it: C:/a/b -> /mnt/c/a/b.
.wsl_path <- function(x) {
  if (startsWith(x, "/")) {
    return(x)
  }
  x <- normalizePath(x, winslash = "/", mustWork = FALSE)
  drv <- regmatches(x, regexpr("^[A-Za-z]:", x))
  if (!length(drv)) {
    return(x)
  }
  paste0("/mnt/", tolower(substr(drv, 1, 1)), substring(x, 3))
}

.wsl_user_args <- function(control) {
  u <- control$wsl_user %||% Sys.getenv("MULTIMOD_WSL_USER", "")
  if (nzchar(u)) c("-u", u) else character(0)
}

.cuopt_cli_path <- function(control, wsl) {
  control$cuopt_cli %||%
    Sys.getenv(
      "MULTIMOD_CUOPT_CLI",
      if (wsl) "cuopt_cli" else get_multimod_solver("cuopt_cli")
    )
}

# The program and arguments of a run on this machine or in WSL: `cuopt_cli`,
# or Python with the script and parameter files (`script`, `params`). "-"
# stands for no warm-start file: wsl.exe drops an empty argument.
.cuopt_local_command <- function(file, sol, warm_out, control, wsl, script, params) {
  path <- if (wsl) .wsl_path else identity
  cli <- .cuopt_cli_path(control, wsl)
  if (!identical(control$api, "python")) {
    return(list(exe = cli, args = .cuopt_cli_args(path(file), sol, control)))
  }
  warm_in <- if (is.null(control$warm_start)) {
    "-"
  } else {
    path(normalizePath(control$warm_start, winslash = "/", mustWork = FALSE))
  }
  list(
    exe = .cuopt_python_path(control, cli),
    args = c("-u", path(script), path(file), sol, path(params), warm_in, warm_out)
  )
}

# The Python that has cuOpt's package: `control$cuopt_python`, else
# MULTIMOD_CUOPT_PYTHON, else `python3` beside `cuopt_cli` (both live in the
# environment's bin/), else `python3` on the PATH.
.cuopt_python_path <- function(control, cli) {
  p <- control$cuopt_python %||% Sys.getenv("MULTIMOD_CUOPT_PYTHON", "")
  if (nzchar(p)) {
    return(p)
  }
  if (grepl("/", cli, fixed = TRUE)) file.path(dirname(cli), "python3") else "python3"
}

# The status and objective from the solution file's header (`# Status: ...`,
# `# Objective value: ...`), without reading its body.
.cuopt_sol_header <- function(sol_file) {
  con <- if (grepl("\\.gz$", sol_file, ignore.case = TRUE)) gzfile(sol_file, "rt") else file(sol_file, "rt")
  on.exit(close(con))
  hdr <- readLines(con, n = 10, warn = FALSE)
  list(
    status = .cuopt_header(hdr, "Status"),
    objective = suppressWarnings(as.numeric(.cuopt_header(hdr, "Objective value")))
  )
}

.check_cuopt_status <- function(h, require_optimal, where) {
  if (require_optimal && !isTRUE(grepl("optimal", h$status, ignore.case = TRUE))) {
    stop("cuOpt (", where, ") finished with status '", h$status, "', not optimal.")
  }
}

.solve_mps_cuopt_local <- function(file, sol_file, control, require_optimal,
                                   verbose, wsl) {
  if (file.exists(sol_file)) file.remove(sol_file)
  t0 <- Sys.time()
  py <- identical(control$api, "python")
  warm <- !is.null(control$warm_start) || isTRUE(control$save_warm_start)
  if (!py && warm) stop("A cuOpt warm start needs control$api = \"python\".")
  if (!is.null(control$warm_start) && !file.exists(control$warm_start)) {
    stop("control$warm_start: no file ", control$warm_start)
  }
  # the watchdog (cuopt_watchdog()) reads the solver's log: `control$log_file`,
  # else a temporary file
  wd <- .cuopt_watchdog_resolve(control)
  out_dir <- dirname(normalizePath(sol_file, mustWork = FALSE))
  log_file <- control$log_file %||% tempfile("cuopt_", fileext = ".log")
  warm_file <- file.path(out_dir, "warm_start.pkl")
  # the Python interface: the script and its parameters in temporary files;
  # it writes the solution whatever the status
  script <- tempfile("solve_cuopt_", fileext = ".py")
  params <- tempfile("cuopt_params_", fileext = ".json")
  on.exit(unlink(c(script, params)), add = TRUE)
  if (py) {
    writeLines(.cuopt_py_source(), script)
    writeLines(as.character(jsonlite::toJSON(.cuopt_py_params(control, warm),
      auto_unbox = TRUE, digits = NA
    )), params)
  }
  solver_args <- function(sol, warm_out) {
    .cuopt_local_command(file, sol, warm_out, control, wsl, script, params)
  }
  exe <- solver_args("", "-")$exe
  if (!wsl) {
    args <- solver_args(sol_file, if (isTRUE(control$save_warm_start)) warm_file else "-")$args
    if (verbose) cat("cuOpt:", exe, paste(args, collapse = " "), "\n")
    run <- function(a) {
      if (!is.null(wd)) {
        return(.cuopt_run_watched(exe, a, log_file, wd, verbose))
      }
      system2(exe, shQuote(a),
        stdout = if (verbose) "" else FALSE,
        stderr = if (verbose) "" else FALSE
      )
    }
    rs <- run(args)
    if (!py && rs == 0 && !file.exists(sol_file) && is.null(control$presolve)) {
      rs <- run(c(args, "--presolve", "0"))
    }
  } else {
    # cuOpt writes the solution line by line, which is slow across the
    # /mnt/<drive> bridge: it writes to a temporary directory inside WSL's own
    # file system, removed afterwards, and the file is copied out in one
    # piece (the warm-start data likewise)
    user <- c(.wsl_user_args(control), "-e")
    wsl_dir <- suppressWarnings(system2("wsl", c(user, "mktemp", "-d"), stdout = TRUE, stderr = FALSE))
    if (!length(wsl_dir) || !startsWith(wsl_dir[1], "/")) {
      stop("cuOpt (WSL): could not create a temporary directory inside WSL.")
    }
    wsl_dir <- wsl_dir[1]
    on.exit(system2("wsl", c(user, "rm", "-rf", wsl_dir)), add = TRUE)
    wsl_sol <- paste0(wsl_dir, "/solution.sol")
    wsl_warm <- if (isTRUE(control$save_warm_start)) paste0(wsl_dir, "/warm_start.pkl") else "-"
    args <- c(user, exe, solver_args(wsl_sol, wsl_warm)$args)
    if (verbose) cat("cuOpt (WSL):", paste(args, collapse = " "), "\n")
    # under the watchdog the solver is started through bash, which writes its
    # process id first: stopping wsl.exe alone may leave the solver running
    pid_file <- paste0(wsl_dir, "/solver.pid")
    run <- function(a) {
      if (is.null(wd)) {
        return(system2("wsl", a,
          stdout = if (verbose) "" else FALSE,
          stderr = if (verbose) "" else FALSE
        ))
      }
      cmd <- c(user, "bash", "-c", sprintf('echo $$ > %s; exec "$0" "$@"', pid_file), a[-seq_along(user)])
      .cuopt_run_watched("wsl", cmd, log_file, wd, verbose,
        # cuopt_cli is a wrapper that starts the solver as its child: both go
        kill = function() {
          processx::run("wsl", c(
            user, "bash", "-c",
            sprintf("p=$(cat %s); pkill -P $p; kill $p", pid_file)
          ),
          error_on_status = FALSE
          )
        }
      )
    }
    in_wsl <- function(f) suppressWarnings(system2("wsl", c(user, "test", "-s", f))) == 0
    rs <- run(args)
    # cuopt_cli writes no solution file when its presolve solves the whole
    # problem (it reports Optimal and exits 0): solve once more without presolve
    if (!py && rs == 0 && !in_wsl(wsl_sol) && is.null(control$presolve)) {
      rs <- run(c(args, "--presolve", "0"))
    }
    for (f in c(wsl_sol, if (wsl_warm != "-") wsl_warm)) {
      if (in_wsl(f)) {
        to <- if (identical(f, wsl_sol)) sol_file else warm_file
        system2("wsl", c(user, "cp", f, .wsl_path(to)))
      }
    }
  }
  if (!file.exists(sol_file)) {
    stop(
      "cuOpt (", if (wsl) "WSL" else "local", ") wrote no solution (exit ", rs, "). ",
      "Check that `", if (wsl) paste(c("wsl", user), collapse = " ") else "", " ", exe,
      if (py) " -c 'import cuopt'" else " --version", "` runs and that the GPU is visible (`nvidia-smi`). Set ",
      if (py) "control$cuopt_python" else "control$cuopt_cli", if (wsl) " / control$wsl_user" else "",
      if (py) " or MULTIMOD_CUOPT_PYTHON" else " or MULTIMOD_CUOPT_CLI", if (wsl) " / MULTIMOD_WSL_USER" else "",
      " where cuOpt is not on the PATH."
    )
  }
  if (isTRUE(control$save_warm_start) && !file.exists(warm_file)) {
    warning("cuOpt: no warm-start data was saved (see the solver log).")
  }
  h <- .cuopt_sol_header(sol_file)
  .check_cuopt_status(h, require_optimal, if (wsl) "WSL" else "local")
  list(
    status_message = h$status, objective = h$objective, sol_file = sol_file,
    run_time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

# cuOpt's Python interface as a script: reads the matrix, applies the
# parameters (a JSON object of cuOpt parameter names), optionally a PDLP
# warm start, and writes the solution in `cuopt_cli`'s format (`# Status:`,
# `# Objective value:`, then one `name value` line per column) whatever the
# status, plus the warm-start data when asked.
.cuopt_py_source <- function() {
  c(
    "import sys, json, pickle",
    "import cuopt.linear_programming as lp",
    "mps, sol_out, params, warm_in, warm_out = sys.argv[1:6]",
    "warm_in, warm_out = ['' if w == '-' else w for w in (warm_in, warm_out)]",
    "with open(params) as fh:",
    "    p = json.load(fh)",
    "dm = lp.ParseMps(mps)",
    "s = lp.SolverSettings()",
    "for k, v in p.items():",
    "    if k == 'pdlp_solver_mode':",
    "        v = lp.PDLPSolverMode(int(v))",
    "    s.set_parameter(k, v)",
    "if warm_in:",
    "    with open(warm_in, 'rb') as fh:",
    "        s.set_pdlp_warm_start_data(pickle.load(fh))",
    "sol = lp.Solve(dm, s)",
    "reason = str(sol.get_termination_reason())",
    "obj = sol.get_primal_objective()",
    "x = sol.get_primal_solution()",
    "names = dm.get_variable_names()",
    "with open(sol_out, 'w') as fh:",
    "    fh.write('# Status: %s\\n' % reason)",
    "    fh.write('# Objective value: %.17g\\n' % obj)",
    "    fh.writelines('%s %.17g\\n' % (n, v) for n, v in zip(names, x))",
    "if warm_out:",
    "    try:",
    "        with open(warm_out, 'wb') as fh:",
    "            pickle.dump(sol.get_pdlp_warm_start_data(), fh)",
    "    except Exception as e:",
    "        print('WARM_START_NOT_SAVED: %s' % e)",
    "print('PY_STATUS: %s %.17g' % (reason, obj))"
  )
}

# The cuOpt parameters of a Python-interface run, from the same control
# fields as `cuopt_cli` (`options` given as `--name value` pairs).
.cuopt_py_params <- function(control, warm) {
  p <- list(method = as.integer(control$method %||% 1L))
  if (!is.null(control$time_limit)) p$time_limit <- as.numeric(control$time_limit)
  if (!is.null(control$presolve)) p$presolve <- as.integer(control$presolve)
  for (o in c("infeasibility_detection", "strict_infeasibility")) {
    v <- control[[o]] %||% getOption(paste0("multimod.cuopt_", o))
    if (!is.null(v)) p[[o]] <- isTRUE(v)
  }
  if (!is.null(control$options)) {
    tok <- unlist(strsplit(paste(control$options, collapse = " "), "[[:space:]]+"))
    tok <- tok[nzchar(tok)]
    for (i in seq(1, length(tok) - 1L, by = 2L)) {
      v <- tok[i + 1L]
      num <- suppressWarnings(as.numeric(v))
      p[[gsub("-", "_", sub("^--", "", tok[i]))]] <-
        if (v %in% c("true", "false")) v == "true" else if (!is.na(num)) num else v
    }
  }
  # PDLP warm-starts in its Stable2 mode only (both runs), and its warm-start
  # data is sized to the problem PDLP solved: after cuOpt's presolve that is
  # the presolved one, which the next run cannot take. Both runs skip it
  # unless `presolve` is set.
  if (warm) {
    p$pdlp_solver_mode <- 1L
    if (is.null(control$presolve)) p$presolve <- 0L
  }
  p
}
