# =============================================================================
# Solving a written MPS file: one entry point, several solvers and places
# =============================================================================
#
# The matrix is a file; the solver reads it and writes a solution file that
# read_mps_solution() decodes against the index tables. Which solver and where
# it runs (in this process, a local executable, WSL, a rented machine) only
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
#' * `"cuopt"`, `"remote"`: a rented GPU through the 'mmcloud' package: the
#'   matrix is compressed and uploaded, the job's handle is written to
#'   `remote$handle_file` before the wait, and the compressed solution is
#'   fetched back.
#'
#' `control` holds the solver's options. HiGHS (both places): named HiGHS
#' options, e.g. `list(solver = "ipm")`. cuOpt: `method` (default `1`, PDLP),
#' `time_limit` (seconds; `cuopt_cli`'s own default is short), `presolve`,
#' `options` (further `cuopt_cli` flags as one string or a character vector);
#' for `"wsl"` also `wsl_user` and `cuopt_cli` (path inside WSL), else the
#' environment variables `MULTIMOD_WSL_USER` and `MULTIMOD_CUOPT_CLI`, else
#' WSL's default user and `cuopt_cli` on its `PATH`.
#'
#' `remote` (for `where = "remote"`): `flavor`, `namespace`, `timeout`
#' (default `"10h"`), `image` (default `"python:3.12"`), `provider` (default
#' `"hf_jobs"`), `handle_file` (default `cloud_job.yml` beside `sol_file`),
#' `name`, `poll` (seconds) and `max_wait` (seconds).
#'
#' @param file Path to the MPS file. HiGHS in this process reads it
#'   uncompressed; cuOpt reads `.gz` as well.
#' @param sol_file Path for the solution file; default `file` with a `.sol`
#'   extension (`.sol.gz` for `where = "remote"`, which always returns it
#'   compressed).
#' @param control Solver options; see Details.
#' @param require_optimal Logical; error unless the run ends optimal.
#' @param verbose Logical; report progress.
#' @param solver `"highs"`, `"julia_highs"` or `"cuopt"`.
#' @param where `"local"`, `"wsl"` or `"remote"`.
#' @param remote Settings of a remote run; see Details.
#'
#' @return A list with `status_message`, `objective`, `sol_file` and
#'   `run_time` (seconds); a remote run adds `job`, the job handle.
#' @export
solve_mps <- function(file, sol_file = NULL, control = list(),
                      require_optimal = TRUE, verbose = FALSE,
                      solver = c("highs", "julia_highs", "cuopt"),
                      where = c("local", "wsl", "remote"),
                      remote = list()) {
  solver <- match.arg(solver)
  where <- match.arg(where)
  if (!file.exists(file)) stop("MPS file not found: ", file)
  if (length(control) && (!is.list(control) || is.null(names(control)) ||
                          any(!nzchar(names(control))))) {
    stop("`control` must be a named list of solver options.")
  }
  key <- paste(solver, where, sep = "/")
  if (is.null(sol_file)) {
    sol_file <- paste0(sub("\\.mps(\\.gz)?$", "", file, ignore.case = TRUE),
                       if (identical(where, "remote")) ".sol.gz" else ".sol")
  }
  switch(key,
    "highs/local"       = .solve_mps_highs(file, sol_file, control, require_optimal, verbose),
    "julia_highs/local" = .solve_mps_julia(file, sol_file, control, require_optimal, verbose),
    "cuopt/local"       = .solve_mps_cuopt_local(file, sol_file, control, require_optimal, verbose, wsl = FALSE),
    "cuopt/wsl"         = .solve_mps_cuopt_local(file, sol_file, control, require_optimal, verbose, wsl = TRUE),
    "cuopt/remote"      = .solve_mps_cuopt_remote(file, sol_file, control, require_optimal, verbose, remote),
    stop("solver = '", solver, "' does not run where = '", where, "'. Available: ",
         "highs/local, julia_highs/local, cuopt/local, cuopt/wsl, cuopt/remote.")
  )
}

# HiGHS in this process ------------------------------------------------------

.solve_mps_highs <- function(file, sol_file, control, require_optimal, verbose) {
  if (!requireNamespace("highs", quietly = TRUE)) {
    stop("solve_mps() needs the 'highs' package. Install it with ",
         "install.packages(\"highs\").")
  }
  if (grepl("\\.gz$", file, ignore.case = TRUE)) {
    stop("The HiGHS reader does not accept gzipped MPS; decompress first.")
  }

  s <- highs::hi_new_solver(highs::highs_model(L = 0, lower = 0, upper = 0))
  highs::hi_solver_read_model(s, file)
  if (length(control)) {
    for (nm in names(control)) highs::hi_solver_set_option(s, nm, control[[nm]])
  }
  highs::hi_solver_run(s)

  msg <- highs::hi_solver_status_message(s)
  if (require_optimal && !grepl("optimal", msg, ignore.case = TRUE)) {
    stop("HiGHS finished with status '", msg, "', not optimal.\n",
         "  Pass require_optimal = FALSE to write the solution anyway.")
  }

  sol <- highs::hi_solver_get_solution(s)
  objective <- sum(highs::hi_solver_get_lp_costs(s) * sol$col_value)
  n <- length(sol$col_value)
  m <- length(sol$row_value)

  # Raw-style solution file (the format read_highs_solution() parses); dual
  # sections are written only when the solver reports valid duals.
  con <- file(sol_file, "wb")   # binary: LF endings, byte-stable across OSes
  wr <- function(...) writeLines(c(...), con, sep = "\n")
  wr("Model status", msg, "")
  wr("# Primal solution values", "Feasible",
     sprintf("Objective %.17g", objective))
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
    message("solved ", basename(file), ": ", msg, ", objective ",
            format(objective, digits = 10), " -> ", sol_file)
  }
  list(status_message = msg, objective = objective, sol_file = sol_file,
       run_time = highs::hi_solver_get_run_time(s))
}

# HiGHS through Julia --------------------------------------------------------

# HiGHS.jl's C API reads the file and writes the solution itself: no JuMP model
# is built. The script carries the options typed from R and is written beside
# the solution file.
.julia_highs_script <- function(control) {
  setter <- function(nm, v) {
    if (is.logical(v)) sprintf('HiGHS.Highs_setBoolOptionValue(h, "%s", %s)', nm, if (isTRUE(v)) "1" else "0")
    else if (is.integer(v)) sprintf('HiGHS.Highs_setIntOptionValue(h, "%s", %d)', nm, v)
    else if (is.numeric(v)) sprintf('HiGHS.Highs_setDoubleOptionValue(h, "%s", %s)', nm, format(v, digits = 17))
    else sprintf('HiGHS.Highs_setStringOptionValue(h, "%s", "%s")', nm, as.character(v))
  }
  c("using HiGHS",
    "h = HiGHS.Highs_create()",
    "t0 = time()",
    "HiGHS.Highs_readModel(h, ARGS[1]) == 0 || error(\"HiGHS could not read \" * ARGS[1])",
    vapply(names(control), function(nm) setter(nm, control[[nm]]), ""),
    "HiGHS.Highs_run(h)",
    "t1 = time()",
    "HiGHS.Highs_writeSolution(h, ARGS[2])",
    "st = HiGHS.Highs_getModelStatus(h)",
    "println(\"MMJULIA status=\", st, \" objective=\", HiGHS.Highs_getObjectiveValue(h), \" seconds=\", round(t1 - t0, digits = 1))",
    "HiGHS.Highs_destroy(h)")
}

# HiGHS' model status codes (HighsModelStatus), as Highs_getModelStatus returns them.
.highs_model_status <- function(code) {
  lab <- c("0" = "Not Set", "1" = "Load error", "2" = "Model error",
           "3" = "Presolve error", "4" = "Solve error", "5" = "Postsolve error",
           "6" = "Empty", "7" = "Optimal", "8" = "Infeasible",
           "9" = "Primal infeasible or unbounded", "10" = "Unbounded",
           "11" = "Bound on objective reached", "12" = "Target for objective reached",
           "13" = "Time limit reached", "14" = "Iteration limit reached",
           "15" = "Unknown", "16" = "Solution limit reached", "17" = "Interrupted",
           "18" = "Memory limit reached")
  out <- unname(lab[as.character(code)])
  ifelse(is.na(out), paste("status", code), out)
}

.solve_mps_julia <- function(file, sol_file, control, require_optimal, verbose) {
  jl <- file.path(dirname(sol_file), "solve_mps.jl")
  writeLines(.julia_highs_script(control), jl)
  if (file.exists(sol_file)) file.remove(sol_file)
  t0 <- Sys.time()
  out <- suppressWarnings(system2(get_multimod_julia(),
                                  c(shQuote(jl), shQuote(file), shQuote(sol_file)),
                                  stdout = TRUE, stderr = TRUE))
  if (verbose) cat(out[!grepl("^MMJULIA ", out)], sep = "\n")
  if (!file.exists(sol_file)) {
    stop("HiGHS (Julia) wrote no solution (exit ", attr(out, "status") %||% 0L, ").\n",
         paste(utils::tail(out, 15), collapse = "\n"))
  }
  line <- utils::tail(grep("^MMJULIA ", out, value = TRUE), 1)
  msg <- if (length(line)) .highs_model_status(sub(".*status=(\\S+) objective=.*", "\\1", line)) else NA_character_
  obj <- if (length(line)) suppressWarnings(as.numeric(sub(".*objective=(\\S+).*", "\\1", line))) else NA_real_
  if (require_optimal && !isTRUE(grepl("optimal", msg, ignore.case = TRUE))) {
    stop("HiGHS (Julia) finished with status '", msg, "', not optimal.")
  }
  list(status_message = msg, objective = obj, sol_file = sol_file,
       run_time = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

# cuOpt ----------------------------------------------------------------------

# `cuopt_cli`'s arguments after the input file: pure, so the command can be
# checked without a GPU. An unset time limit means cuopt_cli's own short one,
# so callers pass it.
.cuopt_options <- function(control) {
  opts <- c("--method", as.character(control$method %||% 1L))
  if (!is.null(control$time_limit)) opts <- c(opts, "--time-limit", as.character(control$time_limit))
  if (!is.null(control$presolve)) opts <- c(opts, "--presolve", as.character(as.integer(control$presolve)))
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
  if (startsWith(x, "/")) return(x)
  x <- normalizePath(x, winslash = "/", mustWork = FALSE)
  drv <- regmatches(x, regexpr("^[A-Za-z]:", x))
  if (!length(drv)) return(x)
  paste0("/mnt/", tolower(substr(drv, 1, 1)), substring(x, 3))
}

.wsl_user_args <- function(control) {
  u <- control$wsl_user %||% Sys.getenv("MULTIMOD_WSL_USER", "")
  if (nzchar(u)) c("-u", u) else character(0)
}

.cuopt_cli_path <- function(control, wsl) {
  control$cuopt_cli %||%
    Sys.getenv("MULTIMOD_CUOPT_CLI",
               if (wsl) "cuopt_cli" else get_multimod_solver("cuopt_cli"))
}

# The status and objective from the solution file's header (`# Status: ...`,
# `# Objective value: ...`), without reading its body.
.cuopt_sol_header <- function(sol_file) {
  con <- if (grepl("\\.gz$", sol_file, ignore.case = TRUE)) gzfile(sol_file, "rt") else file(sol_file, "rt")
  on.exit(close(con))
  hdr <- readLines(con, n = 10, warn = FALSE)
  list(status = .cuopt_header(hdr, "Status"),
       objective = suppressWarnings(as.numeric(.cuopt_header(hdr, "Objective value"))))
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
  cli <- .cuopt_cli_path(control, wsl)
  if (!wsl) {
    args <- .cuopt_cli_args(file, sol_file, control)
    if (verbose) cat("cuOpt:", cli, paste(args, collapse = " "), "\n")
    rs <- system2(cli, shQuote(args), stdout = if (verbose) "" else FALSE,
                  stderr = if (verbose) "" else FALSE)
    if (rs == 0 && !file.exists(sol_file) && is.null(control$presolve)) {
      rs <- system2(cli, shQuote(c(args, "--presolve", "0")),
                    stdout = if (verbose) "" else FALSE, stderr = if (verbose) "" else FALSE)
    }
  } else {
    # cuOpt writes the solution line by line, which is slow across the
    # /mnt/<drive> bridge: it writes inside WSL's own file system and the
    # file is copied out in one piece
    wsl_sol <- sprintf("/tmp/multimod_%s_%d.sol", basename(dirname(normalizePath(sol_file, mustWork = FALSE))),
                       Sys.getpid())
    user <- c(.wsl_user_args(control), "-e")
    args <- c(user, cli, .cuopt_cli_args(.wsl_path(file), wsl_sol, control))
    if (verbose) cat("cuOpt (WSL):", paste(args, collapse = " "), "\n")
    run <- function(a) system2("wsl", a, stdout = if (verbose) "" else FALSE,
                               stderr = if (verbose) "" else FALSE)
    sol_in_wsl <- function() suppressWarnings(system2("wsl", c(user, "test", "-s", wsl_sol))) == 0
    rs <- run(args)
    # cuOpt writes no solution file when its presolve solves the whole problem
    # (it reports Optimal and exits 0): solve once more without presolve
    if (rs == 0 && !sol_in_wsl() && is.null(control$presolve)) {
      rs <- run(c(args, "--presolve", "0"))
    }
    if (sol_in_wsl()) {
      system2("wsl", c(user, "cp", wsl_sol, .wsl_path(sol_file)))
      system2("wsl", c(user, "rm", "-f", wsl_sol))
    }
  }
  if (!file.exists(sol_file)) {
    stop("cuOpt (", if (wsl) "WSL" else "local", ") wrote no solution (exit ", rs, "). ",
         "Check that `", if (wsl) paste(c("wsl", user), collapse = " ") else "", " ", cli,
         " --version` runs and that the GPU is visible (`nvidia-smi`). Set ",
         "control$cuopt_cli", if (wsl) " / control$wsl_user" else "",
         " or MULTIMOD_CUOPT_CLI", if (wsl) " / MULTIMOD_WSL_USER" else "",
         " where cuOpt is not on the PATH.")
  }
  h <- .cuopt_sol_header(sol_file)
  .check_cuopt_status(h, require_optimal, if (wsl) "WSL" else "local")
  list(status_message = h$status, objective = h$objective, sol_file = sol_file,
       run_time = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

# The script a rented machine runs. The image is a plain Python one: cuOpt's
# wheels carry their own CUDA runtime, so the driver is enough. The install is
# repeated because a download that stalls mid-file is not retried by pip. The
# solution is written to the container's own disk and compressed into /out in
# one pass: /out is the volume the platform uploads after the script ends, and
# that upload is billed time on the GPU machine. GPU memory is sampled each
# second; the peak goes to the log.
.cuopt_worker_script <- function(mps_name, out_name, control) {
  solve <- paste(c("cuopt_cli", .cuopt_cli_args(paste0("/job/", mps_name), "/tmp/model.sol", control)),
                 collapse = " ")
  c("set -e",
    "nvidia-smi || true",
    paste("for i in 1 2 3 4 5; do",
          "pip install -q --no-cache-dir --retries 10 --timeout 120 cuopt-cu12 && break;",
          "echo \"pip install attempt $i failed\"; sleep 15; done"),
    "(nvidia-smi --query-gpu=memory.used --format=csv,noheader,nounits -l 1 > /tmp/vram.log 2>/dev/null &) || true",
    solve,
    # cuOpt writes no solution file when its presolve solves the whole problem
    # (it reports Optimal and exits 0): solve once more without presolve
    if (is.null(control$presolve)) paste("[ -s /tmp/model.sol ] ||", solve, "--presolve 0"),
    "echo VRAM_PEAK_MIB $(sort -n /tmp/vram.log 2>/dev/null | tail -1)",
    paste0("gzip -1 -c /tmp/model.sol > /out/", out_name),
    "ls -l /out")
}

.solve_mps_cuopt_remote <- function(file, sol_file, control, require_optimal,
                                    verbose, remote) {
  if (!requireNamespace("mmcloud", quietly = TRUE)) {
    stop("where = 'remote' needs the 'mmcloud' package.")
  }
  if (!grepl("\\.gz$", sol_file, ignore.case = TRUE)) {
    stop("A remote solve returns the solution compressed: `sol_file` must end in .gz.")
  }
  t0 <- Sys.time()
  dir <- dirname(sol_file)
  gz <- if (grepl("\\.gz$", file, ignore.case = TRUE)) file else {
    mmcloud::cloud_compress(file, level = 1L, echo = verbose)
  }
  out_name <- basename(sol_file)
  if (verbose) message(sprintf("uploading %.2f GB", file.size(gz) / 1e9))
  job <- mmcloud::cloud_run(
    inputs = gz, outputs = out_name,
    script = .cuopt_worker_script(basename(gz), out_name, control),
    provider = remote$provider %||% "hf_jobs",
    flavor = remote$flavor %||% "a10g-large",
    timeout = remote$timeout %||% "10h",
    namespace = remote$namespace,
    image = remote$image %||% "python:3.12",
    name = remote$name %||% paste0("multimod-", tools::file_path_sans_ext(basename(file))),
    echo = verbose)
  # the handle outlives this call: a killed session can still poll, cancel or
  # fetch the job
  handle <- remote$handle_file %||% file.path(dir, "cloud_job.yml")
  tryCatch(mmcloud::cloud_handle_write(job, handle), error = function(e) NULL)
  # nothing of the matrix is needed through the wait
  clear_data_cache()
  gc(FALSE)
  stage <- mmcloud::cloud_wait(job, poll = remote$poll %||% 15,
                               max_wait = remote$max_wait %||% Inf, echo = verbose)
  ok <- tryCatch({
    mmcloud::cloud_fetch(job, dir, echo = verbose)
    TRUE
  }, error = function(e) e)
  if (!isTRUE(ok) || !identical(stage, "COMPLETED") || !file.exists(sol_file)) {
    # a job can end cleanly and return nothing (infeasible, time limit,
    # cancelled): report the solver's own conclusion where there is one
    v <- tryCatch(mmcloud::cloud_verdict(job), error = function(e) NULL)
    why <- if (!is.null(v) && !identical(v$verdict, "ok")) v$message else
      if (inherits(ok, "error")) conditionMessage(ok) else paste("job ended", stage)
    stop("Remote cuOpt solve returned no solution (job ", job$id, "): ", why)
  }
  h <- .cuopt_sol_header(sol_file)
  .check_cuopt_status(h, require_optimal, "remote")
  list(status_message = h$status, objective = h$objective, sol_file = sol_file,
       run_time = as.numeric(difftime(Sys.time(), t0, units = "secs")),
       job = job)
}
