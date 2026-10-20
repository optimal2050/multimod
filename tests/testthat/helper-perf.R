# Per-test cost accounting ####################################################
#
# testthat records how many expectations passed, never what they cost. A suite
# that takes an hour tells you nothing about WHERE the hour went, and a test
# that quietly grows from 2s to 200s looks identical to one that did not.
#
# Two things are recorded per measured block: wall-clock seconds, and the peak
# R heap during it (gc's own max-used counter, reset first, which is the only
# memory figure R reports honestly without an external sampler). Peak matters
# more than the delta -- a block that allocates 8 GB and frees it will not show
# up in a before/after difference, but it is exactly the block that will fail
# on a smaller machine.
#
# MMCLOUD_PERF_LOG names a CSV to append to; unset, measurements are kept in
# memory and printed by perf_summary().

.perf_env <- new.env(parent = emptyenv())
.perf_env$rows <- list()

#' Measure one block, return its value unchanged
#'
#' Wrap the expensive part of a test, not the whole test -- the point is to
#' attribute cost, and wrapping everything attributes it to nothing.
#'
#' @param label Short name for the measured block.
#' @param expr Expression to evaluate.
#' @return Whatever `expr` returns.
perf <- function(label, expr) {
  gc(reset = TRUE, full = TRUE)
  t0 <- proc.time()[["elapsed"]]
  on.exit({
    el <- proc.time()[["elapsed"]] - t0
    g <- gc(full = FALSE)
    # gc() columns 6:7 are "max used" for cells (Ncells) and vectors (Vcells),
    # already in MB.
    peak_mb <- sum(g[, 6], na.rm = TRUE)
    .perf_record(label, el, peak_mb)
  }, add = TRUE)
  force(expr)
}

.perf_record <- function(label, seconds, peak_mb) {
  row <- data.frame(
    file = .perf_current_file(),
    label = label,
    seconds = round(seconds, 2),
    peak_mb = round(peak_mb, 1),
    stringsAsFactors = FALSE
  )
  .perf_env$rows[[length(.perf_env$rows) + 1L]] <- row

  f <- Sys.getenv("MMCLOUD_PERF_LOG", unset = "")
  if (nzchar(f)) {
    utils::write.table(row, f, sep = ",", row.names = FALSE,
                       col.names = !file.exists(f), append = file.exists(f))
  }
  invisible(row)
}

# testthat does not expose the running file, so fall back to the call stack.
.perf_current_file <- function() {
  for (i in seq_len(sys.nframe())) {
    f <- tryCatch(sys.frame(i)$test_path, error = function(e) NULL)
    if (!is.null(f)) return(basename(f))
  }
  Sys.getenv("MMCLOUD_PERF_FILE", unset = NA_character_)
}

#' Everything measured so far, slowest first
perf_summary <- function() {
  if (!length(.perf_env$rows)) {
    return(data.frame(file = character(), label = character(),
                      seconds = numeric(), peak_mb = numeric()))
  }
  d <- do.call(rbind, .perf_env$rows)
  d[order(-d$seconds), , drop = FALSE]
}

# --- guards for external tools -----------------------------------------------
#
# These belong beside the timing because they are the same problem: a test that
# shells out to a missing tool does not fail, it waits. `julia` on Windows is
# usually a Microsoft Store App Execution Alias -- invoking it when Julia is not
# installed opens the Store and blocks forever, which is indistinguishable from
# a slow solve.

.tool_available <- function(cmd, args = "--version", timeout = 20) {
  path <- Sys.which(cmd)
  if (!nzchar(path)) return(FALSE)
  ok <- tryCatch({
    out <- suppressWarnings(system2(path, args, stdout = TRUE, stderr = TRUE,
                                    timeout = timeout))
    st <- attr(out, "status")
    is.null(st) || identical(as.integer(st), 0L)
  }, error = function(e) FALSE)
  isTRUE(ok)
}

skip_without_tool <- function(cmd, args = "--version") {
  testthat::skip_if(!.tool_available(cmd, args),
                    paste0("`", cmd, "` not available (or not responding)"))
}

skip_without_glpsol <- function() skip_without_tool("glpsol", "--version")
skip_without_julia  <- function() skip_without_tool("julia", "--version")
