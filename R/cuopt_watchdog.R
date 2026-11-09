# cuOpt watchdog --------------------------------------------------------------
#
# cuOpt's PDLP runs until its time limit when a problem diverges: a dual
# objective that runs away (~1e22 on an infeasible problem) or residuals that
# stop improving. The watchdog reads cuopt_cli's log while it runs and stops it
# on either sign. Every threshold is a setting: per solve (`control$watchdog`),
# per session (`options(multimod.cuopt_watchdog = )`) or the defaults below.

#' cuOpt watchdog settings
#'
#' Thresholds for stopping a cuOpt (PDLP) solve that diverges or stalls. The
#' watchdog reads `cuopt_cli`'s iteration log every `poll` seconds and stops
#' the solve when
#' * **runaway**: the primal or dual objective stays beyond `runaway` in
#'   absolute value for `runaway_n` consecutive log lines (cuOpt logs every
#'   1,000 iterations), or
#' * **stall**: over the last `stall_window` seconds, or the last `stall_iter`
#'   iterations, none of the primal residual, the dual residual and the gap
#'   improved on its best value before that window by a factor `stall_factor`.
#'
#' A rule whose threshold is `NA` is off. Pass `FALSE` as `control$watchdog`
#' (or the session option) to turn the watchdog off; it also stays off when
#' the 'processx' package is not installed.
#'
#' Precedence: `control$watchdog` of the solve, then
#' `getOption("multimod.cuopt_watchdog")`, then `cuopt_watchdog()`.
#'
#' @param poll seconds between log reads.
#' @param runaway,runaway_n the runaway rule.
#' @param stall_window,stall_iter,stall_factor the stall rule.
#' @return a `cuopt_watchdog` list.
#' @export
#' @examples
#' cuopt_watchdog()
#' # a longer stall window for a slow but healthy model
#' w <- cuopt_watchdog(stall_window = 3600)
#' # control$watchdog <- w   (or FALSE to turn it off)
cuopt_watchdog <- function(poll = 30, runaway = 1e20, runaway_n = 5,
                           stall_window = 1800, stall_iter = 5e5, stall_factor = 2) {
  structure(list(
    poll = poll, runaway = runaway, runaway_n = runaway_n,
    stall_window = stall_window, stall_iter = stall_iter,
    stall_factor = stall_factor
  ), class = "cuopt_watchdog")
}

#' @export
print.cuopt_watchdog <- function(x, ...) {
  cat("<cuopt_watchdog>\n")
  for (nm in names(x)) cat(sprintf("  %-13s %s\n", nm, format(x[[nm]])))
  invisible(x)
}

# The settings in force: the solve's, the session's or the defaults; NULL = off.
# The watchdog needs 'processx'.
.cuopt_watchdog_resolve <- function(control, local = TRUE) {
  w <- control$watchdog
  if (is.null(w)) w <- getOption("multimod.cuopt_watchdog", cuopt_watchdog())
  if (is.null(w) || isFALSE(w)) {
    return(NULL)
  }
  if (!inherits(w, "cuopt_watchdog")) w <- do.call(cuopt_watchdog, as.list(w))
  if (local && !requireNamespace("processx", quietly = TRUE)) {
    message("cuOpt watchdog off: the 'processx' package is not installed.")
    return(NULL)
  }
  w
}

# The PDLP iteration lines of a cuopt_cli log:
#   Iter  Primal Obj.  Dual Obj.  Gap  Primal Res.  Dual Res.  Time
.cuopt_log_iters <- function(lines) {
  x <- grep("^\\s*[0-9]+\\s+[-+]?[0-9.]+e[-+][0-9]+\\s", lines, value = TRUE)
  if (!length(x)) {
    return(NULL)
  }
  m <- strsplit(trimws(x), "\\s+")
  m <- do.call(rbind, m[lengths(m) == 7L])
  if (is.null(m)) {
    return(NULL)
  }
  num <- function(v) suppressWarnings(as.numeric(sub("s$", "", v)))
  data.frame(
    iter = num(m[, 1]), pobj = num(m[, 2]), dobj = num(m[, 3]), gap = num(m[, 4]),
    pres = num(m[, 5]), dres = num(m[, 6]), time = num(m[, 7])
  )
}

# The verdict on the log so far: NULL (carry on) or the reason to stop.
.cuopt_watchdog_verdict <- function(it, w) {
  if (is.null(it) || nrow(it) < 2) {
    return(NULL)
  }
  last <- it[nrow(it), ]
  if (is.finite(w$runaway) && is.finite(w$runaway_n)) {
    big <- pmax(abs(it$pobj), abs(it$dobj), na.rm = TRUE) > w$runaway
    big[is.na(big)] <- FALSE
    r <- rle(big)
    k <- utils::tail(r$lengths, 1)
    if (utils::tail(big, 1) && k >= w$runaway_n) {
      return(sprintf(
        "objective beyond %g for %d log lines (primal %.3g, dual %.3g)",
        w$runaway, k, last$pobj, last$dobj
      ))
    }
  }
  if (is.finite(w$stall_factor)) {
    stalled <- function(inside) {
      before <- it[!inside, , drop = FALSE]
      now <- it[inside, , drop = FALSE]
      if (!nrow(before) || !nrow(now)) {
        return(FALSE)
      }
      b <- function(v) suppressWarnings(min(v, na.rm = TRUE))
      all(c(
        b(now$pres) > b(before$pres) / w$stall_factor,
        b(now$dres) > b(before$dres) / w$stall_factor,
        b(now$gap) > b(before$gap) / w$stall_factor
      ))
    }
    if (is.finite(w$stall_window) && last$time - it$time[1] >= w$stall_window &&
      stalled(it$time > last$time - w$stall_window)) {
      return(sprintf(
        "no progress by a factor %g in the last %g s (primal res %.3g, dual res %.3g, gap %.3g)",
        w$stall_factor, w$stall_window, last$pres, last$dres, last$gap
      ))
    }
    if (is.finite(w$stall_iter) && last$iter - it$iter[1] >= w$stall_iter &&
      stalled(it$iter > last$iter - w$stall_iter)) {
      return(sprintf(
        "no progress by a factor %g in the last %s iterations (primal res %.3g, dual res %.3g, gap %.3g)",
        w$stall_factor, format(w$stall_iter, big.mark = ",", scientific = FALSE),
        last$pres, last$dres, last$gap
      ))
    }
  }
  NULL
}

# One cuopt_cli call under the watchdog: the process writes its log to
# `log_file`, which is read every `poll` seconds (and echoed when verbose); on
# a verdict `kill()` stops the solver (inside WSL: its own process id) and the
# call ends with an error carrying the reason and the last log lines.
.cuopt_run_watched <- function(cmd, args, log_file, w, verbose, kill = NULL) {
  p <- processx::process$new(cmd, args, stdout = log_file, stderr = "2>&1", cleanup_tree = TRUE)
  seen <- 0L
  reason <- NULL
  repeat {
    p$wait(timeout = w$poll * 1000)
    lines <- tryCatch(readLines(log_file, warn = FALSE), error = function(e) character())
    if (verbose && length(lines) > seen) cat(lines[(seen + 1L):length(lines)], sep = "\n")
    seen <- length(lines)
    if (!p$is_alive()) break
    reason <- .cuopt_watchdog_verdict(.cuopt_log_iters(lines), w)
    if (!is.null(reason)) {
      if (!is.null(kill)) try(kill(), silent = TRUE)
      p$kill_tree()
      break
    }
  }
  if (!is.null(reason)) {
    stop("cuOpt stopped by the watchdog: ", reason, ". Log: ", log_file, "\n",
      paste(utils::tail(lines, 5), collapse = "\n"),
      "\nChange or turn off with control$watchdog = cuopt_watchdog(...) / FALSE.",
      call. = FALSE
    )
  }
  p$get_exit_status()
}
