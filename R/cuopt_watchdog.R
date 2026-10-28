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
  structure(list(poll = poll, runaway = runaway, runaway_n = runaway_n,
                 stall_window = stall_window, stall_iter = stall_iter,
                 stall_factor = stall_factor), class = "cuopt_watchdog")
}

#' @export
print.cuopt_watchdog <- function(x, ...) {
  cat("<cuopt_watchdog>\n")
  for (nm in names(x)) cat(sprintf("  %-13s %s\n", nm, format(x[[nm]])))
  invisible(x)
}

# The settings in force: the solve's, the session's or the defaults; NULL = off.
# A local run needs 'processx'; a cloud job runs the rules in its own script.
.cuopt_watchdog_resolve <- function(control, local = TRUE) {
  w <- control$watchdog
  if (is.null(w)) w <- getOption("multimod.cuopt_watchdog", cuopt_watchdog())
  if (is.null(w) || isFALSE(w)) return(NULL)
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
  if (!length(x)) return(NULL)
  m <- strsplit(trimws(x), "\\s+")
  m <- do.call(rbind, m[lengths(m) == 7L])
  if (is.null(m)) return(NULL)
  num <- function(v) suppressWarnings(as.numeric(sub("s$", "", v)))
  data.frame(iter = num(m[, 1]), pobj = num(m[, 2]), dobj = num(m[, 3]), gap = num(m[, 4]),
             pres = num(m[, 5]), dres = num(m[, 6]), time = num(m[, 7]))
}

# The verdict on the log so far: NULL (carry on) or the reason to stop.
.cuopt_watchdog_verdict <- function(it, w) {
  if (is.null(it) || nrow(it) < 2) return(NULL)
  last <- it[nrow(it), ]
  if (is.finite(w$runaway) && is.finite(w$runaway_n)) {
    big <- pmax(abs(it$pobj), abs(it$dobj), na.rm = TRUE) > w$runaway
    big[is.na(big)] <- FALSE
    r <- rle(big)
    k <- utils::tail(r$lengths, 1)
    if (utils::tail(big, 1) && k >= w$runaway_n) {
      return(sprintf("objective beyond %g for %d log lines (primal %.3g, dual %.3g)",
                     w$runaway, k, last$pobj, last$dobj))
    }
  }
  if (is.finite(w$stall_factor)) {
    stalled <- function(inside) {
      before <- it[!inside, , drop = FALSE]; now <- it[inside, , drop = FALSE]
      if (!nrow(before) || !nrow(now)) return(FALSE)
      b <- function(v) suppressWarnings(min(v, na.rm = TRUE))
      all(c(b(now$pres) > b(before$pres) / w$stall_factor,
            b(now$dres) > b(before$dres) / w$stall_factor,
            b(now$gap) > b(before$gap) / w$stall_factor))
    }
    if (is.finite(w$stall_window) && last$time - it$time[1] >= w$stall_window &&
        stalled(it$time > last$time - w$stall_window)) {
      return(sprintf("no progress by a factor %g in the last %g s (primal res %.3g, dual res %.3g, gap %.3g)",
                     w$stall_factor, w$stall_window, last$pres, last$dres, last$gap))
    }
    if (is.finite(w$stall_iter) && last$iter - it$iter[1] >= w$stall_iter &&
        stalled(it$iter > last$iter - w$stall_iter)) {
      return(sprintf("no progress by a factor %g in the last %s iterations (primal res %.3g, dual res %.3g, gap %.3g)",
                     w$stall_factor, format(w$stall_iter, big.mark = ",", scientific = FALSE),
                     last$pres, last$dres, last$gap))
    }
  }
  NULL
}

# The same two rules for a cloud job, as an awk program over the cuopt_cli log:
# prints the reason to stop, or nothing. An NA threshold is passed as 0 (off).
.cuopt_watchdog_awk <- function() {
  c('function a(x) { return x < 0 ? -x : x }',
    'function stall(x, cut,   i, bp, bd, bg, ip, id, ig, nb, ni) {',
    '  bp = bd = bg = ip = id = ig = 1e308; nb = ni = 0',
    '  for (i = 1; i <= n; i++) {',
    '    if (x[i] > cut) { ni++; if (pr[i] < ip) ip = pr[i]; if (dr[i] < id) id = dr[i]; if (g[i] < ig) ig = g[i] }',
    '    else { nb++; if (pr[i] < bp) bp = pr[i]; if (dr[i] < bd) bd = dr[i]; if (g[i] < bg) bg = g[i] }',
    '  }',
    '  return (nb > 0 && ni > 0 && ip > bp / F && id > bd / F && ig > bg / F)',
    '}',
    '/^ *[0-9]+ +[-+]?[0-9.]+e[-+][0-9]+ / && NF == 7 {',
    '  n++; it[n] = $1 + 0; po[n] = $2 + 0; du[n] = $3 + 0; g[n] = $4 + 0; pr[n] = $5 + 0; dr[n] = $6 + 0',
    '  t = $7; sub(/s$/, "", t); tm[n] = t + 0',
    '  if (R > 0 && (a(po[n]) > R || a(du[n]) > R)) c++; else c = 0',
    '}',
    'END {',
    '  if (n < 2) exit',
    '  if (R > 0 && N > 0 && c >= N) { printf "objective beyond %g for %d log lines (primal %.3g, dual %.3g)\\n", R, c, po[n], du[n]; exit }',
    '  if (F > 0 && SW > 0 && tm[n] - tm[1] >= SW && stall(tm, tm[n] - SW)) { printf "no progress by a factor %g in the last %g s\\n", F, SW; exit }',
    '  if (F > 0 && SI > 0 && it[n] - it[1] >= SI && stall(it, it[n] - SI)) { printf "no progress by a factor %g in the last %d iterations\\n", F, SI; exit }',
    '}')
}

# The awk variables of the settings (an NA rule is 0: off)
.cuopt_watchdog_awk_vars <- function(w) {
  v <- function(x) if (is.finite(x)) format(x, scientific = FALSE) else "0"
  sprintf("-v R=%s -v N=%s -v SW=%s -v SI=%s -v F=%s", v(w$runaway), v(w$runaway_n),
          v(w$stall_window), v(w$stall_iter), v(w$stall_factor))
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
         call. = FALSE)
  }
  p$get_exit_status()
}
