# =============================================================================
# Doubleton elimination: x = c * y equality rows removed before solving
# =============================================================================
#
# Many columns of a generated LP are exact multiples of another: an activity
# equal to its single output, an auxiliary flow proportional to an output. Each
# such pair is an equality row with two nonzeros and a zero right-hand side,
# a * x + b * y = 0. Substituting x = c * y (c = -b / a) removes the row and the
# column; the solver never sees either, and the solution is expanded back.
#
# The reduction is exact: y takes x's column (scaled by c) in every other row
# and in the objective, and x's bounds become bounds on y. It runs in passes,
# so chains (x = c y, y = d z) resolve; within a pass no eliminated column is
# also a target, which keeps the substitution matrix a single product.
# =============================================================================

#' Presolve an assembled LP
#'
#' Exact reductions applied before the matrix is handed to a solver. The one
#' step implemented, `"doubletons"`, removes every equality row of the form `a * x + b * y = 0` together with
#' column `x`, substituting `x = c * y` (`c = -b / a`) into the rest of the
#' matrix, the objective and `y`'s bounds. Integer columns are never
#' eliminated. The reduced LP solves to the same optimum; [read_mps_solution()]
#' restores the eliminated columns from the table kept in the result.
#'
#' @param lp An LP from [model_to_lp()].
#' @param steps Reductions to apply; `"doubletons"` is the only one so far.
#' @param max_pass Maximum number of passes; chains need one pass per link.
#' @param tol Entries smaller than this in absolute value after substitution
#'   are dropped (exact cancellations that floating point leaves behind).
#' @param verbose Logical; report each pass.
#'
#' @return The LP with `A`, `obj` and the bounds reduced. `col_index` and
#'   `row_index` stay complete and gain `jred` / `ired`, the position of each
#'   kept column / row in the reduced LP (`NA` when eliminated); `eliminated`
#'   lists the removed columns (`col`, `ref`, `coef`, `pass`, positions in the
#'   full index) so that `value[col] = coef * value[ref]`, applied from the last
#'   pass to the first.
#' @export
lp_presolve <- function(lp, steps = "doubletons", max_pass = 50L,
                        tol = 1e-12, verbose = FALSE) {
  steps <- match.arg(steps, "doubletons", several.ok = TRUE)
  if (!is.null(lp$eliminated)) return(lp)
  A <- methods::as(lp$A, "CsparseMatrix")
  rid <- seq_len(nrow(A))
  cid <- seq_len(ncol(A))
  obj <- as.numeric(lp$obj)
  clo <- lp$col_lo
  cup <- lp$col_up
  rlo <- lp$row_lo
  rup <- lp$row_up
  vt <- if ("vtype" %in% names(lp$col_index)) lp$col_index[["vtype"]] else NULL
  int <- if (is.null(vt)) rep(FALSE, length(cid)) else
    tolower(as.character(vt)) %in% c("integer", "binary", "int", "bin", "i", "b")
  elim <- list()
  n0r <- nrow(A)
  n0c <- ncol(A)

  for (pass in seq_len(max_pass)) {
    ri <- A@i + 1L
    cj <- rep.int(seq_len(ncol(A)), diff(A@p))
    rc <- tabulate(ri, nrow(A))
    cand <- rc == 2L & rlo == 0 & rup == 0
    if (!any(cand)) break
    s <- which(cand[ri])
    o <- order(ri[s], cj[s])
    s <- s[o]
    one <- s[c(TRUE, FALSE)]
    two <- s[c(FALSE, TRUE)]
    r <- ri[one]
    u <- cj[one]
    v <- cj[two]
    au <- A@x[one]
    av <- A@x[two]
    nnz <- diff(A@p)
    # Which of the two to eliminate: never an integer column; prefer one that
    # no other candidate row would keep (eliminating a column another row
    # substitutes INTO would need two products in one pass); then the one with
    # fewer entries (less fill). Chains resolve over successive passes.
    pref <- function(w, other) {
      (!int[w]) * 4L + (!(w %in% c(u, v)[duplicated(c(u, v))])) * 2L +
        (nnz[w] <= nnz[other])
    }
    pick_u <- pref(u, v) >= pref(v, u)
    x <- ifelse(pick_u, u, v)
    y <- ifelse(pick_u, v, u)
    ax <- ifelse(pick_u, au, av)
    ay <- ifelse(pick_u, av, au)
    k <- !int[x] & ax != 0
    k[k] <- !duplicated(x[k])
    # a kept target must not be eliminated in the same pass
    k <- k & !(x %in% y[k])
    k <- k & !(y %in% x[k])
    if (!any(k)) break
    r <- r[k]; x <- x[k]; y <- y[k]
    cf <- -ay[k] / ax[k]

    # x's bounds, divided by c, bound y (the sign of c swaps them)
    lo_y <- ifelse(cf > 0, clo[x] / cf, cup[x] / cf)
    up_y <- ifelse(cf > 0, cup[x] / cf, clo[x] / cf)
    b <- data.table::data.table(y = y, lo = lo_y, up = up_y)[
      , list(lo = max(lo), up = min(up)), by = "y"]
    clo[b$y] <- pmax(clo[b$y], b$lo)
    cup[b$y] <- pmin(cup[b$y], b$up)

    keepc <- rep(TRUE, ncol(A))
    keepc[x] <- FALSE
    newpos <- cumsum(keepc)
    kc <- which(keepc)
    Tm <- Matrix::sparseMatrix(
      i = c(kc, x), j = c(newpos[kc], newpos[y]),
      x = c(rep(1, length(kc)), cf), dims = c(ncol(A), length(kc)))
    keepr <- rep(TRUE, nrow(A))
    keepr[r] <- FALSE
    A <- Matrix::drop0(A[keepr, , drop = FALSE] %*% Tm, tol = tol)
    obj <- as.numeric(Matrix::crossprod(Tm, obj))
    elim[[pass]] <- data.table::data.table(col = cid[x], ref = cid[y],
                                           coef = cf, pass = pass)
    cid <- cid[keepc]
    clo <- clo[keepc]
    cup <- cup[keepc]
    int <- int[keepc]
    rid <- rid[keepr]
    rlo <- rlo[keepr]
    rup <- rup[keepr]
    if (verbose) {
      message(sprintf("doubletons pass %d: %d columns and rows eliminated",
                      pass, length(x)))
    }
  }

  # rows the substitutions emptied: a zero right-hand side is satisfied and
  # dropped; anything else is an infeasibility the solver must report
  emp <- tabulate(A@i + 1L, nrow(A)) == 0L
  drop <- emp & rlo <= 0 & rup >= 0
  if (any(drop)) {
    A <- A[!drop, , drop = FALSE]
    rid <- rid[!drop]
    rlo <- rlo[!drop]
    rup <- rup[!drop]
  }
  bad <- which(clo > cup + 1e-9 * pmax(1, abs(clo)))
  if (length(bad)) {
    warning("lp_presolve(): ", length(bad), " column(s) have ",
            "crossing bounds after substitution; the LP is infeasible.",
            call. = FALSE)
  }

  ci <- data.table::copy(data.table::as.data.table(lp$col_index))
  data.table::set(ci, j = "jred", value = match(seq_len(n0c), cid))
  data.table::setattr(ci, "dim_names", attr(lp$col_index, "dim_names"))
  ri2 <- data.table::copy(data.table::as.data.table(lp$row_index))
  data.table::set(ri2, j = "ired", value = match(seq_len(n0r), rid))
  el <- data.table::rbindlist(elim)
  if (!nrow(el)) {
    el <- data.table::data.table(col = integer(), ref = integer(),
                                 coef = numeric(), pass = integer())
  }
  if (verbose) {
    message(sprintf("doubletons: %d -> %d columns, %d -> %d rows, %d nonzeros",
                    n0c, ncol(A), n0r, nrow(A), length(A@x)))
  }
  lp$A <- A
  lp$obj <- obj
  lp$col_lo <- clo
  lp$col_up <- cup
  lp$row_lo <- rlo
  lp$row_up <- rup
  lp$col_index <- ci
  lp$row_index <- ri2
  lp$eliminated <- el
  lp
}

#' The doubleton substitution from a scan of the rows
#'
#' The same reduction as [lp_presolve()], composed from the scan pass of a
#' streamed build instead of the assembled matrix. The scan keeps the entries
#' of every row with few entries; after each pass those rows are recomputed
#' with the substitution applied (entries moved to the reference column,
#' merged, exact zeros dropped), so a row that collapses to two entries
#' becomes a candidate exactly as it does on the matrix. The candidate rows
#' are those with exactly two entries, an equality and a zero right-hand
#' side; passes choose the column to eliminate by the same preference, keep a
#' target out of the same pass, tighten the target's bounds from the
#' eliminated column's, and resolve chains one link per pass. What the
#' matrix form sees and this one does not: a row with more entries than the
#' scan tracks that collapses to two, and merges in such rows when the column
#' entry counts are updated, so a tie in the preference may fall the other
#' way there.
#'
#' @param scan The result of `mm_scan_close()`.
#' @param sense,rhs Row senses and right-hand sides, full index.
#' @param col_lo,col_up,vtype Column bounds and types, full index.
#' @return A list: `eliminated` (col, ref, coef, pass), `jred`, `ired`
#'   (0 for an eliminated column or row), `ref` and `coef` (per full-index
#'   column, the kept column and factor an eliminated column resolves to;
#'   0 and 1 for a kept column), tightened `col_lo` / `col_up`.
#' @keywords internal
#' @noRd
.mx_presolve_map <- function(scan, sense, rhs, col_lo, col_up, vtype = NULL,
                             max_pass = 50L, tol = 1e-12, verbose = FALSE, full = FALSE) {
  ncol <- length(col_lo)
  int <- .mx_int_cols(vtype, ncol)
  cand <- sense == "==" & rhs == 0
  m <- mm_presolve_map(scan, cand, int, as.numeric(col_lo), as.numeric(col_up),
                       as.integer(max_pass), as.numeric(tol), .mx_threads(), isTRUE(verbose))
  if (m$crossing > 0) {
    warning("presolve: ", m$crossing, " column(s) have crossing bounds after ",
            "substitution; the LP is infeasible.", call. = FALSE)
  }
  out <- list(map = m$map,
              eliminated = data.table::data.table(col = m$col, ref = m$ref, coef = m$coef, pass = m$pass),
              passes = m$passes)
  # the map's full-index vectors are left in the core; `full = TRUE` brings
  # them over for a comparison
  if (isTRUE(full)) out <- c(out, mm_map_vectors(m$map))
  out
}

#' Which columns are integer, from the index table's `vtype`
#' @keywords internal
#' @noRd
.mx_int_cols <- function(vtype, ncol) {
  if (is.null(vtype)) return(rep(FALSE, ncol))
  # a handful of distinct values over millions of columns
  u <- unique(vtype)
  (tolower(as.character(u)) %in% c("integer", "binary", "int", "bin", "i", "b"))[match(vtype, u)]
}

#' The substitution composed in R, from a closed scan
#'
#' The reference implementation of the rules the native `mm_presolve_map()`
#' follows, kept for the tests that compare the two. `scan` is the list
#' `mm_scan_close()` returns.
#' @keywords internal
#' @noRd
.mx_presolve_map_r <- function(scan, sense, rhs, col_lo, col_up, vtype = NULL,
                               max_pass = 50L, tol = 1e-12, verbose = FALSE) {
  ncol <- length(col_lo)
  nrow <- length(sense)
  int <- .mx_int_cols(vtype, ncol)
  nnz <- as.numeric(scan$col_counts)
  clo <- col_lo; cup <- col_up
  # the tracked rows' entries; only equality rows with a zero right-hand side
  # can ever be candidates, the others are not needed
  T <- data.table::data.table(r = scan$row, c = scan$col, x = scan$x)
  T <- T[sense[r] == "==" & rhs[r] == 0]
  elim <- list()
  dropped_row <- rep(FALSE, nrow)
  elim_col <- rep(FALSE, ncol)
  # where an eliminated column currently resolves to, and with which factor
  ref <- seq_len(ncol); coef <- rep(1, ncol)

  for (pass in seq_len(max_pass)) {
    if (!nrow(T)) break
    # candidates: tracked rows with exactly two entries, columns ascending
    cnt <- T[, list(n = .N), by = "r"]
    two <- cnt$r[cnt$n == 2L]
    if (!length(two)) break
    d <- T[r %in% two][order(r, c)]
    first <- seq(1L, nrow(d), by = 2L)
    r <- d$r[first]; u <- d$c[first]; au <- d$x[first]; v <- d$c[first + 1L]; av <- d$x[first + 1L]
    pref <- function(w, other) {
      (!int[w]) * 4L + (!(w %in% c(u, v)[duplicated(c(u, v))])) * 2L +
        (nnz[w] <= nnz[other])
    }
    pick_u <- pref(u, v) >= pref(v, u)
    x <- ifelse(pick_u, u, v)
    y <- ifelse(pick_u, v, u)
    ax <- ifelse(pick_u, au, av)
    ay <- ifelse(pick_u, av, au)
    k <- !int[x] & ax != 0
    k[k] <- !duplicated(x[k])
    k <- k & !(x %in% y[k])
    k <- k & !(y %in% x[k])
    if (!any(k)) break
    rk <- r[k]; xk <- x[k]; yk <- y[k]
    cf <- -ay[k] / ax[k]

    lo_y <- ifelse(cf > 0, clo[xk] / cf, cup[xk] / cf)
    up_y <- ifelse(cf > 0, cup[xk] / cf, clo[xk] / cf)
    b <- data.table::data.table(y = yk, lo = lo_y, up = up_y)[
      , list(lo = max(lo), up = min(up)), by = "y"]
    clo[b$y] <- pmax(clo[b$y], b$lo)
    cup[b$y] <- pmin(cup[b$y], b$up)

    elim[[pass]] <- data.table::data.table(col = xk, ref = yk, coef = cf, pass = pass)
    elim_col[xk] <- TRUE
    dropped_row[rk] <- TRUE
    ref[xk] <- yk; coef[xk] <- cf
    # the eliminated column's other entries move to the target, and the
    # target loses its own entry in the dropped row
    moved <- data.table::data.table(y = yk, n = nnz[xk] - 2)[, list(n = sum(n)), by = "y"]
    nnz[moved$y] <- nnz[moved$y] + moved$n
    nnz[xk] <- 0
    if (verbose) {
      message(sprintf("doubletons pass %d: %d columns and rows eliminated", pass, length(xk)))
    }
    # the tracked rows after this pass: eliminated rows gone, eliminated
    # columns moved to their reference, entries on one column merged, exact
    # zeros dropped (the merges reduce the column counts)
    T <- T[!(r %in% rk)]
    if (!nrow(T)) break
    this <- logical(ncol); this[xk] <- TRUE
    e <- this[T$c]
    if (any(e)) {
      data.table::set(T, i = which(e), j = "x", value = T$x[e] * coef[T$c[e]])
      data.table::set(T, i = which(e), j = "c", value = ref[T$c[e]])
      before <- T[, list(n = .N), by = "c"]
      T <- T[, list(x = sum(x)), by = c("r", "c")]
      T <- T[abs(x) > tol]
      after <- T[, list(n = .N), by = "c"]
      # entries merged or cancelled in the tracked rows leave the column counts
      left <- after$n[match(before$c, after$c)]
      gone <- before$n - ifelse(is.na(left), 0L, left)
      if (any(gone > 0L)) {
        nnz[before$c] <- nnz[before$c] - gone
        if (verbose) message(sprintf("  %d entries merged or cancelled in the tracked rows", sum(gone)))
      }
    }
  }

  el <- data.table::rbindlist(elim)
  if (!nrow(el)) {
    el <- data.table::data.table(col = integer(), ref = integer(), coef = numeric(), pass = integer())
  }
  # resolve every eliminated column to a kept one (chains over passes)
  final_ref <- seq_len(ncol); final_coef <- rep(1, ncol)
  if (nrow(el)) {
    for (p in sort(unique(el$pass), decreasing = TRUE)) {
      e <- el[el$pass == p]
      final_ref[e$col] <- final_ref[e$ref]
      final_coef[e$col] <- e$coef * final_coef[e$ref]
    }
  }
  bad <- which(clo > cup + 1e-9 * pmax(1, abs(clo)))
  if (length(bad)) {
    warning("presolve: ", length(bad), " column(s) have crossing bounds after ",
            "substitution; the LP is infeasible.", call. = FALSE)
  }
  jred <- integer(ncol); jred[!elim_col] <- seq_len(sum(!elim_col))
  ired <- integer(nrow); ired[!dropped_row] <- seq_len(sum(!dropped_row))
  list(eliminated = el, jred = jred, ired = ired,
       ref = ifelse(elim_col, final_ref, 0L), coef = ifelse(elim_col, final_coef, 1),
       col_lo = clo, col_up = cup, nnz = nnz)
}

#' Expand a reduced-LP column vector to the full index
#'
#' @param x Values of the kept columns, in reduced order.
#' @param jred Position of each full-index column in the reduced LP (`NA` when
#'   eliminated).
#' @param eliminated The table from [lp_presolve()].
#' @param fill_eliminated Logical; `TRUE` rebuilds eliminated columns from
#'   their references (primal values), `FALSE` leaves them `NA` (reduced costs).
#' @keywords internal
#' @noRd
.expand_eliminated <- function(x, jred, eliminated, fill_eliminated = TRUE) {
  out <- rep(NA_real_, length(jred))
  k <- !is.na(jred)
  out[k] <- x[jred[k]]
  if (fill_eliminated && !is.null(eliminated) && nrow(eliminated)) {
    for (p in sort(unique(eliminated$pass), decreasing = TRUE)) {
      e <- eliminated[eliminated$pass == p]
      out[e$col] <- e$coef * out[e$ref]
    }
  }
  out
}
