# =============================================================================
# Implied bounds of an LP: the interval each variable can move in
# =============================================================================
#
# Every row bounds each of its variables once the others are bounded: with
# l_i <= sum_j a_ij x_j <= u_i and l_j <= x_j <= u_j, the activity of the
# row without x_k lies between L_i - a_ik l_k and U_i - a_ik u_k (for
# a_ik > 0), so x_k <= (u_i - L_i + a_ik l_k) / a_ik and the mirror for the
# lower bound. Passing over the rows until nothing moves gives, for each
# variable, an interval that contains the projection of the feasible set on
# that coordinate: the support of the variable. Infinite contributions are
# counted, not summed, so a row whose only unbounded variable is x_k still
# bounds x_k. A feasible objective value adds the objective as one more row.

#' Implied bounds of every variable
#'
#' Propagates the row and column bounds of an LP to a fixed point and
#' returns the LP with the implied interval of each column, `lo_impl` and
#' `up_impl`, on its column index. The interval contains every feasible value
#' of the variable; it is tight only where a single row decides it.
#'
#' @param lp An LP from [model_to_lp()] or [lp_presolve()].
#' @param objective A known feasible objective value, or `NULL`. When given,
#'   the objective row `sum c_j x_j <= objective` (`>=` for a maximisation)
#'   takes part, which bounds variables that rows alone leave open.
#' @param max_pass Passes over the rows; propagation stops earlier when no
#'   bound moves by more than `tol`.
#' @param tol Minimal improvement of a bound that counts as a change.
#' @param verbose Report each pass.
#' @return The LP with `lo_impl`, `up_impl` on `col_index` and a `bounds`
#'   element: `passes`, `tightened` (columns whose interval shrank), `fixed`
#'   (zero-width intervals), `open` (an infinite end left) and
#'   `infeasible` (a row whose activity range misses its bounds, by index).
#' @export
#' @examples
#' lp <- model_to_lp(example_models$toy)
#' b <- lp_bounds(lp)
#' b$bounds[c("tightened", "fixed", "open")]
#' head(b$col_index[, c("symbol", "lo", "up", "lo_impl", "up_impl")])
lp_bounds <- function(lp, objective = NULL, max_pass = 50L, tol = 1e-9, verbose = FALSE) {
  stopifnot(!is.null(lp$A), !is.null(lp$col_lo), !is.null(lp$col_up))
  A <- methods::as(lp$A, "TsparseMatrix")
  e <- data.table::data.table(i = A@i + 1L, j = A@j + 1L, a = A@x)
  e <- e[a != 0]
  rlo <- as.numeric(lp$row_lo)
  rup <- as.numeric(lp$row_up)
  if (!is.null(objective)) {
    obj <- as.numeric(lp$obj)
    nz <- which(obj != 0)
    r <- length(rlo) + 1L
    e <- data.table::rbindlist(list(e, data.table::data.table(i = r, j = nz, a = obj[nz])))
    rlo <- c(rlo, if (isTRUE(lp$maximum)) objective else -Inf)
    rup <- c(rup, if (isTRUE(lp$maximum)) Inf else objective)
  }
  lo <- as.numeric(lp$col_lo)
  up <- as.numeric(lp$col_up)
  lo0 <- lo
  up0 <- up
  n <- length(lo)
  data.table::setkey(e, i)
  infeasible <- integer()
  passes <- 0L
  repeat {
    passes <- passes + 1L
    # the bound each entry brings to its row's minimal and maximal activity
    e[, `:=`(blo = ifelse(a > 0, lo[j], up[j]), bup = ifelse(a > 0, up[j], lo[j]))]
    e[, `:=`(clo = a * blo, cup = a * bup)]
    # per row: the finite part of the activity range and the number of
    # infinite contributions on each side
    rows <- e[, list(Lf = sum(clo[is.finite(clo)]), Ln = sum(!is.finite(clo)),
                     Uf = sum(cup[is.finite(cup)]), Un = sum(!is.finite(cup))), by = i]
    # a row whose whole activity range misses its bounds
    Lmin <- ifelse(rows$Ln > 0, -Inf, rows$Lf)
    Umax <- ifelse(rows$Un > 0, Inf, rows$Uf)
    bad <- rows$i[Lmin > rup[rows$i] + tol | Umax < rlo[rows$i] - tol]
    if (length(bad)) { infeasible <- unique(c(infeasible, bad)); }
    e[rows, `:=`(Lf = i.Lf, Ln = i.Ln, Uf = i.Uf, Un = i.Un), on = "i"]
    # the activity of the row without this entry: finite when the entry
    # carries the row's only infinite contribution, else when there is none
    e[, `:=`(Lrest = ifelse(is.finite(clo), ifelse(Ln == 0L, Lf - clo, NA_real_),
                            ifelse(Ln == 1L, Lf, NA_real_)),
             Urest = ifelse(is.finite(cup), ifelse(Un == 0L, Uf - cup, NA_real_),
                            ifelse(Un == 1L, Uf, NA_real_)))]
    # the entry's implied bounds on its variable
    e[, `:=`(nup = ifelse(a > 0, (rup[i] - Lrest) / a, (rlo[i] - Urest) / a),
             nlo = ifelse(a > 0, (rlo[i] - Urest) / a, (rup[i] - Lrest) / a))]
    cb <- e[, list(nlo = suppressWarnings(max(nlo, na.rm = TRUE)),
                   nup = suppressWarnings(min(nup, na.rm = TRUE))), by = j]
    nlo <- lo; nup <- up
    nlo[cb$j] <- pmax(lo[cb$j], cb$nlo)
    nup[cb$j] <- pmin(up[cb$j], cb$nup)
    # an implied interval cannot cross; a crossing is an infeasibility of
    # rounding size at worst, kept at the declared bound
    cross <- nlo > nup
    if (any(cross)) { nlo[cross] <- pmin(nlo[cross], up[cross]); nup[cross] <- pmax(nup[cross], lo[cross]) }
    changed <- function(new, old) {
      scale <- pmax(1, abs(ifelse(is.finite(old), old, 0)))
      is.finite(new) != is.finite(old) | (is.finite(old) & abs(new - old) > tol * scale)
    }
    moved <- sum(changed(nlo, lo), na.rm = TRUE) + sum(changed(nup, up), na.rm = TRUE)
    lo <- nlo; up <- nup
    if (verbose) message(sprintf("bounds pass %d: %d bound(s) moved", passes, moved))
    if (moved == 0L || passes >= max_pass) break
  }
  ci <- data.table::copy(data.table::as.data.table(lp$col_index))
  # a reduced LP keeps the full column index: the eliminated columns have no
  # interval of their own (their value follows their reference)
  if (nrow(ci) != n && "jred" %in% names(ci)) {
    keep <- which(!is.na(ci$jred))
    keep <- keep[order(ci$jred[keep])]
    full <- function(v) { out <- rep(NA_real_, nrow(ci)); out[keep] <- v; out }
    lo_ci <- full(lo); up_ci <- full(up)
  } else {
    lo_ci <- lo; up_ci <- up
  }
  data.table::set(ci, j = "lo_impl", value = lo_ci)
  data.table::set(ci, j = "up_impl", value = up_ci)
  lp$col_index <- ci
  lp$bounds <- list(
    passes = passes,
    tightened = sum(lo > lo0 | up < up0),
    fixed = sum(is.finite(lo) & is.finite(up) & up - lo <= tol * pmax(1, abs(lo))),
    open = sum(!is.finite(lo) | !is.finite(up)),
    infeasible = infeasible,
    objective = objective)
  lp
}

utils::globalVariables(c("a", "blo", "bup", "clo", "cup", "Lf", "Ln", "Uf", "Un",
                         "i.Lf", "i.Ln", "i.Uf", "i.Un", "Lrest", "Urest", "nlo", "nup"))
