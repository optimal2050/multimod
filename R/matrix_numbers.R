# =============================================================================
# Numerical health of an assembled LP
# =============================================================================
#
# The counterpart of `dev/check_mps_numbers.R`, which streams a written .mps.
# That one exists for Pyomo output, where names are symbolic; run against our
# own file it reports nothing useful, because `write_mps()` emits positional
# names (`c0`, `r0`) and every entry collapses into one "family".
#
# Here the file is not needed at all. `model_to_lp()` already holds the
# coefficients next to `row_index` / `col_index`, so the same report can be
# produced with real symbol families, before anything is written, and without
# re-parsing gigabytes.
#
# Why it matters: first-order solvers (PDLP, and cuOpt's PSLP) judge tolerances
# against global problem norms, so entries many orders below the typical
# magnitude are either trampled or poison convergence. Presolve normally
# scrubs them; a truncated presolve does not. Measured: `pWeather` dust at
# 5e-11 diverged a 214M-nonzero solve, and an `eqTechAInp` /8760 unit
# conversion put 1.2M entries at 5e-6..5e-5 into a full-year matrix. HiGHS
# silently dropped 153,504 entries around 1e-13 from a PyPSA-Eur matrix.
#
# The offender tables are the actionable part: not "there is a 5e-11
# somewhere" but "841k entries below 1e-2, all in eqTechAfUp rows against
# vTechCap columns", which is the level at which clipping, rescaling or
# reformulating can be chosen.
# =============================================================================

#' Report the magnitude spectrum of an assembled LP
#'
#' @param lp An LP from [model_to_lp()].
#' @param small,large Thresholds for the "too small" / "too large" counts.
#' @param top How many offender families to list.
#' @param verbose Logical; print the report.
#'
#' @return Invisibly, a list with `summary` (one row per section), `decades`
#'   (a magnitude histogram of the matrix coefficients), and `offenders_row` /
#'   `offenders_col` (entries below `small`, by equation and by variable).
#' @export
check_matrix_numbers <- function(lp, small = 1e-4, large = 1e6, top = 15,
                                 verbose = TRUE) {
  stopifnot(is.list(lp), !is.null(lp$A))

  spectrum <- function(v, section, count_nonfinite = TRUE) {
    n_all <- length(v)
    # NaN and (for coefficients) Inf are reported, never quietly filtered: a
    # solver rejects the matrix outright for them, and a summary that drops
    # them shows a reassuring maximum while the model cannot be solved at all.
    bad <- if (count_nonfinite) sum(!is.finite(v)) else 0L
    v <- abs(v[is.finite(v)])
    nz <- v[v > 0]
    data.frame(
      section = section,
      entries = n_all,
      zeros = sum(v == 0),
      nonfinite = bad,
      min_abs = if (length(nz)) min(nz) else NA_real_,
      max_abs = if (length(nz)) max(nz) else NA_real_,
      orders = if (length(nz)) round(log10(max(nz) / min(nz)), 1) else NA_real_,
      below_small = sum(nz < small),
      above_large = sum(nz > large),
      stringsAsFactors = FALSE
    )
  }

  ax <- lp$A@x
  # Row bounds are the model's rhs; -Inf/Inf mark the open side of an
  # inequality and are not numbers the solver has to scale.
  rhs <- c(lp$row_lo, lp$row_up)
  bnd <- c(lp$col_lo, lp$col_up)

  summ <- rbind(
    spectrum(ax, "COLUMNS"),
    spectrum(lp$obj[lp$obj != 0], "OBJECTIVE"),
    # An infinite row/column bound is the open side of an inequality, not a bad
    # number, so those two sections are filtered before counting.
    spectrum(rhs[is.finite(rhs)], "RHS", count_nonfinite = FALSE),
    spectrum(bnd[is.finite(bnd) & bnd != 0], "BOUNDS", count_nonfinite = FALSE)
  )

  nz <- abs(ax[ax != 0])
  decades <- if (length(nz)) table(floor(log10(nz))) else table(integer())

  # Only the offending entries need their row/column resolved. Materialising a
  # column index for every nonzero would cost as much memory again as the
  # matrix; findInterval() over the column pointers costs nothing.
  tally <- function(sym, label) {
    t <- sort(table(sym), decreasing = TRUE)
    out <- data.frame(family = names(t), n = as.integer(t),
                      stringsAsFactors = FALSE)
    names(out)[2] <- label
    out[seq_len(min(nrow(out), top)), , drop = FALSE]
  }
  attribute <- function(k, label) {
    if (!length(k)) return(list(row = data.frame(), col = data.frame()))
    i <- lp$A@i[k] + 1L
    j <- findInterval(k - 1L, lp$A@p)
    list(row = tally(lp$row_index$symbol[i], label),
         col = tally(lp$col_index$symbol[j], label))
  }

  sm <- attribute(which(abs(ax) > 0 & abs(ax) < small), "n_small")
  offenders_row <- sm$row
  offenders_col <- sm$col
  nf <- attribute(which(!is.finite(ax)), "n_nonfinite")

  # Structural check. A column with no entries is a variable that appears in no
  # constraint: if it also carries an objective coefficient, minimisation just
  # drives it to its bound and the model is silently wrong rather than
  # infeasible. That is what an undefined `vTotalUserCosts` looks like.
  n_per_col <- diff(lp$A@p)
  empty_cols <- which(n_per_col == 0L)
  empty_rows <- setdiff(seq_len(nrow(lp$A)), unique(lp$A@i + 1L))
  undefined <- intersect(empty_cols, which(lp$obj != 0))
  structure_tbl <- data.frame(
    empty_cols = length(empty_cols),
    empty_rows = length(empty_rows),
    empty_cols_in_objective = length(undefined),
    stringsAsFactors = FALSE
  )

  res <- list(summary = summ, decades = decades,
              offenders_row = offenders_row, offenders_col = offenders_col,
              nonfinite_row = nf$row, nonfinite_col = nf$col,
              structure = structure_tbl,
              empty_col_symbols = unique(lp$col_index$symbol[empty_cols]),
              empty_row_symbols = unique(lp$row_index$symbol[empty_rows]),
              small = small, large = large)

  if (verbose) {
    print(summ, row.names = FALSE)
    n_bad <- summ$nonfinite[summ$section == "COLUMNS"]
    if (n_bad > 0) {
      # A solver refuses the matrix for these, so they come first: nothing else
      # in this report matters until they are gone.
      cat(sprintf("\nERROR-LEVEL: %d non-finite coefficient(s) (Inf/NaN).\n",
                  n_bad))
      cat("  by EQUATION:\n"); print(nf$row, row.names = FALSE)
      cat("  by VARIABLE:\n"); print(nf$col, row.names = FALSE)
      cat("  Usually a division by a parameter whose default is 0.\n")
    }
    if (nrow(offenders_row)) {
      cat(sprintf("\nCoefficients < %g by EQUATION:\n", small))
      print(offenders_row, row.names = FALSE)
      cat(sprintf("\nCoefficients < %g by VARIABLE:\n", small))
      print(offenders_col, row.names = FALSE)
    }
    if (length(res$empty_col_symbols) || length(res$empty_row_symbols)) {
      cat("\nStructure:", structure_tbl$empty_cols, "empty column(s),",
          structure_tbl$empty_rows, "empty row(s)\n")
      if (length(res$empty_col_symbols)) {
        cat("  variables with no entries:",
            paste(utils::head(res$empty_col_symbols, top), collapse = ", "), "\n")
      }
      if (length(res$empty_row_symbols)) {
        cat("  equations with no entries:",
            paste(utils::head(res$empty_row_symbols, top), collapse = ", "), "\n")
      }
    }
    if (structure_tbl$empty_cols_in_objective > 0) {
      cat(sprintf(paste0(
        "\nWARNING: %d column(s) carry an objective coefficient but appear in\n",
        "  no constraint. The solve will push them to a bound and report a\n",
        "  plausible wrong objective rather than failing.\n"),
        structure_tbl$empty_cols_in_objective))
    }

    o <- summ$orders[summ$section == "COLUMNS"]
    if (!is.na(o) && o > 8) {
      cat(sprintf(paste0(
        "\nWARNING: matrix coefficients span %.1f orders of magnitude.\n",
        "  First-order solvers (PDLP) degrade beyond ~8; consider clipping or\n",
        "  rescaling the families above.\n"), o))
    }
  }
  invisible(res)
}
