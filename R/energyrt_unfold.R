# =============================================================================
# Wildcard (folded) parameter data
# =============================================================================
#
# `interp_mod(fold = TRUE)` collapses a parameter's region / timeslice /
# vintage (and, opt-in, year / comm / tech / stg / trade) column to a single
# `NA` row wherever the value does not vary across that dimension. NA there
# means "every member", not "no member".
#
# multimod joins parameter data on its index columns, and an NA key matches
# nothing - so a folded row is silently skipped and the tuple falls back to the
# parameter's default. Nothing errors. Measured on the UTOPIA R7 kit, the
# folded twin of a scenario built to the same LP shape (18,163 x 18,298) with
# 42,010 nonzeros instead of 43,612, 4,928 differing coefficients, and solved
# to an objective of 0 instead of 46,398.42.
#
# energyRt's own solver route avoids this by rewriting the generated model code
# to index an artificial ANY* set member (fold_artificial.R). There is no model
# code to rewrite on the matrix route, so multimod materialises the wildcards
# instead: "folded scenarios UNFOLDED at import".
#
# IMPORTANT - two entry points, only one of them safe here:
#   unfold_scenario_parameter()   singular, pure, returns a data.frame
#   unfold_scenario_parameters()  plural, WRITES BACK: for an on-disk parameter
#                                 it unlink()s the data directory and rewrites
#                                 it (fold.R `.fold_write_back`), permanently
#                                 unfolding the user's stored scenario.
# Only the singular one is used.
# =============================================================================

#' Index columns of a parameter's data table
#' @keywords internal
#' @noRd
.index_cols <- function(d) setdiff(names(d), "value")

#' Does this table carry a wildcard in an index column?
#'
#' `NA` is the wildcard energyRt writes when folding. (`ANY*` tokens are a
#' property of the *written* model files, not of `modInp`, so they are not
#' expected here.)
#' @keywords internal
#' @noRd
.has_wildcard <- function(d) {
  if (is.null(d) || !is.data.frame(d) || !nrow(d)) return(FALSE)
  cols <- .index_cols(d)
  if (!length(cols)) return(FALSE)
  for (k in cols) if (anyNA(d[[k]])) return(TRUE)
  FALSE
}

#' Is this scenario folded?
#'
#' True when any parameter records a fold. Note this is necessary but not
#' sufficient for detecting wildcards: on the R7 kit `fold = TRUE` leaves
#' wildcards in 22 parameters while only 11 carry `fold_info`, because
#' `fold = FALSE` also densifies wildcards that came from the source data and
#' `fold = TRUE` skips that step. Use it to decide *whether to look*, never as
#' the per-parameter test.
#' @keywords internal
#' @noRd
.scenario_is_folded <- function(scenario) {
  params <- scenario@modInp@parameters
  for (p in params) {
    if (!isS4(p) || !methods::is(p, "parameter")) next
    fi <- p@misc$fold_info
    if (!is.null(fi) && isTRUE(fi$folded)) return(TRUE)
  }
  FALSE
}

#' Materialise a folded parameter's wildcard rows
#'
#' Delegates to energyRt's read-time helper, which builds the per-entity
#' membership maps this parameter needs and expands each wildcard row to one
#' row per member. Returns `data` unchanged when there is nothing to expand or
#' when the helper is unavailable.
#'
#' @param data The parameter's data, as loaded.
#' @param ert_param The energyRt parameter object.
#' @param scenario The energyRt scenario (needed for the membership maps).
#' @keywords internal
#' @noRd
.unfold_param_data <- function(data, ert_param, scenario) {
  if (is.null(scenario) || !.has_wildcard(data)) return(data)
  unfold1 <- tryCatch(
    utils::getFromNamespace("unfold_scenario_parameter", "energyRt"),
    error = function(e) NULL
  )
  if (is.null(unfold1)) {
    stop("Parameter '", ert_param@name, "' carries wildcard (NA) index values, ",
         "which means the scenario was interpolated with fold = TRUE, but ",
         "energyRt::unfold_scenario_parameter() is not available to expand ",
         "them.\n  Joining on an NA key silently drops the row and substitutes ",
         "the default, so continuing would produce a wrong model rather than ",
         "an error.\n  Re-interpolate with fold = FALSE, or install an ",
         "energyRt that provides the helper.", call. = FALSE)
  }
  out <- tryCatch(unfold1(scenario, ert_param), error = function(e) {
    stop("Could not unfold folded parameter '", ert_param@name, "': ",
         conditionMessage(e), call. = FALSE)
  })
  out <- as.data.frame(out)
  if (.has_wildcard(out)) {
    left <- .index_cols(out)[vapply(.index_cols(out),
                                    function(k) anyNA(out[[k]]), logical(1))]
    warning("Parameter '", ert_param@name, "' still carries wildcard values in ",
            paste(left, collapse = ", "), " after unfolding.", call. = FALSE)
  }
  out
}
