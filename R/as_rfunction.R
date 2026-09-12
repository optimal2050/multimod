#' Convert AST to R expression
#'
#' @param x An AST node
#' @return A character string representing an R expression
#' @noRd
as_rexpression <- function(x, ...) {
  UseMethod("as_rexpression")
}

#' @exportS3Method
#' @noRd
as_rexpression.constant <- function(x, ...) {
  format(x$value, scientific = FALSE)
}

#' @exportS3Method
#' @noRd
as_rexpression.symbol <- function(x, ...) {
  x$name
}

#' @exportS3Method
#' @noRd
as_rexpression.variable <- function(x, ...) {
  paste0(x$name)
}

#' @exportS3Method
#' @noRd
as_rexpression.parameter <- function(x, ...) {
  paste0(x$name)
}

#' @exportS3Method
#' @noRd
as_rexpression.expression <- function(x, ...) {
  lhs <- as_rexpression(x$lhs, ...)
  rhs <- as_rexpression(x$rhs, ...)
  op <- x$op

  # Group if needed
  if (!is.null(x$brackets) && x$brackets) {
    paste0("(", lhs, " ", op, " ", rhs, ")")
  } else {
    paste(lhs, op, rhs)
  }
}

#' @exportS3Method
#' @noRd
as_rexpression.sum <- function(x, ...) {
  index_expr <- as_rexpression(x$index, ...)
  value_expr <- as_rexpression(x$value, ...)
  paste0("df_sum(", index_expr, ", ", value_expr, ")")
}

#' @exportS3Method
#' @noRd
as_rexpression.prod <- function(x, ...) {
  index_expr <- as_rexpression(x$index, ...)
  value_expr <- as_rexpression(x$value, ...)
  paste0("df_prod(", index_expr, ", ", value_expr, ")")
}

#' @exportS3Method
#' @noRd
as_rexpression.when <- function(x, ...) {
  cond <- as_rexpression(x$condition, ...)
  then <- as_rexpression(x$then, ...)
  paste0("df_filter(", then, ", ", cond, ")")
}

#' @exportS3Method
#' @noRd
as_rexpression.dims <- function(x, ...) {
  paste0(vapply(x, function(s) s$name, character(1)), collapse = ",")
}

# Implementation using dplyr/dtplyr

#' Sum over a grouped data frame
#' @noRd
df_sum <- function(data, index) {
  data %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(!!!rlang::syms(index)) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

#' Product over a grouped data frame
#' @noRd
df_prod <- function(data, index) {
  data %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(!!!rlang::syms(index)) %>%
    dplyr::summarise(value = prod(value, na.rm = TRUE), .groups = "drop")
}

#' Filter a data frame by condition
#' @noRd
df_filter <- function(data, condition) {
  data %>%
    dtplyr::lazy_dt() %>%
    dplyr::filter(!!rlang::parse_expr(condition))
}


