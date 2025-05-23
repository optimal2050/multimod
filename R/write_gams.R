#' Write a full GAMS model file from a multimod object
#'
#' @param model A `multimod` model object
#' @param file Output file path (optional)
#' @param format_expr logical; whether to format expressions with line breaks
#' @param ... Additional arguments passed to formatting functions
#'
#' @return Character vector or writes file if `file` is given
write_gams.multimod <- function(model, file = NULL, format_expr = TRUE, ...) {
  stopifnot(inherits(model, "model"))

  lines <- character()

  # Header
  if (!is.null(model$name)) {
    lines <- c(lines, paste0("* Model: ", model$name))
  }
  if (!is.null(model$desc)) {
    lines <- c(lines, paste0("* ", model$desc))
  }
  lines <- c(lines, "")

  # Sets
  for (s in model$sets) {
    lines <- c(lines, as_gams(s))
  }

  # Aliases
  if (!is.null(model$aliases) && length(model$aliases) > 0) {
    for (alias in model$aliases) {
      if (length(alias) >= 2) {
        lines <- c(lines, paste0("alias (", paste(alias, collapse = ", "), ");"))
      }
    }
  }

  lines <- c(lines, "")

  # Mappings
  for (m in model$mappings) {
    lines <- c(lines, as_gams(m))
  }
  lines <- c(lines, "")

  # Parameters
  for (p in model$parameters) {
    lines <- c(lines, as_gams(p))
  }
  lines <- c(lines, "")

  # Variables
  for (v in model$variables) {
    lines <- c(lines, as_gams(v))
  }
  lines <- c(lines, "")

  # Equations (declarations)
  for (eq in model$equations) {
    lines <- c(lines, paste0("equation ", eq$name, ";"))
  }
  lines <- c(lines, "")

  # Equations (definitions)
  for (eq in model$equations) {
    gams_eq <- as_gams(eq)
    if (format_expr) {
      gams_eq <- format_gams_expression(gams_eq, ...)
    }
    lines <- c(lines, gams_eq, "")
  }

  # Output
  if (!is.null(file)) {
    writeLines(lines, con = file, useBytes = TRUE)
  } else {
    return(lines)
  }
}
