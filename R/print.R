#' ---
#' Title: Print Methods for AST and Multimod Classes
#' Description: S3 print methods to display human-readable summaries of multimod and AST objects.
#' Purpose: Improve interpretability and debugging of parsed modeling components.
#' Dependencies: Assumes multimod_ast and multimod_symbol classes are defined.
#' ---

# This file defines S3 print methods for:
# - Abstract syntax tree (AST) nodes: ast_expr, ast_var, ast_param, ast_sum, ast_cond, etc.
# - Multimod domain-specific objects: multimod_equation, multimod_model, etc.
#
# Guidelines:
# * Keep output concise and aligned with class structure
# * Prefer single-line summaries, unless nested expressions require indented display
# * Use format() helpers internally if logic is shared with other rendering functions
# * Avoid side-effects — these functions are for display only
#
# Example:
# > print(ast_variable("x", dims = c("region", "year")))
# Variable: x(region, year)

# S3 generic dispatch for print(x) where class(x) is e.g. ast_expr, multimod_equation, etc.

#' @title Print methods for AST classes

#' @export
print.multimod_ast <- function(x, ...) {
  cat("<multimod_ast>", "\n")
  print(as.character(x), ...)
}

#' @export
print.ast_set <- function(x, ...) {
  cat("<AST set> ", x$name, "\n", sep = "")
}

#' @export
print.ast_expression <- function(x, ...) {
  cat("<AST expression> ", x$op, "\n", sep = "")
  cat("  left :\n"); print(x$left)
  cat("  right:\n"); print(x$right)
}

#' @export
print.ast_unary <- function(x, ...) {
  cat("<AST unary>\n")
  cat("  Operator: ", x$op, "\n", sep = "")
  cat("  Argument:\n")
  print(x$rhs, ...)
  invisible(x)
}


#' @export
print.ast_variable <- function(x, ...) {
  cat("<AST variable> ", x$name, "\n", sep = "")
  if (length(x$dims) > 0) cat("  dims: ", paste(x$dims, collapse = ", "), "\n")
}

#' @export
print.ast_parameter <- function(x, ...) {
  cat("<AST parameter> ", x$name, "\n", sep = "")
  if (length(x$dims) > 0) cat("  dims: ", paste(x$dims, collapse = ", "), "\n")
}

#' @export
print.ast_symbol <- function(x, ...) {
  cat("<AST symbol> ", x$value, "\n", sep = "")
}

#' @export
print.ast_constant <- function(x, ...) {
  cat("<AST constant> ", x$value, "\n", sep = "")
}

#' @export
print.ast_condition <- function(x, ...) {
  cat("<AST condition> ", x$op, "\n", sep = "")
  cat("  then :\n"); print(x$then)
  cat("  condition:\n"); print(x$condition)
}

#' @export
print.model_structure <- function(x, ...) {
  cat("Model Structure (", x$language, ")\n", sep = "")
  cat("  Sets:       ", length(x$sets), "\n")
  cat("  Mappings:   ", length(x$mappings), "\n")
  cat("  Aliases:    ", length(x$aliases), "\n")
  cat("  Parameters: ", length(x$parameters), "\n")
  cat("  Variables:  ", length(x$variables), "\n")
  cat("  Equations:  ", length(x$equations), "\n")
  cat("  Source:     ", x$source, "\n")
  cat("  Language:   ", x$language, "\n")
}
