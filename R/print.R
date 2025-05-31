# ---
# Title: Print Methods for AST and Multimod Classes
# Description: S3 print methods to display human-readable summaries of multimod and AST objects.
# Purpose: Improve interpretability and debugging of parsed modeling components.
# Dependencies: Assumes ast and symbol classes are defined.
# ---

# This file defines S3 print methods for:
# - Abstract syntax tree (AST) nodes: expr, var, param, sum, cond, etc.
# - Multimod domain-specific objects: equation, model, etc.
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

# S3 generic dispatch for print(x) where class(x) is e.g. expr, equation, etc.

#' @title Print methods for AST classes

#' @export
#' @method print ast
print.ast <- function(x, ...) {
  cat("<ast>", "\n")
  print(as.character(x), ...)
}

#' @export
#' @method print set
print.set <- function(x, ...) {
  cat("<AST set> ", x$name, "\n", sep = "")
}

#' @export
#' @method print dims
print.dims <- function(x, brackets = "[", ...) {
  cat("<AST dims> \n")
  cat(as.character(x, brackets = brackets))
}

#' @export
#' @method print expression
print.expression <- function(x, max_char = 50, ...) {
  cat("<AST expression> ", x$op, "\n", sep = "")
  cat("  lhs :\n"); print(x$lhs, max_char = max_char, ...)
  cat("  rhs:\n"); print(x$rhs, max_char = max_char, ...)
}

#' @export
#' @method print unary
print.unary <- function(x, ...) {
  cat("<AST unary>\n")
  cat("  Operator: ", x$op, "\n", sep = "")
  cat("  Argument:\n")
  print(x$rhs, ...)
  invisible(x)
}


#' @export
#' @method print variable
print.variable <- function(x, ...) {
  cat("<AST variable> ", x$name, "\n", sep = "")
  if (length(x$dims) > 0) cat("  dims: ", paste(x$dims, collapse = ", "), "\n")
}

#' @export
#' @method print parameter
print.parameter <- function(x, ...) {
  cat("<AST parameter> ", x$name, "\n", sep = "")
  if (length(x$dims) > 0) cat("  dims: ", as.character(x$dims), "\n")
}

#' @export
#' @method print mapping
print.mapping <- function(x, ...) {
  cat("<AST mapping> ", x$name, "\n", sep = "")
  if (length(x$dims) > 0) cat("  dims: ", paste(x$dims, collapse = ", "), "\n")
}

#' @export
#' @method print symbol
print.symbol <- function(x, ...) {
  cat("<AST symbol> ", x$name, "\n", sep = "")
  if (length(x$dims) > 0) {
    cat(" ", as.character(x$dims), "\n")
  }
}

#' @export
#' @method print constant
print.constant <- function(x, ...) {
  cat("<AST constant> ", x$value, "\n", sep = "")
}

#' @export
#' @method print when
print.when <- function(x, ...) {
  cat("<AST when> ", x$op, "\n", sep = "")
  cat("  condition:\n"); print(x$condition)
  cat("  then :\n"); print(x$then)
  cat("  otherwise :\n"); print(x$otherwise)
}

#' @export
#' @method print where
print.where <- function(x, ...) {
  cat("<AST where> ", x$name, "\n", sep = "")
  cat("content: ", as.character(x$content), "\n")
}

#' @export
#' @method print equation
print.equation <- function(x, ...) {
  # browser()
  cat("<AST equation> ", x$name, "\n", sep = "")
  if (!is.null(x$domain)) {
    cat("  domain: ", as.character(x$domain), "\n")
  }
  cat("  relation: ", x$relation, "\n")
  cat("  lhs: ", as.character(x$lhs), "\n")
  cat("  rhs: ", as.character(x$rhs), "\n")
}

#' @export
#' @method print sum
print.sum <- function(x, ...) {
  cat("<AST sum> ", as.character(x$index), "\n", sep = "")
  cat("  value: ", as.character(x$value), "\n")
  if (!is.null(x$domain)) {
    cat("  domain: ", as.character(x$domain), "\n")
  }
}

#' @export
#' @method print prod
print.prod <- function(x, ...) {
  cat("<AST prod> ", as.character(x$index), "\n", sep = "")
  cat("  value: ", as.character(x$value), "\n")
  if (!is.null(x$domain)) {
    cat("  domain: ", as.character(x$domain), "\n")
  }
}

# print.func <- function(x, ...) {
#   cat("<AST function> ", x$name, "\n", sep = "")
#   if (!is.null(x$index)) {
#     cat("  index: ", as.character(x$index), "\n")
#   }
#   cat("  value: ", as.character(x$value), "\n")
# }

#' @export
#' @method print func
#' @rdname print
print.func <- function(x, ...) {
  cat("<func>\n")
  cat("  name:   ", x$name, "\n")
  if (!is.null(x$index)) {
    cat("  index:  "); print(x$index)
  }
  cat("  value:  ")
  if (is.list(x$value)) {
    lapply(x$value, print)
  } else {
    print(x$value)
  }
  invisible(x)
}


#' @export
#' @method print model_structure
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

#' @export
#' @method print model
print.model <- function(x, ...) {
  cat("Model: ", x$name, "\n", sep = "")
  if (!is.null(x$desc)) {
    cat("Description: ", x$desc, "\n", sep = "")
  }
  if (!is.null(x$authors)) {
    cat("Authors: ", paste(x$authors, collapse = ", "), "\n")
  }
  if (!is.null(x$source)) {
    cat("Source: ", x$source, "\n")
  }
  if (!is.null(x$language)) {
    cat("Language: ", x$language, "\n")
  }
  cat("Sets: ", length(x$sets), "\n")
  cat("Mappings: ", length(x$mappings), "\n")
  cat("Aliases: ", length(x$aliases), "\n")
  cat("Parameters: ", length(x$parameters), "\n")
  cat("Variables: ", length(x$variables), "\n")
  cat("Equations: ", length(x$equations), "\n")
}
