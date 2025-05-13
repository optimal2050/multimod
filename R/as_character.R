# as.character ####
#' @export
as.character.ast_expression <- function(x, ...) {
  paste0("(", as.character(x$left), " ", x$op, " ", as.character(x$right), ")")
}

#' @export
as.character.ast_unary <- function(x, ...) {
  paste0(x$op, "(", as.character(x$rhs), ")")
}

#' @export
as.character.ast_variable <- function(x, ...) {
  if (length(x$dims) > 0) {
    paste0(x$name, "[", paste(x$dims, collapse = ","), "]")
  } else {
    x$name
  }
}

#' @export
as.character.ast_parameter <- function(x, ...) {
  if (length(x$dims) > 0) {
    paste0(x$name, "[", paste(x$dims, collapse = ","), "]")
  } else {
    x$name
  }
}

#' @export
as.character.ast_symbol <- function(x, ...) {
  x$value
}

#' @export
as.character.ast_constant <- function(x, ...) {
  as.character(x$value)
}

#' @export
as.character.ast_set <- function(x, ...) {
  paste0("set: ", x$name)
}

as.character.ast_dims <- function(x, ...) {
  # paste0("dims: ", paste(x$dims, collapse = ", "))
  # browser()
  # paste0("[",
         paste(sapply(x$dims, function(y) y$name), collapse = ",")
         # , "]")
}

#' @export
as.character.ast_condition <- function(x, ...) {
  paste0("if (", as.character(x$condition), ") {", as.character(x$then), "}")
}
