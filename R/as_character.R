

#' Get a pair of brackets
#'
#' This function returns a pair of brackets based on the input character.
#'
#' @param x A character string or vector of length 1 or 2.
#' If the length is 1, it will return a pair of brackets based on the
#' character. If the length is 2, it will return the two characters as a vector.
#'
#' @return A character vector of length 2 containing the opening and closing brackets.
#'
#' @export
brackets_pair <- function(x) {
  if (is_empty(x) | isTRUE(x == "")) {
    return(NULL)
  } else if (length(x) > 2) {
    stop("x must be of length 1 or 2")
  } else if (length(x) == 2) {
    return(c(x[1], x[2]))
  }
  if (nchar(x) == 2) {
    return(c(substr(x, 1, 1), substr(x, 2, 2)))
  } else if (nchar(x) == 1) {
    switch(x,
      "[" = c("[", "]"),
      "]" = c("[", "]"),
      "{" = c("{", "}"),
      "}" = c("{", "}"),
      "(" = c("(", ")"),
      ")" = c("(", ")"),
      "<" = c("<", ">"),
      ">" = c("<", ">"),
      "/" = c("/", "/"),
      "\\" = c("\\", "\\"),
      "|" = c("|", "|"),
      c(x, x) # return the same character twice
    )
  } else {
    stop("x must have on or two characters, ",
         "try using length of two for special cases.")
  }

}


#' @export
add_brackets <- function(x, brackets = NULL, ...) {
  stopifnot(length(x) == 1)
  if (is_empty(x)) return(x)
  brackets <- brackets_pair(brackets)
  return(paste0(brackets[1], x, brackets[2]))
}


# as.character ####
#' @export
#' @method as.character ast_expression
as.character.ast_expression <- function(x, brackets = NULL, ...) {
  out <- paste0(as.character(x$lhs), " ", x$op, " ", as.character(x$rhs))
  if (is_empty(brackets)) return(out)
  brackets <- brackets_pair(brackets)
  out <- paste0(brackets[1], out, brackets[2])
  return(out)
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
#' @method as.character ast_symbol
as.character.ast_symbol <- function(x, ...) {
  x$value
}

#' @export
#' @method as.character ast_constant
as.character.ast_constant <- function(x, ...) {
  as.character(x$value)
}

#' @export
#' @method as.character ast_set
as.character.ast_set <- function(x, ...) {
  paste0("set: ", x$name)
}

#' @export
#' @method as.character ast_dims
as.character.ast_dims <- function(x, brackets = "[", ...) {
  # browser()
  if (length(x$dims) == 0) return("")
  out <- paste(sapply(x$dims, function(y) y$name), collapse = ",")
  if (is.null(brackets) || all(brackets == "")) return(out)
  brackets <- brackets_pair(brackets)
  out <- paste0(brackets[1], out, brackets[2])
  return(out)
}

#' @export
as.character.ast_condition <- function(x, ...) {
  paste0("if (", as.character(x$condition), ") {", as.character(x$then), "}")
}
