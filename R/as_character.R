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
    stop("x must have one or two characters, ",
         "try using length of two for special cases.")
  }

}

#' @export
wrap_brackets <- function(x, brackets = NULL, ...) {
  stopifnot(length(x) == 1)
  if (is_empty(x)) return(x)
  brackets <- brackets_pair(brackets)
  return(paste0(brackets[1], x, brackets[2]))
}

# as.character ####
#' @export
#' @method as.character expression
as.character.expression <- function(x, brackets = NULL, max_char = 50, ...) {
  out <- paste0(as.character(x$lhs, max_char = NULL),
                " ", x$op, " ",
                as.character(x$rhs, max_char = NULL)
                )
  if (!is_empty(max_char) && nchar(out) > max_char) {
    out <- paste0(substr(out, 1, max_char), "...")
  }
  if (is_empty(brackets)) return(out)
  brackets <- brackets_pair(brackets)
  out <- paste0(brackets[1], out, brackets[2])
  return(out)
}

#' @export
#' @method as.character unary
as.character.unary <- function(x, ...) {
  paste0(x$op, "(", as.character(x$rhs), ")")
}

#' @export
#' @method as.character variable
as.character.variable <- function(x, ...) {
  if (length(x$dims) > 0) {
    paste0(x$name, as.character(x$dims))
  } else {
    x$name
  }
}

#' @export
#' @method as.character parameter
as.character.parameter <- function(x, ...) {
  if (length(x$dims) > 0) {
    paste0(x$name, as.character(x$dims))
  } else {
    x$name
  }
}

#' @export
#' @method as.character mapping
as.character.mapping <- function(x, ...) {
  if (length(x$dims) > 0) {
    paste0(x$name, as.character(x$dims))
  } else {
    x$name
  }
}

#' @export
#' @method as.character symbol
as.character.symbol <- function(x, ...) {
  x$value
}

#' @export
#' @method as.character constant
as.character.constant <- function(x, ...) {
  as.character(x$value)
}

#' @export
#' @method as.character set
as.character.set <- function(x, ...) {
  paste0("set: ", x$name)
}

#' @export
#' @method as.character dims
as.character.dims <- function(x, brackets = "[", ...) {
  if (length(x) == 0) return("")
  out <- paste(sapply(x, function(y) y$name), collapse = ",")
  if (is.null(brackets) || all(brackets == "")) return(out)
  brackets <- brackets_pair(brackets)
  out <- paste0(brackets[1], out, brackets[2])
  return(out)
}

#' @export
#' @method as.character when
as.character.when <- function(x, ...) {
  paste0("if (", as.character(x$condition), ") {", as.character(x$then), "}")
}

#' @export
#' @method as.character sum
as.character.sum <- function(x, max_char = 50, ...) {
  out <- paste0("sum(",
                as.character(x$index, max_char = NULL),
                as.character(x$value, max_char = NULL),
                ")")
  if (!is_empty(max_char) && nchar(out) > max_char) {
    out <- paste0(substr(out, 1, max_char), "...")
  }
  return(out)
}

#' @export
#' @method as.character prod
as.character.prod <- function(x, ...) {
  paste0("prod(", as.character(x$index), as.character(x$value), ")")
}

