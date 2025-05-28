
#' @title Convert multimod object to GAMS syntax
#' @description Render a `multimod` object as a GAMS code string.
#' @param x A `multimod` object.
#' @param ... Additional arguments (not used).
#' @returns A character string with valid GAMS syntax.
#' @export
as_gams <- function(x, ...) {
  UseMethod("as_gams", x)
}


#' @export
#' @method as_gams set
as_gams.set <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "set"))
  if (declaration) {
    paste0("set ", x$name, ifelse(desc, paste0(" ", x$desc), ""),";")
  } else {
    paste0(x$name, ifelse(desc, x$desc, ""))
  }
}

#' @export
#' @method as_gams mapping
as_gams.mapping <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "mapping"))
  if (declaration) {
    paste0("set ", as.character(x), ifelse(desc, paste0(" ", x$desc), ""), ";")
  } else {
    paste0(as.character(x), ifelse(desc, paste0(" ", x$desc), ""))
  }
}

#' @export
#' @method as_gams dims
as_gams.dims <- function(x, ...) {
  # paste0("(", paste0(vapply(x, as.character, ""), collapse = ","), ")")
  as.character(x, ...)
}

#' @export
#' @method as_gams parameter
as_gams.parameter <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "parameter"))
  if (declaration) {
    paste0("parameter ", as.character(x), ifelse(desc, paste0(" ", x$desc), ""), ";")
  } else {
    paste0(as.character(x), ifelse(desc, paste0(" ", x$desc), ""))
  }
}

#' @export
#' @method as_gams variable
as_gams.variable <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "variable"))
  if (declaration) {
    paste0("variable ", as.character(x),
                  ifelse(desc, paste0(" ", x$desc), ""), ";")
  } else {
    paste0(as.character(x), ifelse(desc, paste0(" ", x$desc), ""))
  }
}

#' @export
#' @method as_gams constant
as_gams.constant <- function(x, ...) {
  format(x$value, scientific = FALSE)
}

#' @export
#' @method as_gams symbol
as_gams.symbol <- function(x, ...) {
  x$name
}

#' @export
#' @method as_gams sum
as_gams.sum <- function(x, ...) {
  # browser()
  idx_brackets <- ifelse(isTRUE(length(x$index$then) > 1), "[]", "")
  idx <- as_gams(x$index, brackets = idx_brackets, ...)
  val <- as_gams(x$value, ...)
  paste0("sum(", idx, ", ", val,")")
}

#' @export
#' @method as_gams prod
as_gams.prod <- function(x, ...) {
  idx_brackets <- ifelse(isTRUE(length(x$index$then) > 1), "[]", "")
  idx <- as_gams(x$index, brackets = idx_brackets, ...)
  val <- as_gams(x$value, ...)
  paste0("prod(", idx, val,")")
}

#' @export
#' @method as_gams when
as_gams.when <- function(x, ...) {
  then <- as_gams(x$then, ...)
  cond <- as_gams(x$condition, ...)
  paste0("(", then, ")$", cond)
}

#' @export
#' @method as_gams where
as_gams.where <- function(x, ...) {
  as_gams(x$content, ...)
}

#' @export
#' @method as_gams expression
as_gams.expression <- function(x, ...) {
  stopifnot(inherits(x, "expression"))

  gams_ops <- c(
    "+", "-", "*", "/", "**",
    "<", ">", "=", "<=", ">=", "==", "!=", "<>",
    "and", "or", "not"
  )

  lhs <- as_gams(x$lhs, ...)
  rhs <- as_gams(x$rhs, ...)

  # Wrap sides in parentheses if the $brackets flag is TRUE
  if (isTRUE(x$lhs$brackets)) lhs <- paste0("(", lhs, ")")
  if (isTRUE(x$rhs$brackets)) rhs <- paste0("(", rhs, ")")

  # Operator normalization if needed
  op <- x$op
  if (op == "^") op <- "**"
  if (op == "==") op <- "="
  if (op == "!=") op <- "<>"

  if (!op %in% gams_ops) {
    stop("Unsupported operator for GAMS export: ", op)
  }

  paste(lhs, op, rhs)
}

#' @export
#' @method as_gams equation
as_gams.equation <- function(x, ...) {
  # browser()
  eqname <- x$name
  index <- as_gams(x$dims, ...)
  domain <- if (!is.null(x$domain)) paste0("$", as_gams(x$domain, ...)) else ""
  lhs <- as_gams(x$lhs, ...)
  rhs <- as_gams(x$rhs, ...)
  rel <- switch(x$relation,
                "==" = "=e=",
                "<=" = "=l=",
                ">=" = "=g=",
                stop("Unsupported relation: ", x$relation)
  )
  paste0(eqname, index, domain, "..\n  ",
         lhs, " ", rel, "\n  ", rhs, ";")
}

