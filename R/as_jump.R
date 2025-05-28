#' @title Convert multimod object to Julia/JuMP syntax
#' @description Render a `multimod` object as a JuMP code string.
#' @param x A `multimod` object.
#' @param ... Additional arguments (not used).
#' @returns A character string with valid Julia/JuMP syntax.
#' @export
as_jump <- function(x, ...) {
  UseMethod("as_jump", x)
}

#' @export
as_jump.default <- function(x, ...) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x) && length(x) == 1) {
    return(format(x, scientific = FALSE))
  }
  stop("No as_jump method for object of class: ", class(x))
}

#' @export
#' @method as_jump character
as_jump.character <- function(x, ...) x

#' @export
#' @method as_jump set
as_jump.set <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "set"))
  if (declaration) {
    paste0("setdef = [", x$name, "]  #", ifelse(desc, x$desc, ""))
  } else {
    x$name
  }
}

#' @export
#' @method as_jump dims
as_jump.dims <- function(x, ...) {
  paste0("(", paste0(vapply(x, as.character, character(1)), collapse = ","), ")")
}

#' @export
#' @method as_jump mapping
as_jump.mapping <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "mapping"))
  if (declaration) {
    paste0("setdef = [", as.character(x), "]  #", ifelse(desc, x$desc, ""))
  } else {
    as.character(x)
  }
}

#' @export
#' @method as_jump parameter
as_jump.parameter <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "parameter"))
  if (declaration) {
    paste0("# Parameter: ", x$name, ifelse(desc, paste0(" — ", x$desc), ""))
  } else {
    x$name
  }
}

#' @export
#' @method as_jump variable
as_jump.variable <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "variable"))
  if (declaration) {
    paste0("@variable(model, ", x$name, ")", if (desc) paste0("  # ", x$desc) else "")
  } else {
    x$name
  }
}

#' @export
#' @method as_jump constant
as_jump.constant <- function(x, ...) {
  format(x$value, scientific = FALSE)
}

#' @export
#' @method as_jump sum
as_jump.sum <- function(x, ...) {
  idxs <- as_jump(x$index$then, ...)  # assume a list of index variables
  idx  <- as_jump(x$index$index, ...)    # assume set symbol or expression
  val  <- as_jump(x$value, ...)

  idx_txt <- paste(idxs, collapse = ", ")
  paste0("sum(", val, " for ", idx_txt, " in ", idx, ")")
}

#' @export
#' @method as_jump prod
as_jump.prod <- function(x, ...) {
  idxs <- as_jump(x$index$then, ...)
  idx  <- as_jump(x$index$index, ...)
  val  <- as_jump(x$value, ...)

  idx_txt <- paste(idxs, collapse = ", ")
  paste0("prod(", val, " for ", idx_txt, " in ", idx, ")")
}

#' @export
#' @method as_jump when
as_jump.when <- function(x, ...) {
  cond <- as_jump(x$condition, ...)
  then <- as_jump(x$then, ...)
  otherwise <- if (!is.null(x$otherwise)) as_jump(x$otherwise, ...) else "nothing"

  paste0("(haskey(", cond, ") ? ", then, " : ", else_, ")")
}

#' @export
#' @method as_jump where
as_jump.where <- function(x, ...) {
  as_jump(x$content, ...)
}

#' @export
#' @method as_jump expression
as_jump.expression <- function(x, ...) {
  lhs <- as_jump(x$lhs, ...)
  rhs <- as_jump(x$rhs, ...)
  op <- x$op

  # Julia uses `^` for power, no need to convert
  op <- switch(op,
               "==" = "==",
               "!=" = "!=",
               "<>" = "!=",
               op)

  # Add brackets if marked or needed
  if (isTRUE(x$lhs$brackets)) lhs <- paste0("(", lhs, ")")
  if (isTRUE(x$rhs$brackets)) rhs <- paste0("(", rhs, ")")

  paste(lhs, op, rhs)
}

#' @export
#' @method as_jump equation
as_jump.equation <- function(x, ...) {
  eqname <- x$name
  index <- paste0("(", paste(x$dims, collapse = ", "), ")")
  domain <- if (!is.null(x$domain)) paste0(" in ", as_jump(x$domain, ...)) else ""

  lhs <- as_jump(x$lhs, ...)
  rhs <- as_jump(x$rhs, ...)

  rel <- switch(x$relation,
                "==" = "==",
                "<=" = "<=",
                ">=" = ">=",
                stop("Unsupported relation: ", x$relation))

  paste0(
    "@constraint(\n  model,\n  ",
    "[", index, "]", domain, ",\n  ",
    lhs, " ", rel, " ", rhs,
    "\n);"
  )
}


