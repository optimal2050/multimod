
#' Quote GAMS explanatory text when it needs it
#'
#' A description is free text after the symbol, and GAMS reads a comma in it
#' as the separator before the next declaration - `ratio,hours,lower bound`
#' declared symbols named `hours` and `lower`. Quoting keeps it one token.
#'
#' @param x Description text.
#' @return The text, quoted if it carries a comma, semicolon or quote.
#' @keywords internal
gams_desc <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) return("")
  txt <- as.character(x[1])
  if (grepl("[,;'\"]", txt)) {
    txt <- gsub("'", "", txt, fixed = TRUE)
    txt <- gsub('"', "", txt, fixed = TRUE)
    return(paste0("'", txt, "'"))
  }
  txt
}

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
as_gams.default <- function(x, ...) {
  if (is.null(x)) return(x)
  if (length(x) == 1 && is.numeric(x)) {
    return(format(x, scientific = FALSE))
  } else if (length(x) == 1 && is.character(x)) {
    return(x)
  }
  warning("No as_gams method for object of class: ", class(x))
  x
}

#' @export
#' @method as_gams character
as_gams.character <- function(x, ...) {
  # If x is a character vector, return it as is
  if (is.character(x)) {
    return(x)
  }
  # Otherwise, call the default method
  UseMethod("as_gams", x)
}

#' @export
#' @method as_gams mm_set
as_gams.mm_set <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "mm_set"))
  if (declaration) {
    paste0("set ", x$name, ifelse(desc, paste0("  ", gams_desc(x$desc)), ""),";")
  } else {
    paste(x$name, ifelse(desc, gams_desc(x$desc), ""), sep = "  ")
  }
}

#' @export
#' @method as_gams mm_mapping
as_gams.mm_mapping <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "mm_mapping"))
  if (declaration) {
    paste0("set ", as.character(x), ifelse(desc, paste0("  ", gams_desc(x$desc)), ""), ";")
  } else {
    paste0(as.character(x), ifelse(desc, paste0("  ", gams_desc(x$desc)), ""))
  }
}

#' @export
#' @method as_gams dims
as_gams.dims <- function(x, ...) {
  # paste0("(", paste0(vapply(x, as.character, ""), collapse = ","), ")")
  as.character(x, ...)
}

#' @export
#' @method as_gams mm_parameter
as_gams.mm_parameter <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "mm_parameter"))
  if (declaration) {
    paste0("parameter ", as.character(x), ifelse(desc, paste0("  ", gams_desc(x$desc)), ""), ";")
  } else {
    paste0(as.character(x), ifelse(desc, paste0("  ", gams_desc(x$desc)), ""))
  }
}

#' @export
#' @method as_gams mm_variable
as_gams.mm_variable <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "mm_variable"))
  if (declaration) {
    paste0("variable ", as.character(x),
                  ifelse(desc, paste0("  ", gams_desc(x$desc)), ""), ";")
  } else {
    paste0(as.character(x), ifelse(desc, paste0("  ", gams_desc(x$desc)), ""))
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
  idx <- as_gams(x$index, brackets = .gams_index_brackets(x$index), ...)
  val <- as_gams(x$value, ...)
  paste0("sum(", idx, ", ", val,")")
}

# GAMS needs a controlling index of several sets bracketed: sum((i,j), ...).
# The index is either a dims node (ast_sum()) or a when node whose `then`
# holds the dims (GAMS-style $-condition).
.gams_index_brackets <- function(index) {
  n <- if (inherits(index, "when")) length(index$then) else length(index)
  if (isTRUE(n > 1)) "[]" else ""
}

#' @export
#' @method as_gams prod
as_gams.prod <- function(x, ...) {
  idx <- as_gams(x$index, brackets = .gams_index_brackets(x$index), ...)
  val <- as_gams(x$value, ...)
  paste0("prod(", idx, ", ", val, ")")
}

#' @export
#' @method as_gams shift
as_gams.shift <- function(x, ...) {
  # A lag or lead on a set index. GAMS doubles the operator for the circular
  # form: `y-1` stops at the ends, `y--1` wraps.
  offset <- x$offset
  if (is.null(offset) || offset == 0) return(x$symbol)
  op <- if (offset > 0) "+" else "-"
  if (isTRUE(x$circular)) op <- paste0(op, op)
  paste0(x$symbol, op, abs(offset))
}

#' @export
#' @method as_gams setmin
as_gams.setmin <- function(x, ...) {
  idx <- as_gams(x$index, brackets = .gams_index_brackets(x$index), ...)
  val <- as_gams(x$value, ...)
  paste0("smin(", idx, ", ", val, ")")
}

#' @export
#' @method as_gams setmax
as_gams.setmax <- function(x, ...) {
  idx <- as_gams(x$index, brackets = .gams_index_brackets(x$index), ...)
  val <- as_gams(x$value, ...)
  paste0("smax(", idx, ", ", val, ")")
}

#' @export
#' @method as_gams func
#' @rdname as_gams
as_gams.func <- function(x, ...) {
  val <- if (inherits(x$value, c("ast", "multimod"))) {
    as_gams(x$value, ...)
  } else if (is.list(x$value) && !is.data.frame(x$value)) {
    sapply(x$value, as_gams, ...)
  } else {
    stop("Unsupported value type for func: ", class(x$value))
  }
  val_str <- paste(val, collapse = ", ")

  if (!is.null(x$index)) {
    idx <- as_gams(x$index, ...)
    return(paste0(x$name, "(", idx, ", ", val_str, ")"))
  } else {
    return(paste0(x$name, "(", val_str, ")"))
  }
}


#' @export
#' @method as_gams call
#' @rdname as_gams
as_gams.call <- function(x, ...) {
  # A generic function application: name(arg, ...). Without this method the
  # node fell through to as_gams.default, which warned and printed the list
  # structure, so log(x) and the other transcendentals left broken GAMS.
  stopifnot(!is.null(x$name))
  if (!is.null(x$args) && length(x$args) > 0) {
    args_text <- paste(vapply(x$args, as_gams, character(1), ...), collapse = ", ")
    paste0(x$name, "(", args_text, ")")
  } else {
    paste0(x$name, "()")
  }
}



#' @export
#' @method as_gams when
as_gams.when <- function(x, ...) {
  then <- as_gams(x$then, ...)
  cond <- as_gams(x$condition, ...)
  # GAMS binds `$` tighter than `and`/`or`, so the condition is bracketed.
  # A dims node is already a bracketed tuple and is not wrapped again.
  lhs <- if (inherits(x$then, "dims")) then else paste0("(", then, ")")
  paste0(lhs, "$(", cond, ")")
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
#' @method as_gams unary
as_gams.unary <- function(x, ...) {
  stopifnot(inherits(x, "unary"))
  op <- x$op
  # The operand of a unary node is in $rhs.
  operand <- as_gams(x$rhs, ...)
  if (op == "-") {
    # GAMS refuses two operators in a row, so a compound operand is bracketed.
    if (grepl("[-+]", operand)) paste0("-(", operand, ")") else paste0("-", operand)
  } else if (op == "not") {
    paste0("not (", operand, ")")
  } else {
    stop("Unrecognized unary operator for GAMS export: ", op)
  }
}


#' @export
#' @method as_gams mm_equation
as_gams.mm_equation <- function(x, ...) {
  eqname <- x$name
  # The equation is indexed by its domain mapping, so the header binds the
  # domain's dims rather than the base sets carried by x$dims.
  index_dims <- x$dims
  if (inherits(x$domain, "mm_mapping") && !is.null(x$domain$dims) &&
      length(x$domain$dims) == length(x$dims)) {
    index_dims <- x$domain$dims
  }
  index <- as_gams(index_dims, ...)
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

