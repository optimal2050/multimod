
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
#' @examples
#' toy <- example_models$toy
#' as_gams(toy$equations$eqDemand)
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
  } else if (isTRUE(desc)) {
    paste(x$name, gams_desc(x$desc), sep = "  ")
  } else {
    x$name
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
  idx <- .gams_iterated_index(x$index, body = x$value, ...)
  val <- as_gams(x$value, ...)
  paste0("sum(", idx, ", ", val,")")
}

# A MathProg iterator ranges over a set under its own name (`sum{t in TECH}`);
# the body then refers to `t`. GAMS has no iterator distinct from a set, so the
# iterator is rendered as the controlling index and `write_gams()` declares it
# an alias of the set. An index whose entries carry no iterator name, or carry
# the set's own name, renders as before.
.gams_iterated_index <- function(index, body = NULL, ...) {
  iters <- .gams_index_iterators(index, body)
  if (is.null(iters)) {
    return(as_gams(index, brackets = .gams_index_brackets(index), ...))
  }
  if (length(iters) > 1) paste0("[", paste(iters, collapse = ","), "]") else iters
}

# Iterator names of a dims index, or NULL when they add nothing to the set
# names. They are used only when the body refers to them: a parse that has
# already resolved the body's indices to set names (`x[L]`) keeps the set
# names in the index too.
.gams_index_iterators <- function(index, body = NULL) {
  if (!inherits(index, "dims") || length(index) == 0) return(NULL)
  iters <- names(index)
  if (is.null(iters) || any(is.na(iters) | !nzchar(iters))) return(NULL)
  sets <- vapply(index, function(d) if (is.null(d$name)) NA_character_ else d$name, "")
  if (any(is.na(sets)) || identical(unname(iters), unname(sets))) return(NULL)
  if (!.gams_body_uses(body, setdiff(iters, sets))) return(NULL)
  iters
}

# Whether any of `names` appears as an index symbol anywhere in `node`.
.gams_body_uses <- function(node, names) {
  if (!length(names)) return(TRUE)
  found <- FALSE
  walk <- function(x) {
    if (found) return(invisible())
    if (inherits(x, "symbol") && !is.null(x$name) && x$name %in% names) {
      found <<- TRUE
      return(invisible())
    }
    if (is.list(x)) for (child in x) walk(child)
  }
  walk(node)
  found
}

# Set-iterator pairs an equation needs declared as GAMS aliases: from its own
# header (`dims_index_aliases`) and from every sum or product inside it.
.gams_equation_aliases <- function(eq) {
  pairs <- list()
  sets <- vapply(eq$dims, function(d) if (is.null(d$name)) NA_character_ else d$name, "")
  hdr <- .gams_header_iterators(eq)
  if (!is.null(hdr)) pairs <- c(pairs, Map(c, sets, hdr))
  walk <- function(node) {
    if (inherits(node, c("sum", "prod"))) {
      iters <- .gams_index_iterators(node$index, node$value)
      if (!is.null(iters)) {
        isets <- vapply(node$index, function(d) d$name, "")
        pairs <<- c(pairs, Map(c, isets, iters))
      }
    }
    if (is.list(node)) for (child in node) walk(child)
  }
  walk(eq$lhs)
  walk(eq$rhs)
  unique(lapply(pairs, unname))
}

# Declaration domain of an equation, bracketed; "" for a scalar. It is the
# index of the definition header (see as_gams.mm_equation()) before MathProg
# iterators are substituted, so a header over aliases matches its declaration.
.gams_equation_domain <- function(eq) {
  index_dims <- .gams_equation_index_dims(eq)
  if (length(index_dims) == 0) return("")
  sets <- vapply(index_dims, function(d) if (is.null(d$name)) NA_character_ else d$name, "")
  if (any(is.na(sets))) return("")
  paste0("[", paste(sets, collapse = ","), "]")
}

# The equation is indexed by its domain mapping when it has one, so the header
# binds the domain's dims rather than the base sets carried by eq$dims.
.gams_equation_index_dims <- function(eq) {
  if (inherits(eq$domain, "mm_mapping") && !is.null(eq$domain$dims) &&
      length(eq$domain$dims) == length(eq$dims)) {
    return(eq$domain$dims)
  }
  eq$dims
}

# Iterator names for an equation header, or NULL to keep the set names.
.gams_header_iterators <- function(eq) {
  iters <- eq$dims_index_aliases
  if (is.null(iters) || length(iters) != length(eq$dims) || length(iters) == 0) {
    return(NULL)
  }
  iters <- unname(as.character(iters))
  sets <- vapply(eq$dims, function(d) if (is.null(d$name)) NA_character_ else d$name, "")
  if (any(is.na(iters) | !nzchar(iters)) || any(is.na(sets)) ||
      identical(iters, unname(sets))) {
    return(NULL)
  }
  if (!.gams_body_uses(list(eq$lhs, eq$rhs), setdiff(iters, sets))) return(NULL)
  iters
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
  idx <- .gams_iterated_index(x$index, body = x$value, ...)
  val <- as_gams(x$value, ...)
  paste0("prod(", idx, ", ", val, ")")
}

#' @export
#' @method as_gams ast_formula
as_gams.ast_formula <- function(x, ...) {
  # A parameter defined by a formula is a declaration plus an assignment in
  # GAMS, not MathProg's inline `:=`. This renders the assignment.
  if (is.null(x$expr)) {
    stop("as_gams: a formula with no expression cannot be written.",
         call. = FALSE)
  }
  idx <- x$index_vars
  if (is.null(idx) || !length(idx)) idx <- x$index_sets
  lhs <- if (length(idx)) {
    paste0(x$target, "(", paste(idx, collapse = ","), ")")
  } else {
    x$target
  }
  paste0(lhs, " = ", as_gams(x$expr, ...), ";")
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
  index <- as_gams(.gams_equation_index_dims(x), ...)
  if (is.null(x$domain)) {
    hdr <- .gams_header_iterators(x)
    if (!is.null(hdr)) index <- paste0("[", paste(hdr, collapse = ","), "]")
  }
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

