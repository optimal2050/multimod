# Functions to parse expression string into tokens and then into an AST
# Mostly for internal use

# Operator precedence with symbolic alternatives
operator_precedence <- c(
  "**" = 1, "^" = 1,
  "*" = 2, "/" = 2,
  "+" = 3, "-" = 3,
  "==" = 4, "!=" = 4, "<>" = 4, "<=" = 4, ">=" = 4, "=" = 4, "<" = 4, ">" = 4,
  "and" = 5, "&" = 5,
  "or"  = 6, "|" = 6,
  "not" = 8, "!" = 8
)
# "$" = 9 # !!! double check

# List of known/used operators
known_operators <- names(operator_precedence)

# Sort operators by precedence and length (longer ones first if tied)
# Example usage:
# sort_by_precedence(c("or", "+", "*"), descending = TRUE)
sort_by_precedence <- function(ops,
                               precedence = operator_precedence,
                               descending = FALSE) {
  # browser()
  if (is.null(precedence)) {
    return(ops)
  }
  missing <- setdiff(ops, names(precedence))
  if (length(missing) > 0) {
    stop("Unknown operator(s): ", paste(missing, collapse = ", "))
  }

  d <- data.frame(
    ops = ops,
    ranks = precedence[ops],
    lengths = nchar(ops)
  )
  d <- d[order(-d$lengths), ]

  if (descending) {
    d <- d[order(-d$ranks), ]
  } else {
    d <- d[order(d$ranks), ]
  }

  return(d$ops)

}

# Retrieve the rank of an operator from the precedence table
get_operator_precedence <- function(op, precedence = operator_precedence) {
  if (op %in% names(precedence)) {
    return(precedence[[op]])
  }
  return(NA_integer_)
}

# Identify nested expression levels in an expression
expression_level <- function(
    expr,
    inc_parens = FALSE,
    as_data_frame = FALSE) {
  chars <- strsplit(expr, "")[[1]]
  ii <- rep(NA, length(chars))
  level <- 0
  for (i in seq_along(chars)) {
    ch <- chars[i]
    ii[i] <- level
    if (ch == "(") {
      if (!inc_parens) {
        ii[i] <- level
      }
      level <- level + 1
    } else if (ch == ")") {
      level <- max(0, level - 1)
      if (!inc_parens) {
        ii[i] <- level
      }
    }
  }

  if (as_data_frame) {
    return(data.frame(index = seq_along(chars), char = chars, level = ii))
  }

  return(ii)
}

# Find and return the top-level operators in an expression
top_level_operators <- function(
    expr,
    # ops = c("+", "-", "*", "/", "<", "<=", ">=", "=", "==", "and", "or", "not")
    ops = gams_ops,
    precedence = operator_precedence,
    descending = FALSE,
    expr_lev = NULL,
    # fixed = FALSE,
    full_table = FALSE) {
  # browser()
  # if (grepl("\\*\\*", expr)) {
  #   browser()
  # }
  # ops <- ops[order(nchar(ops), decreasing = TRUE)]
  # expr_chars <- strsplit(expr, "")[[1]]
  # word_buffer <- ""
  if (is.null(expr_lev)) {
    expr_lev <- expression_level(expr, as_data_frame = TRUE)
  } else {
    stopifnot(is.data.frame(expr_lev))
    stopifnot(all(c("index", "char", "level") %in% names(expr_lev)))
    stopifnot(nrow(expr_lev) == nchar(expr))
  }

  expr_lev$op <- NA_character_
  expr_lev$pos <- NA_integer_
  expr_lev$prec <- NA_integer_

  # Support for multi-character operators: sort by length
  ops <- ops[order(nchar(ops), decreasing = TRUE)]
  for (op in ops) {
    op <- trimws(op)
    # check if op is a special character
    if (is_special(op)) {
      fixed <- TRUE
      op_sp <- op
    } else if (grepl("^[a-zA-Z]+$", op)) {
      fixed <- FALSE
      # add white space around the op
      op_sp <- paste0("\\b", op, "\\b")
    } else {
      stop("Invalid/Unrecognized operator: ", op)
    }
    op_nn <- gregexpr(op_sp, expr, fixed = fixed)[[1]]
    if (op_nn[1] == -1) next
    expr_lev$op[op_nn] <- op
    expr_lev$pos[op_nn] <- op_nn
    # expr_lev$prec[op_nn] <- operator_precedence[[op]] %||% NA_integer_
    expr_lev$prec[op_nn] <- get_operator_precedence(op, precedence = precedence)
    # if (!is.null(descending)) {
    #   expr_lev$prec[op_nn] <- sort_by_precedence(op, descending = descending)
    # }

    # replace matched positions in expr with white space
    expr <- gsub(op_sp, " ", expr, fixed = fixed)

    # if (!is.null(precedence[[op]])) {
    #   expr_lev$prec[op_nn] <- precedence[[op]]
    # } else {
    # Default precedence for unknown operators
    # }
  }

  if (full_table) {
    return(expr_lev)
  }

  # browser()
  expr_lev <- expr_lev[expr_lev$level == 0 & !is.na(expr_lev$op), ]
  expr_lev <- expr_lev[order(expr_lev$prec, decreasing = isTRUE(descending)), ]
  expr_lev <- expr_lev[, names(expr_lev) %in% c("op", "pos", "prec")]

  return(expr_lev)
}

# Split an expression into tokens based on top-level operators
split_top_level <- function(
    expr,
    ops,
    expr_lev = NULL,
    keep_op = TRUE,
    as_list = FALSE) {
  # browser()
  if (is.null(expr_lev)) {
    expr_lev <- expression_level(expr, as_data_frame = TRUE)
  } else {
    stopifnot(is.data.frame(expr_lev))
    stopifnot(all(c("index", "char", "level") %in% names(expr_lev)))
    stopifnot(nrow(expr_lev) == nchar(expr))
  }

  top_lev <- min(expr_lev$level)

  if (is.na(top_lev)) {
    stop("NA value in level column")
  }

  expr_lev$op <- NA_character_
  expr_lev$split <- FALSE

  for (op in ops) {
    op <- trimws(op)
    if (is_special(op)) {
      fixed <- TRUE
      op_sp <- op
    } else if (grepl("^[a-zA-Z]+$", op)) {
      fixed <- FALSE
      # add white space around the op
      op_sp <- paste0("\\b", op, "\\b")
    } else {
      stop("Invalid/Unrecognized operator: ", op)
    }

    # check if op is in the expression
    op_nn <- gregexpr(op_sp, expr, fixed = fixed)[[1]]
    if (op_nn[1] == -1) next
    # browser()
    for (i in seq_along(op_nn)) {
      expr_lev$op[op_nn[i]] <- op
      expr_lev$split[op_nn[i]:(op_nn[i] + nchar(op) - 1)] <- TRUE
    }
  }

  expr_lev$split[expr_lev$level != top_lev] <- FALSE

  tokens <- list()
  ntoken <- 0
  for (i in seq_along(expr_lev$char)) {
    if (expr_lev$split[i]) {
      # browser()
      # split point
      if (i > 1 && !expr_lev$split[i - 1]) { # end of previous token
        stopifnot(ntoken > 0)
        # save previous token
        token_end <- i - 1
        tokens[[ntoken]] <- paste0(
          expr_lev$char[token_start:token_end],
          collapse = ""
        )
        # save operator
        if (keep_op) {
          ntoken <- ntoken + 1
          tokens[[ntoken]] <- expr_lev$op[i]
        }
      } else if (i == 1) {
        if (keep_op) {
          # save operator
          ntoken <- ntoken + 1
          tokens[[ntoken]] <- expr_lev$op[i]
        }
      }
      next
    } else { # body
      if (i == 1 || expr_lev$split[i - 1]) { # start of new token
        ntoken <- ntoken + 1
        token_start <- i
      } else if (i == length(expr_lev$char)) { # end of last token
        stopifnot(ntoken > 0)
        tokens[[ntoken]] <- paste0(
          expr_lev$char[token_start:i],
          collapse = ""
        )
      }
    }
  }

  if (as_list) {
    return(tokens)
  }

  return(unlist(tokens))
}

# @export
# needs_brackets <- function(expr, parent = NULL, side = NULL) {
#   UseMethod("needs_brackets")
# }

# @export
#
# @method needs_brackets expression
# needs_brackets.expression <- function(expr, parent = NULL, side = NULL) {
#   if (is.null(expr$op)) return("none")
#
#   op_prec <- operator_precedence[[expr$op]]
#   parent_prec <- if (!is.null(parent) && !is.null(parent$op)) operator_precedence[[parent$op]] else op_prec
#
#   # Recursively inspect children
#   lhs_bracket <- if (!is.null(expr$lhs) && inherits(expr$lhs, "expression")) {
#     needs_brackets(expr$lhs, expr, "lhs")
#   } else "none"
#
#   rhs_bracket <- if (!is.null(expr$rhs) && inherits(expr$rhs, "expression")) {
#     needs_brackets(expr$rhs, expr, "rhs")
#   } else "none"
#
#   # Parent binds more tightly than child ⇒ wrap required
#   if (!is.null(parent) && parent_prec < op_prec) return("required")
#
#   # Special case: division chain — b / (c / d) needs brackets around rhs
#   if (!is.null(parent) && parent$op == "/" && expr$op == "/" && side == "rhs") {
#     return("required")
#   }
#
#   # Suggest brackets for visual clarity if children need them
#   if (lhs_bracket == "required" || rhs_bracket == "required") return("suggested")
#
#   # For associative chains: excess brackets
#   if (expr$op %in% c("+", "-", "*", "and", "or")) return("excessive")
#
#   return("none")
# }

# needs_brackets <- function(node, parent = NULL) {
#   if (!inherits(node, "expression")) return(FALSE)
#   if (is.null(parent)) return(FALSE)
#   parent_op <- parent$op
#   # browser()
#   # Get precedence of this and parent
#   this_op <- node$op
#   this_prec <- if (isTRUE(this_op %in% names(operator_precedence))) {
#     operator_precedence[[this_op]]
#   } else {Inf}
#   parent_prec <- if (isTRUE(parent_op %in% names(operator_precedence))) {
#     operator_precedence[[parent_op]]
#   } else {-Inf}
#
#   # Recurse on children
#   lhs_brackets <- if (!is.null(node$lhs)) needs_brackets(node$lhs, this_op) else FALSE
#   rhs_brackets <- if (!is.null(node$rhs)) needs_brackets(node$rhs, this_op) else FALSE
#
#   # Case 1: this expression is inside a higher precedence context (e.g., a+b in a*b)
#   if (this_prec > parent_prec) return(TRUE)
#
#   # Case 2: nested division/subtraction (a / b / c or a - b - c)
#   if (this_op %in% c("/", "-")) {
#     if (inherits(node$lhs, "expression") && node$lhs$op == this_op) return(TRUE)
#     if (inherits(node$rhs, "expression") && node$rhs$op == this_op) return(TRUE)
#   }
#
#   # Case 3: recurse children indicate need
#   return(lhs_brackets || rhs_brackets)
# }

needs_brackets <- function(node, parent = NULL) {
  # browser()
  if (!inherits(node, "expression")) {
    # Structural nodes like sum, prod may or may not need brackets depending on parent
    if (inherits(parent, c("sum", "prod", "when"))) {
      if (inherits(node, "expression")) return(TRUE)
      if (inherits(node, c("sum", "prod"))) return(FALSE)
      return(FALSE)
    }
    return(FALSE)
  }

  if (is.null(parent)) return(FALSE)

  parent_op <- if (inherits(parent, "expression")) parent$op else NULL

  # Precedence rules
  this_op <- node$op
  this_prec <- operator_precedence[[this_op]] %||% Inf
  # parent_prec <- operator_precedence[[parent_op]] %||% -Inf
  parent_prec <- if (isTRUE(parent_op %in% names(operator_precedence))) {
      operator_precedence[[parent_op]]
    } else {-Inf}

  # Recursively assess children
  lhs_brackets <- if (!is.null(node$lhs)) needs_brackets(node$lhs, node) else FALSE
  rhs_brackets <- if (!is.null(node$rhs)) needs_brackets(node$rhs, node) else FALSE

  # Case 1: Parent structure that usually requires brackets around expressions
  if (inherits(parent, c("sum", "prod", "when"))) {
    if (inherits(node, "expression")) return(TRUE)
    if (inherits(node, c("sum", "prod"))) return(FALSE)
  }

  # Case 2: this expression is inside a higher precedence context (e.g., a+b in a*b)
  if (this_prec > parent_prec) return(TRUE)

  # Case 3: nested division or subtraction (e.g., a / b / c)
  if (this_op %in% c("/", "-")) {
    if (inherits(node$lhs, "expression") && node$lhs$op == this_op) return(TRUE)
    if (inherits(node$rhs, "expression") && node$rhs$op == this_op) return(TRUE)
  }

  # Case 4: Child expressions indicate brackets are needed
  return(lhs_brackets || rhs_brackets)
}



contains_loose_operator <- function(node) {
  if (!inherits(node, "expression")) return(FALSE)
  op <- node$op
  prec <- operator_precedence[[op]]
  if (!is.null(prec) && prec > 2) return(TRUE)
  lhs <- if (!is.null(node$lhs)) contains_loose_operator(node$lhs) else FALSE
  rhs <- if (!is.null(node$rhs)) contains_loose_operator(node$rhs) else FALSE
  return(lhs || rhs)
}


#' @export
is_associative <- function(op) {
  op %in% c("+", "*", "and", "or", "&", "|")
}

#' Annotate expression tree with bracket requirements
#'
#' @export
annotate_brackets <- function(expr, parent = NULL, side = NULL) {
  if (!inherits(expr, "expression")) return(expr)

  # Determine bracketing need
  bracket_status <- needs_brackets(expr, parent = parent, side = side)
  expr$brackets <- bracket_status

  # Recurse into subexpressions
  if (!is.null(expr$lhs)) {
    expr$lhs <- annotate_brackets(expr$lhs, parent = expr, side = "lhs")
  }
  if (!is.null(expr$rhs)) {
    expr$rhs <- annotate_brackets(expr$rhs, parent = expr, side = "rhs")
  }

  expr
}
