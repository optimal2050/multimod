# Functions to parse expression string into tokens and then into an AST
# Mostly for internal use

# Operator precedence with symbolic alternatives
operator_precedence <- c(
  "**" = 1, "^" = 1,
  "*" = 2, "/" = 2,
  "+" = 3, "-" = 3,
  "==" = 4, "!=" = 4, "<>" = 4, "<=" = 4, ">=" = 4, "=" = 4, "<" = 4, ">" = 4,
  "and" = 7, "&" = 7,
  "or"  = 6, "|" = 6,
  "not" = 5, "!" = 5
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
get_expression_level <- function(
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
find_top_level_operators <- function(
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
    expr_lev <- get_expression_level(expr, as_data_frame = TRUE)
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
      # warning("Unrecognized operator: ", op)
      fixed <- TRUE
      op_sp <- op
    }
    # browser()
    op_nn <- gregexpr(op_sp, expr, fixed = fixed, ignore.case = !fixed)[[1]]
    if (op_nn[1] == -1) next
    expr_lev$op[op_nn] <- op
    expr_lev$pos[op_nn] <- op_nn
    # expr_lev$prec[op_nn] <- operator_precedence[[op]] %||% NA_integer_
    expr_lev$prec[op_nn] <- get_operator_precedence(op, precedence = precedence)
    # if (!is.null(descending)) {
    #   expr_lev$prec[op_nn] <- sort_by_precedence(op, descending = descending)
    # }

    # replace matched positions in expr with white space
    expr <- gsub(op_sp, " ", expr, fixed = fixed, ignore.case = !fixed)

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
    expr_lev <- get_expression_level(expr, as_data_frame = TRUE)
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
    } else if (grepl("^[a-zA-Z]+$", op, ignore.case = TRUE)) {
      fixed <- FALSE
      # add white space around the op
      op_sp <- paste0("\\b", op, "\\b")
    } else {
      stop("Invalid/Unrecognized operator: ", op)
    }

    # check if op is in the expression
    op_nn <- gregexpr(op_sp, expr, fixed = fixed, ignore.case = !fixed)[[1]]
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
  # browser()
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
      if (i == 1) { # start of new token
        ntoken <- ntoken + 1
        token_start <- i
      } else if (expr_lev$split[i - 1] && i < length(expr_lev$char)) {
        # start of new token after an operator/split
        # stopifnot(ntoken > 0)
        ntoken <- ntoken + 1
        token_start <- i
      } else if (i == length(expr_lev$char)) { # end of last token
        stopifnot(ntoken > 0)
        if (expr_lev$split[i - 1]) { # new one
          ntoken <- ntoken + 1
          token_start <- i
        }
        tokens[[ntoken]] <- paste0(
          expr_lev$char[token_start:i],
          collapse = ""
        )
      } else {
        # continue building the current token
        next
      }
    }
  }

  if (as_list) {
    return(tokens)
  }

  return(unlist(tokens))
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


# Generic function parser
parse_function_expr <- function(expr, symbols,
                                known_funcs, known_funcs_indexed, ...) {
  # browser()
  # Extract function name (before first parenthesis)
  fn_name <- sub("\\(.*", "", expr)
  fn_name_lower <- tolower(fn_name)

  # Determine function type
  is_indexed <- fn_name_lower %in% tolower(known_funcs_indexed)
  is_known <- fn_name_lower %in% tolower(known_funcs)

  if (!(is_indexed || is_known)) return(NULL)

  if (is_indexed) {
    # Indexed function: func(index, value)
    # browser()
    parts <- split_indexed_operator(expr, func_name = fn_name)
    if (is.null(parts) || length(parts) != 2) return(NULL)

    index <- parse_gams_expr(parts[[1]], symbols = symbols,
                             # known_funcs = known_funcs,
                             # known_funcs_indexed =known_funcs_indexed,
                             ...)
    if (!inherits(index, "ast")) index <- ast_dims(index)

    value_expr <- parse_gams_expr(parts[[2]], symbols = symbols,
                                  # known_funcs = known_funcs,
                                  # known_funcs_indexed = known_funcs_indexed,
                                  ...)
    # browser()
    return(ast_func(fn_name, index = index, value = value_expr))

  } else {
    # browser()
    # Scalar function: func(arg1, arg2, ...)
    # Extract string inside the outermost parentheses
    args_str <- sub("^[^(]*\\((.*)\\)$", "\\1", expr)
    args_raw <- split_top_level_args(args_str)
    parsed_args <- lapply(args_raw, function(a) parse_gams_expr(a, symbols, known_funcs, ...))
    return(ast_func(fn_name, value = parsed_args))

    # args_raw <- split_top_level_args(expr)
    # args_parsed <- lapply(args_raw, function(a) parse_gams_expr(a, symbols, known_funcs, known_funcs_indexed, ...))
    # return(ast_func(fn_name, value = args_parsed))
  }
}

#' Split function arguments at the top level
#'
#' This function splits an argument string like `"x, y+z, f(a,b), g(i)$c(i)"` into
#' individual arguments while respecting nested parentheses.
#'
#' @param expr_str A string containing comma-separated expressions
#' @return A character vector of top-level arguments
split_top_level_args <- function(expr_str) {
  # browser()
  chars <- strsplit(expr_str, "")[[1]]
  level <- 0
  start <- 1
  args <- c()
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "(") {
      level <- level + 1
    } else if (ch == ")") {
      level <- level - 1
    } else if (ch == "," && level == 0) {
      arg <- substring(expr_str, start, i - 1)
      args <- c(args, trimws(arg))
      start <- i + 1
    }
  }
  # Add the last argument
  if (start <= length(chars)) {
    arg <- substring(expr_str, start)
    args <- c(args, trimws(arg))
  }
  return(args)
}


# # Generic function parser logic
# parse_function_expr <- function(expr, symbols, known_funcs, known_funcs_indexed, ...) {
#   # Extract function name
#   fn_name <- sub("\\(.*", "", expr)
#
#   # If not a known function, exit
#   if (!(tolower(fn_name) %in% c(tolower(known_funcs), tolower(known_funcs_indexed)))) {
#     return(NULL)
#   }
#
#   # Split arguments (with support for indexed form: func(index, value))
#   parts <- split_indexed_operator(expr)
#   if (is.null(parts)) {
#     # Scalar function fallback: func(arg1, arg2, ...)
#     args <- split_top_level_args(expr)
#     parsed_args <- lapply(args, function(a) parse_gams_expr(a, symbols, known_funcs, ...))
#     return(ast_func(fn_name, parsed_args))
#   }
#
#   # Indexed function: func(index, value)
#   index <- parse_gams_expr(parts[1], symbols, known_funcs, ...)
#   if (!inherits(index, "ast")) {
#     index <- ast_dims(index)
#   }
#   value_expr <- parse_gams_expr(trimws(parts[2]), symbols, known_funcs, ...)
#
#   return(ast_func(fn_name, index = index, value = value_expr))
# }

