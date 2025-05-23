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



#' Identify top-level LaTeX operators in an expression
#'
#' @param latex_str A LaTeX math string
#' @param operators Vector of operators to detect at top level
#'
#' @return Data frame of matched operators and positions
#' @export
latex_top_level_operators <- function(latex_str,
                                     operators = c("+", "-", "\\\\cdot", "\\\\div", "=")) {
  chars <- strsplit(latex_str, "")[[1]]
  positions <- list()
  i <- 1
  n <- length(chars)
  depth <- 0
  cmd_mode <- FALSE
  env_stack <- character()

  while (i <= n) {
    ch <- chars[i]

    # -- Handle LaTeX command --
    if (ch == "\\") {
      j <- i + 1
      cmd <- ch
      while (j <= n && grepl("[a-zA-Z*]", chars[j])) {
        cmd <- paste0(cmd, chars[j])
        j <- j + 1
      }

      if (cmd %in% c("\\left", "\\bigl", "\\Bigl", "\\biggl", "\\Biggl")) {
        env_stack <- c(env_stack, cmd)
      } else if (cmd %in% c("\\right", "\\bigr", "\\Bigr", "\\biggr", "\\Biggr")) {
        if (length(env_stack)) env_stack <- head(env_stack, -1)
      } else if (cmd %in% c("\\sum", "\\prod", "\\frac", "\\mathbb", "\\mathcal", "\\mathsf")) {
        # skip over braces like \frac{a}{b}
        i <- j
        next
      } else if (cmd %in% operators && depth == 0 && length(env_stack) == 0) {
        positions[[length(positions) + 1]] <- list(op = cmd, pos = i)
      }

      i <- j
      next
    }

    # -- Bracket depth tracking --
    if (ch %in% c("{", "[", "(", "<")) {
      depth <- depth + 1
    } else if (ch %in% c("}", "]", ")", ">")) {
      depth <- max(depth - 1, 0)
    }

    # -- Plain operators (+, -, =), allowed only at depth = 0
    if (depth == 0 && length(env_stack) == 0 && ch %in% c("+", "-", "=") && ch %in% operators) {
      positions[[length(positions) + 1]] <- list(op = ch, pos = i)
    }

    # -- Move next --
    i <- i + 1
  }

  # Return as data.frame
  if (length(positions)) {
    data.frame(
      op = sapply(positions, `[[`, "op"),
      pos = sapply(positions, `[[`, "pos"),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(op = character(0), pos = integer(0))
  }
}

#' Split LaTeX math string at top-level operators
#'
#' @param latex_str A LaTeX math string
#' @param operators Vector of operators to split at
#'
#' @return A character vector of expression chunks, including the operators
#' @export
split_top_level_operators <- function(latex_str,
                                         operators = c("+", "-", "\\\\cdot", "\\\\div", "=")) {
  op_locs <- latex_top_level_operators(latex_str, operators)
  if (nrow(op_locs) == 0) return(latex_str)

  result <- c()
  last_pos <- 1

  for (i in seq_len(nrow(op_locs))) {
    op_pos <- op_locs$pos[i]
    op_len <- nchar(op_locs$op[i])
    part <- substr(latex_str, last_pos, op_pos - 1)
    result <- c(result, trimws(part))
    result <- c(result, op_locs$op[i])
    last_pos <- op_pos + op_len
  }

  # Append final chunk
  if (last_pos <= nchar(latex_str)) {
    result <- c(result, trimws(substr(latex_str, last_pos, nchar(latex_str))))
  }

  return(result)
}
