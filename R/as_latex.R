
latex_operators <- list(
  "*" = " \\cdot ",
  "/" = " \\div ",
  # "/" = " \\frac",
  # "/" = "/",
  "+" = " + ",
  "-" = " - ",
  "^" = " ^ ",
  "**" = " ^ ",
  "=" = " = ",
  "<=" = " \\leq ",
  "LE" = " \\leq ",
  "le" = " \\leq ",
  "LT" = " < ",
  "lt" = " < ",
  ">=" = " \\geq ",
  "GE" = " \\geq ",
  "ge" = " \\geq ",
  "GT" = " > ",
  "gt" = " > ",
  "==" = " = ",
  "eq" = " = ",
  "EQ" = " = ",
  "!=" = " \\neq ",
  "<>" = " \\neq ",
  "ne" = " \\neq ",
  "NE" = " \\neq ",
  "<" = " < ",
  ">" = " > ",
  "and" = " \\land ",
  "AND" = " \\land ",
  "or"  = " \\lor ",
  "OR" = " \\lor ",
  "!" = " \\lnot ",
  "not" = " \\lnot ",
  "NOT" = " \\lnot "
)

escape_latex <- function(txt) {
  # Escape common LaTeX special characters
  txt <- gsub("\\\\", "\\\\textbackslash{}", txt)
  txt <- gsub("([#$%&_{}])", "\\\\\\1", txt)
  txt <- gsub("~", "\\\\textasciitilde{}", txt)
  txt <- gsub("\\^", "\\\\textasciicircum{}", txt)
  return(txt)
}

#' @export
latex_wrap_brackets <- function(x,
                                brackets = NULL,
                                autosize = TRUE,
                                math = FALSE, # [] only
                                content = x,
                                context = NULL,
                                ...) {
  if (is.null(x)) return(NULL)
  if (is.null(brackets) && !math) return(x)
  if (isFALSE(brackets)) return(x)

  if (math) {
    if (!is.null(brackets)) {
      brackets <- NULL
    }
    if (!autosize) {content <- NULL; context <- NULL}
    brackets <- latex_math_brakets(content, context)
    out <- paste(brackets[1], x, brackets[2])
    return(out)
  }

  brackets <- brackets_pair(brackets)
  if (brackets[1] == "[") {
    if (autosize) {
      out <- paste0("\\left[", x, "\\right]")
    } else {
      out <- paste0("[", x, "]")
    }
  } else if (brackets[1] == "{") {
    if (autosize) {
      out <- paste0("\\left\\{", x, "\\right\\}")
    } else {
      out <- paste0("{", x, "}")
    }
  } else if (brackets[1] == "(") {
    if (autosize) {
      out <- paste0("\\left(", x, "\\right)")
    } else {
      out <- paste0("(", x, ")")
    }
  } else {
    stop("Unsupported bracket type: ", brackets[1])
  }
  return(out)
}

#' Get effective dimensions for LaTeX rendering
#' @keywords internal
get_latex_dims <- function(x, use_folded = TRUE) {
  if (use_folded && !is.null(x$active_dims) && length(x$active_dims) > 0) {
    return(ensure_dims_object(x$active_dims))
  }

  if (!is.null(x$dims) && length(x$dims) > 0) {
    return(ensure_dims_object(x$dims))
  }

  if (!is.null(x$data) && nrow(x$data) > 0) {
    data_cols <- setdiff(names(x$data), "value")
    if (length(data_cols) > 0) {
      inferred_dims <- structure(vector("list", length(data_cols)), names = data_cols)
      return(ensure_dims_object(inferred_dims))
    }
  }

  return(ensure_dims_object(x$dims))
}

#' Format set elements for LaTeX display
#' @keywords internal
format_set_elements <- function(set_data,
                               max_inline = 10,
                               head_tail_threshold = 100,
                               head_n = 3,
                               tail_n = 3) {
  n_elem <- length(set_data)
  if (n_elem == 0) {
    return("(empty)")
  }

  safe_data <- gsub("_", "\\\\_", set_data)

  if (n_elem <= max_inline) {
    elem_text <- paste(safe_data, collapse = ", ")
    return(sprintf("(%d elements: %s)", n_elem, elem_text))
  }

  if (n_elem <= head_tail_threshold) {
    elem_text <- paste(c(head(safe_data, 5), "..."), collapse = ", ")
    return(sprintf("(%d elements: %s)", n_elem, elem_text))
  }

  if (head_n > 0 && tail_n > 0) {
    head_text <- paste(head(safe_data, head_n), collapse = ", ")
    tail_text <- paste(tail(safe_data, tail_n), collapse = ", ")
    elem_text <- sprintf("%s, ..., %s", head_text, tail_text)
    return(sprintf("(%s elements: %s)", format(n_elem, big.mark = ","), elem_text))
  }

  sprintf("(%s elements)", format(n_elem, big.mark = ","))
}

#' Render LaTeX indices for extrema constructs
#' @keywords internal
render_extrema_index <- function(index_node, brackets = NULL, ...) {
  if (inherits(index_node, "dims")) {
    return(paste0(vapply(index_node, as_latex, character(1)), collapse = ", "))
  }

  if (inherits(index_node, "when")) {
    cond <- index_node$condition
    idx_latex <- paste0(
      vapply(index_node$then, as_latex, character(1)),
      collapse = ", ")

    if (inherits(cond, "where")) {
      set_name <- paste0("\\mathsf{", cond$name, "}")
      return(paste0(idx_latex, " \\in ", set_name))
    }

    if (inherits(cond, "mapping")) {
      dims <- cond$dims
      dims_latex <- as_latex(dims, ...)
      mapping_latex <- paste0("\\mathsf{", cond$name, "}_{", dims_latex, "}")
      return(paste0(idx_latex, " \\in ", mapping_latex))
    }

    cond_latex <- as_latex(cond, brackets = NULL, ...)
    return(paste0(idx_latex, " \\mid ", cond_latex))
  }

  as_latex(index_node, brackets = brackets, ...)
}

#' @returns A LaTeX bracket size prefix (e.g., "", "\\big", "\\Big", etc.)
#' @export
latex_bracket_size <- function(content, context = NULL) {
  # Strip LaTeX commands and brackets for rough length estimation
  content_clean <- gsub("\\\\[a-zA-Z]+|\\{|\\}|\\s+", "", content)

  # Count key heuristics
  n_chars <- nchar(content_clean)
  n_ops <- stringr::str_count(content, "\\+|\\-|\\\\cdot|\\\\sum|\\\\prod|\\\\frac")
  n_lines <- stringr::str_count(content, "\\\\\\\\")

  # Composite score
  score <- n_chars + 5 * n_ops + 10 * n_lines

  # Adjust based on context
  if (!is.null(context)) {
    if (context %in% c("sum", "prod")) score <- score + 10
  }

  # Choose bracket size
  bracket <- dplyr::case_when(
    score < 30 ~ "",           # normal
    score < 60 ~ "\\big",
    score < 90 ~ "\\Big",
    score < 130 ~ "\\bigg",
    TRUE ~ "\\Bigg"
  )

  return(bracket)
}

#' Generate LaTeX brackets for math expressions
#'
#' @param content A LaTeX string (the expression inside the brackets)
#' @param context Optional context string (e.g., "sum", "prod", or NULL)
#' @param size Optional size prefix for the brackets to pass
#' (e.g., `""`, `"\\big"`, `"\\Big"`, `"\\bigg"`, or `"\\Bigg"`)
#'
#' @returns A character vector with two elements: `open` and `close`
#' @export
latex_math_brakets <- function(content = NULL, context = NULL, size = NULL) {
  if (is.null(content) && is.null(size)) {
    size <- ""
  } else if (is.null(size)) {
    size <- latex_bracket_size(content, context)
  }
  if (is.null(size) || size == "") {
    return(c(open = "[", close = "]"))
  }
  c(open = paste0(size, "l["), close = paste0(size, "r]"))
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


#' Convert objects to LaTeX format
#'
#' @export
#' @rdname as_latex
as_latex <- function(x, ...) {
  UseMethod("as_latex")
}

#' @export
#' @method as_latex default
#' @rdname as_latex
as_latex.default <- function(x, ...) {
  if (is.null(x)) {return(NULL)}
  stop("as_latex not implemented for class '", class(x)[1], "', ",
       "value: ", as.character(x), call. = FALSE)
}

#' Convert a character string to LaTeX-safe format
#'
#' @param x A character string (or vector of strings) to be converted to LaTeX.
#' @param math Logical; if TRUE, wraps the result in math mode (`$...$`).
#' @param bold Logical; if TRUE, wraps the text in `\\textbf{...}`.
#' @param italic Logical; if TRUE, wraps the text in `\\textit{...}`.
#'
#' @return A character vector of LaTeX-safe strings.
#' @method as_latex character
#' @rdname as_latex
#' @export
#'
#' @examples
#' as_latex("alpha & beta_1 = 0.5%")
#' as_latex("theta", math = TRUE)
#' as_latex("Note:", bold = TRUE, italic = TRUE)
as_latex.character <- function(x, math = FALSE, bold = FALSE, italic = FALSE, ...) {
  if (!is.character(x)) stop("Input must be a character string.")

  escape_latex <- function(txt) {
    txt <- gsub("\\\\", "\\\\textbackslash{}", txt)
    txt <- gsub("([#%&$_{}])", "\\\\\\1", txt)
    txt
  }

  format_with_style <- function(txt) {
    if (bold) txt <- paste0("\\textbf{", txt, "}")
    if (italic) txt <- paste0("\\textit{", txt, "}")
    txt
  }

  sapply(x, function(s) {
    rel_pattern <- "(<=|>=|==)"
    match <- regexpr(rel_pattern, s, perl = TRUE)

    if (match[1] > 0) {
      op <- regmatches(s, match)
      parts <- strsplit(s, rel_pattern, perl = TRUE)[[1]]

      if (length(parts) == 2) {
        left  <- format_with_style(escape_latex(trimws(parts[1])))
        right <- format_with_style(escape_latex(trimws(parts[2])))
        latex_op <- switch(op,
                           "<=" = "\\le",
                           ">=" = "\\ge",
                           "==" = "=")

        if (math) {
          paste0("$", left, " ", latex_op, " ", right, "$")
        } else {
          paste0(left, " $", latex_op, "$ ", right)
        }
      } else {
        txt <- format_with_style(escape_latex(s))
        if (math) paste0("$", txt, "$") else txt
      }
    } else {
      txt <- format_with_style(escape_latex(s))
      if (math) paste0("$", txt, "$") else txt
    }
  }, USE.NAMES = FALSE)
}

#' @export
#' @method as_latex call
#' @rdname as_latex
as_latex.call <- function(x, brackets = NULL, ...) {
  is_ast_call <- is.list(x) && !is.null(x$name) && !is.null(x$args)
  fn <- if (is_ast_call) x$name else as.character(x[[1]])
  args <- if (is_ast_call) x$args else if (length(x) > 1) as.list(x[-1]) else list()

  render_arg <- function(arg) {
    if (is.null(arg)) {
      return("\\varnothing")
    }
    if (inherits(arg, "ast")) {
      return(as_latex(arg, brackets = NULL, ...))
    }
    if (is.call(arg)) {
      return(as_latex(arg, brackets = NULL, ...))
    }
    if (is.list(arg)) {
      pieces <- vapply(arg, render_arg, character(1))
      return(paste(pieces, collapse = ", "))
    }
    if (is.character(arg)) {
      return(escape_latex(paste(arg, collapse = " ")))
    }
    if (is.numeric(arg) || is.logical(arg)) {
      return(paste(format(arg, trim = TRUE, scientific = FALSE), collapse = ", "))
    }
    escape_latex(paste(deparse(arg), collapse = " "))
  }

  args_tex <- if (length(args)) vapply(args, render_arg, character(1)) else character(0)
  fn_tex <- escape_latex(fn)
  call_tex <- paste0(fn_tex, "(", paste(args_tex, collapse = ", "), ")")

  if (!is.null(brackets) && !identical(brackets, FALSE)) {
    call_tex <- latex_wrap_brackets(call_tex, brackets = brackets, ...)
  }

  call_tex
}


# as_latex.call <- function(x, brackets = NULL, ...) {
#   is_ast_call <- is.list(x) && !is.null(x$name) && !is.null(x$args)
#   fn <- if (is_ast_call) x$name else as.character(x[[1]])
#   args <- if (is_ast_call) x$args else if (length(x) > 1) as.list(x[-1]) else list()

#   render_arg <- function(arg) {
#     if (is.null(arg)) {
#       return("\\varnothing")
#     }
#     if (inherits(arg, "ast")) {
#       return(as_latex(arg, brackets = NULL, ...))
#     }
#     if (is.call(arg)) {
#       return(as_latex(arg, brackets = NULL, ...))
#     }
#     if (is.list(arg)) {
#       pieces <- vapply(arg, render_arg, character(1))
#       return(paste(pieces, collapse = ", "))
#     }
#     if (is.character(arg)) {
#       return(escape_latex(paste(arg, collapse = " ")))
#     }
#     if (is.numeric(arg) || is.logical(arg)) {
#       return(paste(format(arg, trim = TRUE, scientific = FALSE), collapse = ", "))
#     }
#     escape_latex(paste(deparse(arg), collapse = " "))
#   }

#   args_tex <- if (length(args)) vapply(args, render_arg, character(1)) else character(0)
#   fn_tex <- escape_latex(fn)
#   call_tex <- paste0(fn_tex, "(", paste(args_tex, collapse = ", "), ")")

#   if (!is.null(brackets) && !identical(brackets, FALSE)) {
#     call_tex <- latex_wrap_brackets(call_tex, brackets = brackets, ...)
#   }

#   call_tex
# }

# as_latex.character <- function(x, math = FALSE, bold = FALSE, italic = FALSE) {
#   if (!is.character(x)) stop("Input must be a character string.")
#
#   latex_txt_style <- function(txt) {
#     txt <- escape_latex(txt)
#     if (bold) txt <- paste0("\\textbf{", txt, "}")
#     if (italic) txt <- paste0("\\textit{", txt, "}")
#     if (math)  txt <- paste0("$", txt, "$")
#     return(txt)
#   }
#
#   sapply(x, function(s) latex_txt_style(s), USE.NAMES = FALSE)
# }

#' @export
#' @method as_latex set
#' @rdname as_latex
as_latex.set <- function(x, math_env = "text", ...) {
  # browser()
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    if (is.null(math_env)) return(name)
    # mathcal
    return(paste0("\\", math_env, "{", name, "}"))
  }
}

#' @export
#' @method as_latex dims
#' @rdname as_latex
as_latex.dims <- function(x, brackets = NULL, subscript_dims = NULL, ...) {
  # browser()
  if (is.null(x)) {
    return(x)
    # return("\\emptyset")
  }
  # Use for-loop to preserve class attributes on each element
  dim_strs <- character(length(x))
  for (i in seq_along(x)) {
    dim_strs[i] <- as_latex(x[[i]], ...)
  }
  dims <- paste(dim_strs, collapse = ",")
  dims <- latex_wrap_brackets(dims, brackets, ...)
  if (!is.null(brackets)) {
    # enforce mathcal for dims
    return(paste0("\\mathcal{", dims, "}"))
  }
  dims
}

#' @export
#' @method as_latex expression
#' @rdname as_latex
as_latex.mapping <- function(x, brackets = NULL,
                             subscript_dims = is.null(brackets), ...) {
  # return(NULL)
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    dims_obj <- get_latex_dims(x)
    if (is.null(dims_obj) || length(dims_obj) == 0) {
      dims <- ""
    } else {
      dims <- as_latex(dims_obj, brackets = brackets, sbscript_dims = subscript_dims, ...)
    }
    if (subscript_dims) {
      dims <- paste0("_{", dims, "}")
    } else {
      # dims <- paste0("(", dims, ")")
      dims <- latex_wrap_brackets(dims, brackets = brackets, ...)
    }
    return(paste0("\\mathit{", name, "}", dims, ""))
  }
}

#' @export
#' @method as_latex parameter
#' @rdname as_latex
as_latex.parameter <- function(x, brackets = NULL,
                              subscript_dims = is.null(brackets), 
                              use_index_aliases = FALSE,
                              model = NULL,
                              ...) {
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    dims_obj <- get_latex_dims(x)
    
    # Apply index alias mapping based on context
    if (!is.null(model) && !is.null(model$index_aliases)) {
      if (use_index_aliases) {
        # Forward mapping for equations: long -> short (e.g., REGION -> r, commp -> cp)
        forward_map <- as.list(model$index_aliases)
        if (!is.null(dims_obj) && length(dims_obj) > 0) {
          dims_obj <- alias_ast_names(dims_obj, alias_map = forward_map)
        }
      } else {
        # Reverse mapping for declarations: short -> long (e.g., r -> REGION, cp -> commp)
        reverse_map <- as.list(setNames(names(model$index_aliases), unname(unlist(model$index_aliases))))
        if (!is.null(dims_obj) && length(dims_obj) > 0) {
          dims_obj <- alias_ast_names(dims_obj, alias_map = reverse_map)
        }
      }
    }
    
    if (is.null(dims_obj) || length(dims_obj) == 0) {
      dims <- ""
    } else {
      dims <- as_latex(dims_obj, brackets = brackets,
                       subscript_dims = subscript_dims,
                       ...)
    }
    if (subscript_dims) {
      dims <- paste0("_{", dims, "}")
    } else {
      # dims <- paste0("(", dims, ")")
      dims <- latex_wrap_brackets(dims, brackets = brackets)
    }
    return(paste0("\\mathsf{", name, "}", dims, ""))
  }
}

#' @export
#' @method as_latex variable
#' @rdname as_latex
as_latex.variable <- function(x, brackets = NULL, subscript_dims = TRUE, 
                             use_index_aliases = FALSE,
                             model = NULL,
                             ...) {
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    dims_obj <- get_latex_dims(x)
    
    # Apply index alias mapping based on context
    if (!is.null(model) && !is.null(model$index_aliases)) {
      if (use_index_aliases) {
        # Forward mapping for equations: long -> short (e.g., REGION -> r, commp -> cp)
        forward_map <- as.list(model$index_aliases)
        if (!is.null(dims_obj) && length(dims_obj) > 0) {
          dims_obj <- alias_ast_names(dims_obj, alias_map = forward_map)
        }
      } else {
        # Reverse mapping for declarations: short -> long (e.g., r -> REGION, cp -> commp)
        reverse_map <- as.list(setNames(names(model$index_aliases), unname(unlist(model$index_aliases))))
        if (!is.null(dims_obj) && length(dims_obj) > 0) {
          dims_obj <- alias_ast_names(dims_obj, alias_map = reverse_map)
        }
      }
    }
    
    if (is.null(dims_obj) || length(dims_obj) == 0) {
      dims <- ""
    } else {
      dims <- as_latex(dims_obj, brackets = brackets, subscript_dims = subscript_dims, ...)
    }
    if (subscript_dims) {
      dims <- paste0("_{", dims, "}")
    } else {
      # dims <- paste0("(", dims, ")")
      dims <- latex_wrap_brackets(dims, brackets = brackets)
    }
    return(paste0("\\bm{\\mathit{", name, "}}", dims, ""))
  }
}

#' @export
#' @method as_latex symbol
#' @rdname as_latex
as_latex.symbol <- function(x, ...) {
  paste0("\\texttt{", x$name, "}")
}

#' @export
#' @method as_latex shift
#' @rdname as_latex
as_latex.shift <- function(x, brackets = NULL, ...) {
  base <- paste0("\\texttt{", x$symbol, "}")
  offset <- x$offset
  if (is.null(offset)) {
    offset <- 0
  }
  if (is.null(offset) || offset == 0) {
    shifted <- base
  } else {
    sign <- if (offset > 0) "+" else "-"
    shifted <- paste0(base, sign, abs(offset))
  }

  if (!is.null(brackets) && !identical(brackets, FALSE)) {
    shifted <- latex_wrap_brackets(shifted, brackets = brackets, ...)
  }

  shifted
}

#' @export
#' @method as_latex constant
#' @rdname as_latex
as_latex.constant <- function(x, ...) {
  as.character(x$value)
}

#' @export
#' @method as_latex unary
#' @rdname as_latex
as_latex.unary <- function(x, brackets = NULL, ...) {
  rhs <- as_latex(x$rhs, brackets = brackets, ...)
  op <- latex_operators[[x$op]]
  if (is.null(op)) {
    stop("Unrecognized operator: ", x$op)
  }
  out <- paste0(op, rhs)
  return(out)
}

#' @export
#' @method as_latex expression
#' @rdname as_latex
as_latex.expression <- function(x, brackets = NULL, ...) {
  # browser()
  lhs <- x$lhs
  rhs <- x$rhs

  if (!inherits(lhs, "ast")) {
    warning("lhs in expression is not an AST: fallback to str()")
    browser()
    lhs <- paste(capture.output(str(lhs)), collapse = "")
  } else {
    lhs <- as_latex(lhs, brackets, ...)
  }

  if (!inherits(rhs, "ast")) {
    warning("rhs in expression is not an AST: fallback to str()")
    browser()
    rhs <- paste(capture.output(str(rhs)), collapse = "")
  } else {
    rhs <- as_latex(rhs, brackets, ...)
  }

  op <- latex_operators[[x$op]]
  if (is.null(op)) stop("Unknown operator: ", x$op)

  # Check if the operator requires {}
  if (trimws(op) %in% c("^")) {
    rhs <- latex_wrap_brackets(rhs, "{}", math = FALSE, autosize = FALSE)
    out <- paste0(lhs, " ", op, " ", rhs)
  } else if (trimws(op) %in% "\\frac") {
    lhs <- latex_wrap_brackets(lhs, "{}", math = FALSE, autosize = FALSE)
    rhs <- latex_wrap_brackets(rhs, "{}", math = FALSE, autosize = FALSE)
    out <- paste0(op, lhs, rhs)
  } else {
    out <- paste0(lhs, " ", op, " ", rhs)
  }

  # browser()
  if (isTRUE(x$brackets)) {
    out <- latex_wrap_brackets(out, math = TRUE, autosize = TRUE,
                               content = out, context = x$op)
  } else if (!is_empty(brackets) && !isFALSE(brackets)) {
    out <- latex_wrap_brackets(out, math = TRUE, autosize = TRUE,
                               content = out, context = x$op)
  }

  return(out)

}

#' @export
#' @method as_latex when
#' @rdname as_latex
as_latex.when <- function(x,
                          brackets = NULL,
                          use_indicator = getOption("multimod.latex.use_indicator", FALSE),
                          indicator_symbol = getOption("multimod.latex.indicator_symbol", "\\delta"),
                          ...) {
  cond <- x$condition
  then <- x$then

  cond_latex <- as_latex(cond, brackets = NULL, ...)
  then_latex <- as_latex(then, brackets = brackets, ...)

  # is_compound <- inherits(then, c("expression", "sum", "prod"))
  # if (is_compound) {
  #   then_latex <- latex_wrap_brackets(then_latex, brackets = NULL,
  #                                     math = TRUE, autosize = TRUE,
  #                                     content = then_latex,
  #                                     context = class(then)[1], ...)
  # }

  # --- Option 1: Indicator notation ---
  if (use_indicator) {
    indicator <- paste0(" \\cdot ", indicator_symbol, "_{", cond_latex, "}")
    return(paste0(then_latex, indicator))
  }

  # --- Option 2: Structured mapping form ---
  dims_tex <- NULL
  mapping_latex <- NULL

  if (inherits(cond, "mapping") && !is.null(cond$dims)) {
    dim_names <- character(length(cond$dims))
    for (i in seq_along(cond$dims)) {
      dim_names[i] <- cond$dims[[i]]$name
    }
    dims_tex <- paste0(
      "\\left\\{\\textnormal{",
      paste(dim_names, collapse = ","),
      "}\\right\\}"
    )
    subscript <- as_latex(cond$dims, brackets = NULL, ...)
    mapping_latex <- paste0("\\mathsf{", cond$name, "}_{", subscript, "}")
  }

  if (inherits(cond, "where") && inherits(cond$content, "mapping")) {
    dim_names <- character(length(cond$content$dims))
    for (i in seq_along(cond$content$dims)) {
      dim_names[i] <- cond$content$dims[[i]]$name
    }
    dims_tex <- paste0(
      "\\textnormal{",
      paste(dim_names, collapse = ","),
      "}"
    )
    mapping_latex <- paste0("\\mathsf{", cond$name, "}")
  }

  # Final format using: [ expr, {tuple} ∈ mapping ]
  if (!is.null(dims_tex) && !is.null(mapping_latex)) {
    # browser()
    if (length(cond$content$dims) > 1 || length(cond$dims) > 1) {
      dims_tex <- latex_wrap_brackets(dims_tex, brackets = "{}")
    }
    return(
      paste0(
      "\\left[", then_latex, "\\mid ",
      dims_tex, " \\in ", mapping_latex,
      "\\right]"
    ))
  }

  # --- Legacy fallback ---
  return(paste0(then_latex, " \\mid ", cond_latex))
}

#' @export
#' @method as_latex sum
#' @rdname as_latex
as_latex.sum <- function(x, brackets = NULL, ...) {
  index_node <- x$index
  body <- as_latex(x$value, brackets = brackets, ...)

  index_latex <- ""

  if (inherits(index_node, "dims")) {
    # Use names of dims elements if available (iterator variables like r, y)
    # Otherwise render the dims elements themselves (set names)
    idx_names <- names(index_node)
    if (!is.null(idx_names) && length(idx_names) > 0 && all(nzchar(idx_names))) {
      # Use iterator variable names
      idx_strs <- paste0("\\texttt{", idx_names, "}")
      index_latex <- paste0(idx_strs, collapse = ", ")
    } else {
      # Fall back to rendering dims elements
      idx_strs <- character(length(index_node))
      for (i in seq_along(index_node)) {
        idx_strs[i] <- as_latex(index_node[[i]])
      }
      index_latex <- paste0(idx_strs, collapse = ", ")
    }

  } else if (inherits(index_node, "when")) {
    cond <- index_node$condition
    then_strs <- character(length(index_node$then))
    for (i in seq_along(index_node$then)) {
      then_strs[i] <- as_latex(index_node$then[[i]])
    }
    idx_latex <- paste0(then_strs, collapse = ", ")

    if (inherits(cond, "where")) {
      # Named condition: i ∈ \mathsf{m4}
      set_name <- paste0("\\mathsf{", cond$name, "}")
      index_latex <- paste0(idx_latex, " \\in ", set_name)

    } else if (inherits(cond, "mapping")) {
      dims <- cond$dims
      dims_latex <- as_latex(dims, ...)
      mapping_latex <- paste0("\\mathsf{", cond$name, "}_{", dims_latex, "}")
      index_latex <- paste0(idx_latex, " \\in ", mapping_latex)

    } else {
      # General condition fallback
      cond_latex <- as_latex(cond, brackets = NULL, ...)
      index_latex <- paste0(idx_latex, " \\mid ", cond_latex)
    }

  } else {
    # fallback
    index_latex <- as_latex(index_node, brackets = brackets, ...)
  }

  domain <- if (!is.null(x$domain)) {
    paste0(",\\; ", as_latex(x$domain, brackets = brackets, ...))
  } else {""}

  return(paste0("\\sum_{", index_latex, domain, "} ", body))
}

#' @export
#' @method as_latex prod
#' @rdname as_latex
as_latex.prod <- function(x, brackets = NULL, ...) {
  index <- as_latex(x$index, brackets = brackets, ...)
  domain <- if (!is.null(x$domain)) {
    paste0(",\\; ", as_latex(x$domain, brackets = brackets, ...))
   } else {""}
  body <- as_latex(x$value, brackets = brackets, ...)
  return(paste0("\\prod_{", index, domain, "} ", body))
}

#' @export
#' @method as_latex setmin
as_latex.setmin <- function(x, brackets = NULL, ...) {
  body <- as_latex(x$value, brackets = brackets, ...)
  body <- paste0("\\left(", body, "\\right)")
  paste0("\\min", body)
}

#' @export
#' @method as_latex setmax
as_latex.setmax <- function(x, brackets = NULL, ...) {
  body <- as_latex(x$value, brackets = brackets, ...)
  body <- paste0("\\left(", body, "\\right)")
  paste0("\\max", body)
}

#' @export
#' @method as_latex func
#' @rdname as_latex
as_latex.func <- function(x, brackets = NULL, subscript_dims = TRUE, ...) {
  if (isTRUE(x$name == "sum")) {
    x <- ast_func_to_sum(x)
    return(as_latex.sum(x, brackets = brackets, ...))
  } else if (isTRUE(x$name == "prod")) {
    x <- ast_func_to_prod(x)
    return(as_latex.prod(x, brackets = brackets, ...))
  } else if (isTRUE(x$name == "min")) {
    # Render min(expr) as \min\{expr\} for set minimum
    value_latex <- as_latex(x$value, brackets = brackets, subscript_dims = subscript_dims, ...)
    return(paste0("\\min\\left\\{", value_latex, "\\right\\}"))
  } else if (isTRUE(x$name == "max")) {
    # Render max(expr) as \max\{expr\} for set maximum
    value_latex <- as_latex(x$value, brackets = brackets, subscript_dims = subscript_dims, ...)
    return(paste0("\\max\\left\\{", value_latex, "\\right\\}"))
  }

  # browser()
  value_latex <- if (inherits(x$value, c("ast"))) {
      as_latex(x$value, brackets = brackets, subscript_dims = subscript_dims, ...)
    } else if (is.list(x$value)) {
      val_strs <- character(length(x$value))
      for (i in seq_along(x$value)) {
        v <- x$value[[i]]
        val_strs[i] <- if (inherits(v, "ast")) {
          as_latex(v, brackets = brackets, subscript_dims = subscript_dims, ...)
        } else {
          # Fallback to character conversion
          as.character(v)
        }
      }
      val_strs
    } else {
      # Single value, not a list
      as.character(x$value)
    }
  value_latex <- paste(value_latex, collapse = ", ")

  if (!is.null(x$index)) {
    index_latex <- as_latex(x$index, brackets = brackets, subscript_dims = subscript_dims, ...)
    return(paste0(x$name, "(", index_latex, ", ", value_latex, ")"))
  } else {
    return(paste0(x$name, "(", value_latex, ")"))
  }
}


# as_latex.func <- function(x, brackets = NULL, subscript_dims = TRUE, ...) {
#   stopifnot(inherits(x, "func"))
#   fun_name <- x$name
#   value_latex <- as_latex(x$value, brackets = FALSE, subscript_dims = subscript_dims, ...)
#
#   # Format function name and argument in LaTeX
#   fun_latex <- paste0("\\mathrm{", fun_name, "}(", value_latex, ")")
#
#   # Optionally wrap in brackets
#   if (!is.null(brackets) && brackets) {
#     fun_latex <- latex_wrap_brackets(fun_latex, brackets)
#   }
#
#   return(fun_latex)
# }


#' @export
#' @method as_latex where
as_latex.where <- function(x, inline_where = NULL, ...) {
  if (is.null(inline_where)) {
    inline_where <- getOption("multimod.render_where_inline", FALSE)
  }

  if (inline_where || is.null(x$content)) {
    return(as_latex(x$content, inline_where = inline_where, ...))
  }

  if (!is.null(x$name)) {
    out <- paste0("\\texttt{", x$name, "}")
  } else {
    out <- paste0("\\texttt{", substr(x$hash, 1, 8), "}")
  }
  out
}

#' @export
#' @method as_latex equation
as_latex.equation <- function(x,
                              # math_env = "equation",
                              brackets_dims = NULL,
                              subscript_dims = is.null(brackets_dims),
                              inline_where = NULL,
                              subsection_number = FALSE,
                              max_len = 100,
                              ...) {
  # browser()
  if (is.null(inline_where)) {
    inline_where <- getOption("multimod.render_where_inline", FALSE)
  }

  # Extract model from extra args for passing to nested calls
  extra_args <- list(...)

  # Render LHS and RHS with index aliases for equation context
  lhs <- as_latex(x$lhs, brackets = brackets_dims,
                  subscript_dims = subscript_dims,
                  inline_where = inline_where, 
                  use_index_aliases = TRUE,  # Use index aliases in equation bodies
                  ...)
  rhs <- as_latex(x$rhs, brackets = brackets_dims,
                  subscript_dims = subscript_dims,
                  inline_where = inline_where,
                  use_index_aliases = TRUE,  # Use index aliases in equation bodies
                  ...)
  rel <- switch(x$relation, `==` = "=", `<=` = "\\le", `>=` = "\\ge", x$relation)

  # Apply alignment formatting
  body <- format_latex_aligned(lhs = lhs, rel = rel, rhs = rhs,
                               max_len = max_len)

  body <- replace_mapping_placeholders(body)

  # Equation name and descriptor
  preamble <- character()

  # browser()
  # Construct preamble for equation output
  # Use dims_index_aliases if available (iterator vars like r,l,f,y)
  # Otherwise fall back to model$index_aliases if provided in ...
  # Finally fall back to dims (set names like REGION, TIMESLICE)
  extra_args <- list(...)
  if (!is.null(x$dims_index_aliases) && length(x$dims_index_aliases) > 0) {
    # Create dims from iterator variable names from equation
    eq_dims <- ast_dims(unname(x$dims_index_aliases))
  } else if (!is.null(extra_args$model) && !is.null(extra_args$model$index_aliases)) {
    # Fall back to model-level index_aliases
    model_aliases <- extra_args$model$index_aliases
    # Get equation dims, could be from domain (mapping) or direct dims
    eq_dim_names <- if (!is.null(x$domain) && inherits(x$domain, "mapping")) {
      # If domain is a mapping, use its dims
      if (!is.null(x$domain$dims)) names(x$domain$dims) else character()
    } else if (!is.null(x$dims)) {
      names(x$dims)
    } else {
      character()
    }
    if (length(eq_dim_names) > 0) {
      # Map through index_aliases: handles both direct matches and set aliases
      # e.g., REGION->r, commp->cp (where commp is a set alias that has index alias cp)
      mapped_dims <- sapply(eq_dim_names, function(d) {
        if (d %in% names(model_aliases)) {
          model_aliases[[d]]
        } else {
          d
        }
      }, USE.NAMES = FALSE)
      eq_dims <- ast_dims(mapped_dims)
    } else {
      eq_dims <- NULL
    }
  } else {
    eq_dims <- get_latex_dims(x)
  }
  
  if (is.null(eq_dims) || length(eq_dims) == 0) {
    dims_latex <- ""
  } else {
    dims_latex <- as_latex(eq_dims, brackets = NULL, subscript_dims = TRUE, ...)
  }
  # Render domain/mapping with index aliases for equation context
  mapping_latex <- as_latex(x$domain, brackets = NULL, subscript_dims = TRUE, 
                            use_index_aliases = TRUE, ...)

  preamble <- character()
  subsection <- if (subsection_number) "subsection" else "subsection*"

  desc_text <- sanitize_description(x$desc, x$name)
  header <- paste0("\\textbf{", as_latex(x$name), "}")
  if (nzchar(desc_text)) {
    header <- paste0(header, " -- \\textit{", as_latex(desc_text), "}")
  }
  preamble <- c(preamble, paste0("\\", subsection, "{", header, "}"))
  # dims and mapping
  # preamble <- c(
    # preamble,
    # paste0("\\quad$\\textbf{", as_latex(x$name), "}_{", dims_latex, "}$"),
    # paste0("$\\mid\\left\\{", dims_latex,"\\right\\} \\in ", mapping_latex, "$ \\\\")
  # )
  preamble <- c(preamble, paste0("\\quad$\\textbf{", as_latex(x$name), "}"))
  if (nchar(dims_latex) > 0) {
    preamble <- c(preamble, paste0("_{", dims_latex, "}$"))
  } else {
    preamble[length(preamble)] <- paste0(preamble[length(preamble)], "$")
  }
  if (!is.null(mapping_latex)) {
    preamble <- c(
      preamble,
      paste0("$\\mid\\left\\{", dims_latex,"\\right\\} \\in ",
             mapping_latex, "$ \\\\"))
  }

  # Prepare where: block if needed
  where_lines <- character()
  wh_len <- integer(0)
  where_cols <- 1
  # browser()
  if (!inline_where) {
    where_map <- extract_where_nodes(x)
    # max lenth of where_map
    # wh_len <- sapply(where_map, function(x) estimate_latex_length(as_latex(x, ...)))
    if (length(where_map) > 0) {
      where_lines <- character()
      for (nm in names(where_map)) {
        def <- as_latex(where_map[[nm]]$content, inline_where = TRUE, ...)
        # browser()
        # def <- replace_mapping_placeholders(def)
        # nm_greek <- replace_mapping_placeholders(nm)
        where_lines <- c(
          where_lines,
          # paste0("\\hspace*{2em}$\\texttt{", nm, "} = ", def, "$ \\"))
          replace_mapping_placeholders(
            paste0("$\\texttt{", nm, "} := ", def, "$ \\\\")
          ))
      }
      wh_len <- sapply(where_lines, function(x) estimate_latex_length(x))
      where_cols <- max(1, round(90 / (max(wh_len) + 4)))
      where_cols <- min(where_cols, length(wh_len))
      if (where_cols > 1) {
        where_lines <- c(#"\\textbf{where:}",
                         # "\\begin{flushleft}",
                         # "\\begin{multicols}{0}",
                         paste0("\\begin{multicols}{", where_cols, "}"),
                         where_lines,
                         # "\\end{flushleft}",
                         "\\end{multicols}")
      }
      where_lines <- c("\\texttt{where:} \\\\", where_lines)

      # where_lines <- c("\\textbf{where:}",
      #                  # "\\begin{flushleft}",
      #                  # "\\begin{multicols}{0}",
      #                  paste0("\\begin{multicols}{", where_cols, "}"),
      #                  where_lines,
      #                  # "\\end{flushleft}",
      #                  "\\end{multicols}")
    }
    # browser()
  }

  # Final output
  out <- c(
    "\\begin{flushleft}",
    paste(preamble, collapse = " \n"),
    "\\begin{equation*}",
    "\\begin{adjustbox}{max width=\\textwidth}",
    # paste0("\\begin{adjustbox}{", where_cols, "}"),
    "$\\begin{aligned}",
    # begin_math_env(math_env),
    body,
    # end_math_env(math_env),
    "\\end{aligned}$",
    "\\end{adjustbox}",
    "\\end{equation*}",
    where_lines,
    "\\vspace{1em}",
    "\\end{flushleft}"
  )

  paste(out, collapse = "\n")
}

# Helper: Collapse multiple LaTeX environments (string or character vector)
begin_math_env <- function(envs) {
  if (is.null(envs)) return("")
  paste(paste0("\\begin{", envs, "}"), collapse = "\n")
}

end_math_env <- function(envs) {
  if (is.null(envs)) return("")
  paste(rev(paste0("\\end{", envs, "}")), collapse = "\n")
}

# format functions ####

#' Estimate the length of a LaTeX string
#' @export
estimate_latex_length <- function(latex_str) {
  if (is.null(latex_str) || !nzchar(latex_str)) return(0)
  # browser()

  # Strip formatting wrappers
  s <- gsub("\\\\text(normal|bf|it)?\\{([^}]*)\\}", "\\2", latex_str)
  s <- gsub("\\\\math(it|bf|sf|cal)\\{([^}]*)\\}", "\\2", s)

  # Approximate cost of math constructs
  s <- gsub("_\\{[^}]*\\}", "~~", s)  # subscripts
  s <- gsub("\\\\frac\\{[^}]*\\}\\{[^}]*\\}", "~~~~~~", s)  # fraction
  s <- gsub("\\\\sum(_\\{[^}]*\\})?", "~~~~~", s)  # sum with or without subscript
  s <- gsub("\\\\prod(_\\{[^}]*\\})?", "~~~~~", s)  # prod

  # Remove other commands like \cdot, \left, \right, etc.
  s <- gsub("\\\\[a-zA-Z]+", "", s)

  # Final approximation
  nchar(s)
}

format_index <- function(lhs) {
  dims <- lhs$dims
  if (is.null(dims) || length(dims) == 0) return("")

  dims_str <- as_latex(dims)
  paste0("_{", dims_str, "}")
}


#' Format a LaTeX equation across multiple lines using aligned
#'
#' @param lhs LaTeX string of the left-hand side
#' @param rhs LaTeX string of the right-hand side
#' @param rel Relational operator (e.g., `=`, `\le`, `\ge`)
#' @param max_len Maximum allowed line length before splitting
#'
#' @return Character string of the formatted LaTeX code
# @export
format_latex_aligned <- function(lhs, rhs, rel = "=", max_len = 90) {
  # browser()
  lhs_len <- estimate_latex_length(lhs)
  rhs_len <- estimate_latex_length(rhs)
  full_len <- lhs_len + nchar(rel) + rhs_len

  if (full_len <= max_len) {
    return(paste0("&", lhs, " ", rel, " ", rhs, " \\\\"))
  }

  if (lhs_len < max_len / 2) {
    lhs <- paste0(lhs, " ", rel)
    rel <- ""
  }


  lines <- paste0("&", lhs, " \\\\")
  # LHS is too long, split it
  if (lhs_len >= max_len) {
    lhs_parts <- split_at_top_level_operators(lhs, max_len = max_len - 10)
    # lines <- character()
    # lines[1] <- paste0("&", lhs, " \\\\")
    # lines[2] <- paste0("&\\qquad", rel, " ", rhs_parts[1], " \\")
    if (length(lhs_parts) > 1) {
      lines <- paste0("&", lhs_parts[1], " \\\\")
      for (i in 2:length(lhs_parts)) {
        lines <- c(lines, paste0("&\\qquad\\qquad ", lhs_parts[i], " \\\\") )
      }
    } else {
      # unsuccessful split
      lines <- paste0("&", lhs_parts[1], " \\\\")
    }
  }

  rhs_max_len <- max_len - nchar(rel) - 4
  if (rhs_len >= rhs_max_len) {
    rhs_parts <- split_at_top_level_operators(
      rhs,
      max_len = rhs_max_len
    )
    # lines[1] <- paste0("&", lhs, " ", rel, " ", rhs_parts[1], " \\")
    if (length(rhs_parts) > 1) {
      lines_rhs <- paste0("&\\qquad\\qquad ", rel, " ", rhs_parts[1], " \\\\")
      for (i in 2:length(rhs_parts)) {
        lines_rhs[i] <- paste0("&\\qquad\\qquad ", rhs_parts[i], " \\\\")
      }
    } else {
      # unsuccessful split
      lines_rhs <- paste0("&\\qquad\\qquad ", rel, " ", rhs_parts[1], " \\\\")
    }
  } else {
    lines_rhs <- paste0("&\\qquad\\qquad ", rel, " ", rhs, " \\\\")
  }

  # Combine LHS and RHS lines
  return(paste(c(lines, lines_rhs), collapse = "\n"))
}

#' Split LaTeX math string at top-level operators
#'
#' This function splits a LaTeX math string at given types of top-level
#' operators and the maximum length of each chunk. The splitting is
#' done recursively with respect to the operator order.
#'
#' @param latex_str A LaTeX math string
#' @param operators Vector of operators to split at
#'
#' @return A character vector of expression chunks
#' @export
split_at_top_level_operators <- function(
    latex_str,
    operators = c("+", "-", "\\cdot", "\\div", "="),
    indent_str = character(0),
    # indent_str = "&\\quad",
    max_len = 80
    ) {
  # browser()
  if (is.null(latex_str) || !nzchar(latex_str)) return(NULL)
  if (is.null(operators) || length(operators) == 0) return(latex_str)
  if (estimate_latex_length(latex_str) <= max_len) return(latex_str)

  for (op in operators) {
    # Split at top-level operator
    op_locs <- latex_top_level_operators(latex_str, op)
    if (nrow(op_locs) > 0) {
      # Split at the first operator location where <= max_len
      op_locs$len <- sapply(
        1:nrow(op_locs),
        function(i) estimate_latex_length(substr(latex_str, 1, op_locs$pos[i]))
        )
      ii <- op_locs$len <= max_len
      if (any(ii)) {
        op_locs <- op_locs[1:nrow(op_locs) == which.max(op_locs$pos[ii]), ]
      } else {
        op_locs <- op_locs[1, ]
      }
      part1 <- substr(latex_str, 1, op_locs$pos[1] - 1)
      part_op <- op_locs$op[1]
      part2 <- substr(latex_str, op_locs$pos[1] + nchar(part_op), nchar(latex_str))
      # Check if part2 is too long
      if (estimate_latex_length(part2) <= max_len) {
        out <- c(trimws(part1), paste(indent_str, part_op, part2))
        return(out)
      } else {
        # Split part2 it recursively
        part2 <- split_at_top_level_operators(
          part2,
          operators = op,
          indent_str = indent_str,
          max_len = max_len
        )
        out <- c(trimws(part1), paste(indent_str, part_op, part2[1]), part2[-1])
        return(out)
      }
    }
  }
  return(latex_str)
}

#' Replace mapping placeholders like \texttt{m1} with styled math symbols
#'
#' @param latex_str A character vector of LaTeX lines.
#' @param style One of "mathcal", "psi", "phi", or "custom".
#' @param base Optional base symbol for style = "custom" (e.g., "M").
#' @return Updated LaTeX character vector.
#' @noRd
replace_mapping_placeholders <- function(latex_str,
                                         mathcal = FALSE,
                                         symbol = ifelse(mathcal, "M", "psi")
                                         # style = "mathcal",
                                         # base = NULL
                                         ) {
  repl_fun <- if (mathcal) {
    function(i) paste0("\\\\mathcal{", symbol, "}_{", i, "}")
  } else {
    function(i) paste0("\\\\", symbol, "_{", i, "}")
  }


  # browser()
  for (i in 1:99) {
    # pattern <- paste0("\\mathsf\\{m", i, "\\}")
    pattern <- paste0("\\\\(texttt|mathsf)\\{m", i, "\\}")
    # replacement <- switch(style,
    #                       mathcal = paste0("\\\\mathcal{M}_{", i, "}"),
    #                       psi     = paste0("\\\\psi_{", i, "}"),
    #                       phi     = paste0("\\\\varphi_{", i, "}"),
    #                       custom  = paste0("\\\\mathcal{", base %||% "X", "}_{", i, "}"),
    #                       stop("Unsupported style")
    # )
    latex_str <- gsub(pattern, repl_fun(i), latex_str, fixed = FALSE)
  }
  # browser()
  latex_str
}

