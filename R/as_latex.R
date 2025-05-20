
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
  ">=" = " \\geq ",
  "==" = " = ",
  "!=" = " \\neq ",
  "<>" = " \\neq ",
  "<" = " < ",
  ">" = " > ",
  "and" = " \\land ",
  "or"  = " \\lor ",
  "!" = " \\lnot ",
  "not" = " \\lnot "
)

#' @export
latex_wrap_brackets <- function(x,
                                brackets = NULL,
                                autosize = TRUE,
                                math = FALSE, # [] only
                                content = x,
                                context = NULL,
                                ...) {
  if (is.null(x)) return(NULL)
  if (is.null(brackets) && !math) return(x) # no brackets

  # math brackets with optional autosize
  if (math) {
    if (!is.null(brackets)) {
      # warning("Brackets are ignored in math mode.")
      brackets <- NULL
    }
    if (!autosize) {content <- NULL; context <- NULL} # reset for default
    brackets <- latex_math_brakets(content, context)
    out <- paste(brackets[1], x, brackets[2])
    return(out)
  }
  # non-math brackets
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

#' Estimate bracket size for LaTeX based on context and expression content
#'
#' @param content A LaTeX string (the expression inside the brackets)
#' @param context Optional context string (e.g., "sum", "prod", or NULL)
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
  c(open = paste0(size, "l["), close = paste0(size, "r]"))
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
  browser()
  stop("as_latex not implemented for this class.")
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
as_latex.character <- function(x, math = FALSE, bold = FALSE, italic = FALSE) {
  if (!is.character(x)) stop("Input must be a character string.")

  escape_latex <- function(txt) {
    # Escape common LaTeX special characters
    txt <- gsub("\\\\", "\\\\textbackslash{}", txt)
    txt <- gsub("([#$%&_{}])", "\\\\\\1", txt)
    txt <- gsub("~", "\\\\textasciitilde{}", txt)
    txt <- gsub("\\^", "\\\\textasciicircum{}", txt)
    return(txt)
  }

  format_decorations <- function(txt) {
    if (bold) txt <- paste0("\\textbf{", txt, "}")
    if (italic) txt <- paste0("\\textit{", txt, "}")
    if (math)  txt <- paste0("$", txt, "$")
    return(txt)
  }

  sapply(x, function(s) format_decorations(escape_latex(s)), USE.NAMES = FALSE)
}

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
as_latex.dims <- function(x, brackets = NULL, ...) {
  # browser()
  # if (is.null(x$name)) {
  #   return(x)
  #   # return("\\emptyset")
  # } else {
    # name <- x$name
  dims <- paste(sapply(x, function(y) as_latex(y, ...)), collapse = ",")
  dims <- latex_wrap_brackets(dims, brackets, ...)
  # return(paste0("\\mathcal{", dims, "}"))
  # }
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
    # dims <- paste(x$dims, collapse = ",") |> latex_wrap_brackets(brackets, ...)
    dims <- as_latex(x$dims, brackets = brackets, sbscript_dims = subscript_dims, ...)
    if (subscript_dims) {
      dims <- paste0("_{", dims, "}")
    } else {
      # dims <- paste0("(", dims, ")")
      dims <- latex_wrap_brackets(dims, brackets = brackets, ...)
    }
    return(paste0("\\mathsf{", name, "}", dims, ""))
  }
}

#' @export
#' @method as_latex parameter
#' @rdname as_latex
as_latex.parameter <- function(x, brackets = NULL,
                              subscript_dims = is.null(brackets), ...) {
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    # dims <- paste(x$dims, collapse = ",") |> latex_wrap_brackets(brackets)
    dims <- as_latex(x$dims, brackets = brackets,
                     subscript_dims = subscript_dims,
                     ...)
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
as_latex.variable <- function(x, brackets = NULL, subscript_dims = TRUE, ...) {
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    # dims <- paste(x$dims, collapse = ",") |> latex_wrap_brackets(brackets)
    dims <- as_latex(x$dims, brackets = brackets, sbscript_dims = subscript_dims, ...)
    if (subscript_dims) {
      dims <- paste0("_{", dims, "}")
    } else {
      # dims <- paste0("(", dims, ")")
      dims <- latex_wrap_brackets(dims, brackets = brackets)
    }
    return(paste0("\\mathit{\\bf ", name, "}", dims, ""))
  }
}

#' @export
#' @method as_latex symbol
#' @rdname as_latex
as_latex.symbol <- function(x, ...) {
  paste0("\\texttt{", x$name, "}")
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


# as_latex0.expression <- function(x, brackets = NULL, ...) {
#   lhs <- as_latex(x$lhs, brackets = NULL, ...)
#   rhs <- as_latex(x$rhs, brackets = NULL, ...)
#   op <- latex_operators[[x$op]]
#   if (is.null(op)) {
#     stop("Unrecognized operator: ", x$op)
#   }
#   out <- paste0(lhs, " ", op, " ", rhs)
#   out <- latex_wrap_brackets(out, brackets)
#   return(out)
# }


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

  is_compound <- inherits(then, c("expression", "sum", "prod"))
  if (is_compound) {
    then_latex <- latex_wrap_brackets(then_latex, brackets = NULL,
                                      math = TRUE, autosize = TRUE,
                                      content = then_latex,
                                      context = class(then)[1], ...)
  }

  # --- Option 1: Indicator notation ---
  if (use_indicator) {
    indicator <- paste0(" \\cdot ", indicator_symbol, "_{", cond_latex, "}")
    return(paste0(then_latex, indicator))
  }

  # --- Option 2: Structured mapping form ---
  dims_tex <- NULL
  mapping_latex <- NULL

  if (inherits(cond, "mapping") && !is.null(cond$dims)) {
    dims_tex <- paste0(
      "\\left\\{\\textnormal{",
      paste(vapply(cond$dims, function(d) d$name, character(1)), collapse = ","),
      "}\\right\\}"
    )
    subscript <- as_latex(cond$dims, brackets = NULL, ...)
    mapping_latex <- paste0("\\mathsf{", cond$name, "}_{", subscript, "}")
  }

  if (inherits(cond, "where") && inherits(cond$content, "mapping")) {
    dims_tex <- paste0(
      "\\textnormal{",
      paste(vapply(cond$content$dims, function(d) d$name, character(1)), collapse = ","),
      "}"
    )
    mapping_latex <- paste0("\\mathsf{", cond$name, "}")
  }

  # Final format using: [ expr, {tuple} ∈ mapping ]
  if (!is.null(dims_tex) && !is.null(mapping_latex)) {
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
    # Just index names (e.g., i, j, k)
    index_latex <- paste0(vapply(index_node, as_latex, character(1)), collapse = ", ")

  } else if (inherits(index_node, "when")) {
    cond <- index_node$condition
    idx_latex <- paste0(vapply(index_node$then, as_latex, character(1)), collapse = ", ")

    if (inherits(cond, "where")) {
      # Named condition: i ∈ \mathsf{s4}
      set_name <- paste0("\\mathsf{", cond$name, "}")
      index_latex <- paste0(idx_latex, " \\in ", set_name)

    } else if (inherits(cond, "mapping")) {
      dims <- cond$dims
      dims_latex <- as_latex(dims, brackets = NULL, ...)
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
                              math_env = "equation",
                              brackets_dims = NULL,
                              subscript_dims = is.null(brackets_dims),
                              inline_where = NULL,
                              ...) {
  if (is.null(inline_where)) {
    inline_where <- getOption("multimod.render_where_inline", FALSE)
  }

  # Render LHS and RHS
  lhs <- as_latex(x$lhs, brackets = brackets_dims,
                  subscript_dims = subscript_dims,
                  inline_where = inline_where, ...)
  rhs <- as_latex(x$rhs, brackets = brackets_dims,
                  subscript_dims = subscript_dims,
                  inline_where = inline_where, ...)
  rel <- switch(x$relation, `==` = "=", `<=` = "\\le", `>=` = "\\ge", x$relation)

  # Apply alignment formatting
  body <- format_latex_aligned(lhs = lhs, rel = rel, rhs = rhs)

  # Equation name and descriptor
  preamble <- character()
  if (!is.null(x$name)) {
    preamble <- c(
      preamble,
      paste0("\\textbf{\\bf Equation:}~\\texttt{", x$name, "}",
             if (subscript_dims && !is.null(x$dims)) {
               paste0("$_{", as_latex(x$dims, brackets = brackets_dims,
                                      subscript_dims = subscript_dims, ...), "}$")
             } else "")
    )
  }
  if (!is.null(x$desc)) {
    preamble <- c(preamble, paste0("—\\textit{", as_latex(x$desc), "} \\\\"))
  }
  if (!is.null(x$domain)) {
    preamble <- c(
      preamble,
      paste0("\\\n\\text{Domain: }~$",
             as_latex(x$domain, brackets = brackets_dims,
                      subscript_dims = subscript_dims, ...), "$ \\\\")
    )
  }

  # Prepare where: block if needed
  where_lines <- character()
  if (!inline_where) {
    where_map <- extract_where_nodes(x)
    if (length(where_map) > 0) {
      where_lines <- c("\\textbf{where:}", "\\begin{flushleft}")
      for (nm in names(where_map)) {
        def <- as_latex(where_map[[nm]], inline_where = TRUE, ...)
        where_lines <- c(where_lines,
                         paste0("\\hspace*{2em}$\\texttt{", nm, "} = ", def, "$ \\\\"))
      }
      where_lines <- c(where_lines, "\\end{flushleft}")
    }
  }

  # Final output
  out <- c(
    "\\begin{flushleft}",
    paste(preamble, collapse = " \n"),
    "\\begin{equation}",
    "\\begin{aligned}",
    body,
    "\\end{aligned}",
    "\\end{equation}",
    where_lines,
    "\\end{flushleft}"
  )

  paste(out, collapse = "\n")
}

format_latex_aligned <- function(lhs, rhs, rel = "=") {
  # Split RHS by top-level operators (currently only top-level "+")
  rhs_terms <- strsplit(rhs, "(?<!\\\\)\\+", perl = TRUE)[[1]]
  rhs_terms <- trimws(rhs_terms)

  # First line: full equation
  lines <- character()
  first_line <- paste0("&", lhs, " ", rel, " ", rhs_terms[1], " \\\\")
  lines <- c(lines, first_line)

  if (length(rhs_terms) > 1) {
    continuation <- paste0("&\\quad + ", rhs_terms[-1], " \\\\")
    lines <- c(lines, continuation)
  }

  # Wrap in aligned + equation
  # body <- c("\\begin{equation}", "\\begin{aligned}", lines, "\\end{aligned}", "\\end{equation}")
  body <- c(lines)
  paste(body, collapse = "\n")
}


#' Split a LaTeX equation body into multiple lines if it's too long
#'
#' @param body LaTeX string containing the full equation body (e.g., "lhs = rhs").
#' @param max_line_length Approximate max number of characters per line.
#'
#' @return A character vector of lines for align or align* block.
format_latex_equation_lines <- function(lhs, rel, rhs, max_line_length = 120) {
  # Step 1: Break RHS into chunks by top-level +/-
  rhs_parts <- strsplit(rhs, "(?<!\\\\)(?=\\+| -)", perl = TRUE)[[1]]

  # Step 2: Accumulate chunks into lines
  rhs_lines <- character()
  current_line <- ""
  for (part in rhs_parts) {
    test_line <- if (nzchar(current_line)) paste0(current_line, " ", part) else part
    if (nchar(test_line) > max_line_length) {
      rhs_lines <- c(rhs_lines, current_line)
      current_line <- part
    } else {
      current_line <- test_line
    }
  }
  rhs_lines <- c(rhs_lines, current_line)

  # Step 3: Format lines for align block
  lines <- character()
  for (i in seq_along(rhs_lines)) {
    if (i == 1) {
      lines[i] <- paste0(lhs, " ", rel, " ", rhs_lines[i])
    } else {
      lines[i] <- paste0("&+ ", rhs_lines[i])
    }
  }

  return(lines)
}



format_index <- function(lhs) {
  dims <- lhs$dims
  if (is.null(dims) || length(dims) == 0) return("")

  dims_str <- as_latex(dims)
  paste0("_{", dims_str, "}")
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

#' @export
estimate_latex_length <- function(latex_str) {
  if (is.null(latex_str) || !nzchar(latex_str)) return(0)

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

#' @export
split_latex_equation <- function(latex_str, max_len = 80) {
  if (estimate_latex_length(latex_str) <= max_len) {
    return(list(latex_str))
  }

  # Tokenize LaTeX into chars with bracket depth tracking
  chars <- strsplit(latex_str, "")[[1]]
  depth <- 0
  buffer <- ""
  lines <- list()

  break_points <- c()
  safe_breaks <- c("+", "-", "\\\\cdot", "\\\\div")  # extend as needed
  inside_command <- FALSE
  command_buf <- ""

  for (i in seq_along(chars)) {
    ch <- chars[i]

    # Track bracket/brace depth
    if (ch %in% c("{", "[")) depth <- depth + 1
    if (ch %in% c("}", "]")) depth <- depth - 1

    buffer <- paste0(buffer, ch)

    # Detect LaTeX command boundaries
    if (ch == "\\" && !inside_command) {
      inside_command <- TRUE
      command_buf <- ch
      next
    }

    if (inside_command) {
      if (grepl("^[a-zA-Z]+$", ch)) {
        command_buf <- paste0(command_buf, ch)
        next
      } else {
        inside_command <- FALSE
        # insert full command back into buffer
        buffer <- paste0(substr(buffer, 1, nchar(buffer) - nchar(command_buf)), command_buf, ch)
      }
    }

    # Safe split if:
    # - current depth is 0 (not inside brackets/subscripts/etc.)
    # - current char matches a split symbol
    # - buffer is long enough
    if (depth == 0 && any(endsWith(buffer, safe_breaks))) {
      if (estimate_latex_length(buffer) >= max_len) {
        lines <- c(lines, substr(buffer, 1, nchar(buffer) - 1))
        buffer <- ch  # start new line with current char
      }
    }
  }

  # Final flush
  if (nzchar(buffer)) {
    lines <- c(lines, buffer)
  }

  return(lines)
}


#' Format LaTeX equation for align* environment
#'
#' This function formats long LaTeX equations or expressions by inserting
#' line breaks and indentations to improve readability in the \code{align*} environment.
#'
#' @param expr Full LaTeX expression string (optional).
#' @param lhs Left-hand side of the equation (optional).
#' @param rhs Right-hand side of the equation (optional).
#' @param rel Relation string, such as '=', '<=', etc. Default is '='.
#' @param max_width Approximate maximum character width before breaking line.
#'
#' @return A character vector of lines suitable for align* environment.
#' @export
format_latex_align <- function(lhs, rel, rhs_string, max_line_length = 100) {
  # Split at unescaped + or -
  split_regex <- "(?<!\\\\)\\s*([+-])\\s*"
  parts <- strsplit(rhs_string, split_regex, perl = TRUE)[[1]]
  ops <- regmatches(rhs_string, gregexpr(split_regex, rhs_string, perl = TRUE))[[1]]
  ops <- gsub("\\s+", "", ops) # remove spaces

  if (length(ops) < length(parts)) {
    ops <- c("", ops)  # first line has no leading op
  }

  # Trim and build lines
  lines <- character()
  for (i in seq_along(parts)) {
    chunk <- trimws(parts[i])
    if (i == 1) {
      lines[i] <- paste0(lhs, " ", rel, " ", chunk, " \\\\")
    } else {
      lines[i] <- paste0("&", ops[i], " ", chunk, " \\\\")
    }
  }

  return(lines)
}



