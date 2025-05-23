
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
    return(paste0("\\mathit{", name, "}", dims, ""))
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
    # Just index names (e.g., i, j, k)
    index_latex <- paste0(
      vapply(index_node, as_latex, character(1)),
      collapse = ", ")

  } else if (inherits(index_node, "when")) {
    cond <- index_node$condition
    idx_latex <- paste0(
      vapply(index_node$then, as_latex, character(1)),
      collapse = ", ")

    if (inherits(cond, "where")) {
      # Named condition: i ∈ \mathsf{s4}
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
                              subsection_number = FALSE,
                              ...) {
  # browser()
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

  body <- replace_mapping_placeholders(body)

  # Equation name and descriptor
  preamble <- character()

  # browser()
  # Construct preamble for equation output
  dims_latex <- as_latex(x$dims, brackets = NULL, subscript_dims = TRUE, ...)
  mapping_latex <- as_latex(x$domain, brackets = NULL, subscript_dims = TRUE, ...)

  preamble <- character()
  subsection <- if (subsection_number) "subsection" else "subsection*"

  if (!is.null(x$desc)) {
    # Header: name + description
    # preamble <- c(
    #   # Header: name + description
    #   paste0(
    #     "\\subsection*{",
    #     "\\textbf{", as_latex(x$name), "}"
    #   ))
    # + description
    # preamble <- paste0(
    #   preamble,
    #   # "}"
    #   "—",
    #   "\\textit{", as_latex(x$desc), "}",
    #   "}"
    # )
    # Description only
    preamble <- c(
      preamble,
      paste0("\\", subsection, "{\\textit{", as_latex(x$desc), "}}")
    )
  } else {
    preamble <- c(
      # Header: name + description
      paste0(
        "\\", subsection, "{",
        "\\textbf{", as_latex(x$name), "}}"
      ))
  }
  # dims and mapping
  preamble <- c(
    preamble,
    paste0("\\quad$\\textbf{", as_latex(x$name), "}_{", dims_latex, "}$"),
    paste0("$\\mid\\left\\{", dims_latex,"\\right\\} \\in ", mapping_latex, "$ \\\\")
  )

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
        def <- as_latex(where_map[[nm]], inline_where = TRUE, ...)
        # browser()
        # def <- replace_mapping_placeholders(def)
        # nm_greek <- replace_mapping_placeholders(nm)
        where_lines <- c(
          where_lines,
          # paste0("\\hspace*{2em}$\\texttt{", nm, "} = ", def, "$ \\"))
          replace_mapping_placeholders(
            paste0("$\\texttt{", nm, "} = ", def, "$ \\\\")
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
      where_lines <- c("\\textbf{where:} \\\\", where_lines)

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
    "\\begin{equation}",
    "\\begin{adjustbox}{max width=\\textwidth}",
    # paste0("\\begin{adjustbox}{", where_cols, "}"),
    "$\\begin{aligned}",
    body,
    "\\end{aligned}$",
    "\\end{adjustbox}",
    "\\end{equation}",
    where_lines,
    "\\vspace{1em}",
    "\\end{flushleft}"
  )

  paste(out, collapse = "\n")
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

# @export
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

#' Format a LaTeX equation across multiple lines using aligned
#'
#' @param lhs LaTeX string of the left-hand side
#' @param rhs LaTeX string of the right-hand side
#' @param rel Relational operator (e.g., `=`, `\le`, `\ge`)
#' @param max_len Maximum allowed line length before splitting
#'
#' @return Character string of the formatted LaTeX code
# @export
format_latex_aligned <- function(lhs, rhs, rel = "=", max_len = 80) {
  # browser()
  lhs_len <- estimate_latex_length(lhs)
  rhs_len <- estimate_latex_length(rhs)
  full_len <- lhs_len + nchar(rel) + rhs_len

  if (full_len <= max_len) {
    return(paste0("&", lhs, " ", rel, " ", rhs, " \\\\"))
  }

  lines <- paste0("&", lhs, " \\\\")
  # LHS is too long, split it
  if (lhs_len >= max_len) {
    lhs_parts <- split_at_top_level_operators(lhs, max_len = max_len - 10)
    # lines <- character()
    # lines[1] <- paste0("&", lhs, " \\\\")
    # lines[2] <- paste0("&\\quad", rel, " ", rhs_parts[1], " \\")
    if (length(lhs_parts) > 1) {
      lines <- paste0("&", lhs_parts[1], " \\\\")
      for (i in 2:length(lhs_parts)) {
        lines <- c(lines, paste0("&\\quad ", lhs_parts[i], " \\\\") )
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
      lines_rhs <- paste0("&\\quad ", rel, rhs_parts[1], " \\\\")
      for (i in 2:length(rhs_parts)) {
        lines_rhs[i] <- paste0("&\\quad ", rhs_parts[i], " \\\\")
      }
    } else {
      # unsuccessful split
      lines_rhs <- paste0("&\\quad ", rel, " ", rhs_parts[1], " \\\\")
    }
  } else {
    lines_rhs <- paste0("&\\quad ", rel, " ", rhs, " \\\\")
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

#' Replace mapping placeholders like \texttt{s1} with styled math symbols
#'
#' @param latex_str A character vector of LaTeX lines.
#' @param style One of "mathcal", "psi", "phi", or "custom".
#' @param base Optional base symbol for style = "custom" (e.g., "M").
#' @return Updated LaTeX character vector.
replace_mapping_placeholders <- function(latex_str, style = "mathcal", base = NULL) {
  # browser()
  for (i in 1:99) {
    # pattern <- paste0("\\mathsf\\{s", i, "\\}")
    pattern <- paste0("\\\\(texttt|mathsf)\\{s", i, "\\}")
    replacement <- switch(style,
                          mathcal = paste0("\\\\mathcal{M}_{", i, "}"),
                          psi     = paste0("\\\\psi_{", i, "}"),
                          phi     = paste0("\\\\varphi_{", i, "}"),
                          custom  = paste0("\\\\mathcal{", base %||% "X", "}_{", i, "}"),
                          stop("Unsupported style")
    )
    latex_str <- gsub(pattern, replacement, latex_str, fixed = FALSE)
  }
  # browser()
  latex_str
}

