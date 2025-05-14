
latex_operators <- list(
  "*" = " \\cdot ",
  "/" = " \\div ",
  "+" = " + ",
  "-" = " - ",
  "^" = " ^ ",
  "=" = " = ",
  "<=" = " \\leq ",
  ">=" = " \\geq ",
  "==" = " = ",
  "!=" = " \\neq ",
  "<" = " < ",
  ">" = " > ",
  "and" = " \\land ",
  "or"  = " \\lor ",
  "!" = " \\lnot ",
  "not" = " \\lnot "
)

latex_brackets <- function(x, brackets = NULL) {
  if (is.null(brackets)) return(x)
  if (is.null(x)) return(NULL)
  brackets <- brackets_pair(brackets)
  if (brackets[1] == "[") {
    return(paste0("\\left[", x, "\\right]"))
  } else if (brackets[1] == "{") {
    return(paste0("\\left\\{", x, "\\right\\}"))
  } else if (brackets[1] == "(") {
    return(paste0("\\left(", x, "\\right)"))
  } else {
    stop("Unsupported bracket type: ", brackets[1])
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
#' @method as_latex ast_set
#' @rdname as_latex
as_latex.ast_set <- function(x, ...) {
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    return(paste0("\\mathcal{", name, "}"))
  }
}

#' @export
#' @method as_latex ast_dims
#' @rdname as_latex
as_latex.ast_dims <- function(x, brackets = "[]", ...) {
  if (is.null(x$name)) {
    return(x)
    # return("\\emptyset")
  } else {
    name <- x$name
    dims <- paste(x$dims, collapse = ",")
    dims <- latex_brackets(dims, brackets)
    return(paste0("\\mathcal{", name, "}(", dims, ")"))
  }
}

#' @export
#' @method as_latex ast_expression
#' @rdname as_latex
as_latex.ast_mapping <- function(x, brackets = "[]", ...) {
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    dims <- paste(x$dims, collapse = ",") |> latex_brackets(brackets)
    return(paste0("\\mathsf{", name, "}(", dims, ")"))
  }
}

#' @export
#' @method as_latex ast_parameter
#' @rdname as_latex
as_latex.ast_parameter <- function(x, brackets = "[]", ...) {
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    dims <- paste(x$dims, collapse = ",") |> latex_brackets(brackets)
    return(paste0("\\mathsf{", name, "}(", dims, ")"))
  }
}

#' @export
#' @method as_latex ast_variable
#' @rdname as_latex
as_latex.ast_variable <- function(x, brackets = "[]", ...) {
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    dims <- paste(x$dims, collapse = ",") |> latex_brackets(brackets)
    return(paste0("\\mathit{", name, "}(", dims, ")"))
  }
}

#' @export
#' @method as_latex ast_symbol
#' @rdname as_latex
as_latex.ast_symbol <- function(x, ...) {
  paste0("\\texttt{", x$value, "}")
}

#' @export
#' @method as_latex ast_constant
#' @rdname as_latex
as_latex.ast_constant <- function(x, ...) {
  as.character(x$value)
}

#' @export
#' @method as_latex ast_unary
#' @rdname as_latex
as_latex.ast_unary <- function(x, brackets = NULL, ...) {
  rhs <- as_latex(x$rhs, brackets = brackets)
  op <- latex_operators[[x$op]]
  if (is.null(op)) {
    stop("Unrecognized operator: ", x$op)
  }
  out <- paste0(op, rhs)
  return(out)
}

#' @export
#' @method as_latex ast_expression
#' @rdname as_latex
as_latex.ast_expression <- function(x, brackets = NULL, ...) {
  lhs <- as_latex(x$lhs, brackets = NULL)
  rhs <- as_latex(x$rhs, brackets = NULL)
  op <- latex_operators[[x$op]]
  if (is.null(op)) {
    stop("Unrecognized operator: ", x$op)
  }
  out <- paste0(lhs, " ", op, " ", rhs)
  out <- latex_brackets(out, brackets)
  return(out)
}

#' @export
#' @method as_latex ast_condition
#' @rdname as_latex
as_latex.ast_condition <- function(x, brackets = NULL, ...) {
  cond <- as_latex(x$condition, brackets = brackets)
  then <- as_latex(x$then, brackets = brackets)
  return(paste0(then, "\\quad \\text{if }", cond))
}

#' @export
#' @method as_latex ast_sum
#' @rdname as_latex
as_latex.ast_sum <- function(x, brackets = NULL, ...) {
  index <- x$index
  domain <- if (!is.null(x$domain)) paste0(",\\; ", as_latex(x$domain, brackets = brackets)) else ""
  body <- as_latex(x$value, brackets = brackets)
  return(paste0("\\sum_{", index, domain, "} ", body))
}

#' @export
#' @method as_latex ast_prod
#' @rdname as_latex
as_latex.ast_prod <- function(x, brackets = NULL, ...) {
  index <- x$index
  domain <- if (!is.null(x$domain)) paste0(",\\; ", as_latex(x$domain, brackets = brackets)) else ""
  body <- as_latex(x$value, brackets = brackets)
  return(paste0("\\prod_{", index, domain, "} ", body))
}

.as_latex.multimod_ast <- function(x, brackets = TRUE, ...) {

  if (x$type == "set") {
    name <- x$name
    return(paste0("\\mathcal{", name, "}"))
  } else if (x$type == "unary") {
    rhs <- as_latex(x$rhs, add_parentheses = add_parentheses)
    op <- if (x$op == "-") "-" else if (x$op == "+") "+" else x$op
    out <- paste0(op, rhs)
    return(if (add_parentheses) paste0("\\left(", out, "\\right)") else out)

  } else if (x$type == "expression") {
    lhs <- as_latex(x$lhs, add_parentheses = add_parentheses)
    rhs <- as_latex(x$rhs, add_parentheses = add_parentheses)
    op <- x$op
    out <- paste0(lhs, " ", latex_operators[[op]], " ", rhs)
    return(if (add_parentheses) paste0("\\left(", out, "\\right)") else out)

  } else if (x$type == "variable" || x$type == "parameter") {
    dims <- paste(x$dims, collapse = ",")
    return(paste0("\\mathit{", x$name, "}(", dims, ")"))

  } else if (x$type == "constant") {
    return(as.character(x$value))

  } else if (x$type == "symbol") {
    return(paste0("\\texttt{", x$value, "}"))

  } else if (x$type == "condition") {
    cond <- as_latex(x$condition, add_parentheses = add_parentheses)
    then <- as_latex(x$then, add_parentheses = add_parentheses)
    return(paste0(then, "\\quad \\text{if }", cond))

  } else if (x$type == "sum") {
    index <- x$index
    domain <- if (!is.null(x$domain)) paste0(",\\; ", as_latex(x$domain, add_parentheses = add_parentheses)) else ""
    body <- as_latex(x$value, add_parentheses = add_parentheses)
    return(paste0("\\sum_{", index, domain, "} ", body))

  } else if (x$type == "prod") {
    index <- x$index
    domain <- if (!is.null(x$domain)) paste0(",\\; ", as_latex(x$domain, add_parentheses = add_parentheses)) else ""
    body <- as_latex(x$value, add_parentheses = add_parentheses)
    return(paste0("\\prod_{", index, domain, "} ", body))

  } else {
    stop("Unsupported AST node type: ", x$type)
  }

}


#' @export
write_latex <- function(x, file, ...) {
  UseMethod("write_latex")
}

# as_latex.multimod_equation <- function(eqn) {
#   # Helper to render an expression recursively
#   render_expr <- function(expr) {
#     if (is.null(expr)) return("")
#     switch(expr$type,
#            "expression" = {
#              lhs <- render_expr(expr$lhs)
#              rhs <- render_expr(expr$rhs)
#              op <- switch(expr$op,
#                           "*" = " \\cdot ",
#                           "/" = " \\div ",
#                           "+" = " + ",
#                           "-" = " - ",
#                           expr$op)
#              paste0("(", lhs, op, rhs, ")")
#            },
#            "sum" = {
#              index <- expr$index
#              domain <- if (!is.null(expr$domain)) paste0(",\\; ", render_expr(expr$domain)) else ""
#              body <- render_expr(expr$value)
#              paste0("\\sum_{", index, domain, "} ", body)
#            },
#            "prod" = {
#              index <- expr$index
#              domain <- if (!is.null(expr$domain)) paste0(",\\; ", render_expr(expr$domain)) else ""
#              body <- render_expr(expr$value)
#              paste0("\\prod_{", index, domain, "} ", body)
#            },
#            "condition" = {
#              cond <- render_expr(expr$condition)
#              then <- render_expr(expr$then)
#              paste0(then, "\\quad \\text{if }", cond)
#            },
#            "variable" = {
#              dims <- paste(expr$dims, collapse = ",")
#              paste0("\\mathit{", expr$name, "}(", dims, ")")
#            },
#            "parameter" = {
#              dims <- paste(expr$dims, collapse = ",")
#              paste0("\\mathsf{", expr$name, "}(", dims, ")")
#            },
#            "symbol" = {
#              paste0("\\texttt{", expr$value, "}")
#            },
#            # "compare" = {
#            #   lhs <- render_expr(expr$lhs)
#            #   rhs <- render_expr(expr$rhs)
#            #   op <- paste0(" ", expr$op, " ")
#            #   paste0(lhs, op, rhs)
#            # },
#            # "logic" = {
#            #   lhs <- render_expr(expr$lhs)
#            #   rhs <- render_expr(expr$rhs)
#            #   op <- paste0(" \\text{", expr$op, "} ")
#            #   paste0("(", lhs, op, rhs, ")")
#            # },
#            paste0("[Unsupported type: ", expr$type, "]")
#     )
#   }
#
#   lhs <- render_expr(eqn$lhs)
#   rhs <- render_expr(eqn$rhs)
#   relation <- switch(eqn$relation,
#                   "==" = "=",
#                   "<=" = "\\leq",
#                   ">=" = "\\geq",
#                   eqn$relation
#   )
#
#   eq_label <- paste0("\\textbf{", eqn$name, "}(", paste(eqn$dims, collapse = ", "), ")")
#   body <- paste(lhs, relation, rhs)
#
#   paste0("\\begin{align*}\n",
#          eq_label, " &:\\quad ", body, "\n",
#          "\\end{align*}")
# }

#' @export
#' @method as_latex multimod_equation
#' @rdname as_latex
as_latex.multimod_equation <- function(eqn,
                                       math_env = "dmath"
                                       ) {
  # Recursive rendering of AST
  render_expr <- function(expr) {
    # browser()
    if (is.null(expr)) return("")
    switch(expr$type,
           "expression" = {
             lhs <- render_expr(expr$lhs)
             rhs <- render_expr(expr$rhs)
             op <- switch(expr$op,
                          "*" = " \\cdot ",
                          "/" = " \\div ",
                          "+" = " + ",
                          "-" = " - ",
                          "^" = " ^ ",
                          "=" = " = ",
                          "<=" = " \\leq ",
                          ">=" = " \\geq ",
                          "==" = " = ",
                          "!=" = " \\neq ",
                          "<" = " < ",
                          ">" = " > ",
                          "and" = " \\land ",
                          "or"  = " \\lor ",
                          "!" = " \\lnot ",
                          "not" = " \\lnot ",
                          expr$op)
             paste0("(", lhs, op, rhs, ")")
           },
           "unary" = {
             # lhs <- render_expr(expr$lhs)
             rhs <- render_expr(expr$rhs)
             op <- switch(expr$op,
                          # "*" = " \\cdot ",
                          # "/" = " \\div ",
                          "+" = " + ",
                          "-" = " - ",
                          # "^" = " ^ ",
                          # "=" = " = ",
                          # "<=" = " \\leq ",
                          # ">=" = " \\geq ",
                          # "==" = " = ",
                          # "!=" = " \\neq ",
                          # "<" = " < ",
                          # ">" = " > ",
                          "!" = " \\lnot ",
                          # "and" = " \\land ",
                          # "or"  = " \\lor ",
                          "not" = " \\lnot ",
                          expr$op)
             paste0("(", lhs, op, rhs, ")")
           },
           "sum" = {
             idx <- expr$index
             dom <- if (!is.null(expr$domain)) paste0(",\\; ", render_expr(expr$domain)) else ""
             body <- render_expr(expr$value)
             paste0("\\sum_{", idx, dom, "} ", body)
           },
           "prod" = {
             idx <- expr$index
             dom <- if (!is.null(expr$domain)) paste0(",\\; ", render_expr(expr$domain)) else ""
             body <- render_expr(expr$value)
             paste0("\\prod_{", idx, dom, "} ", body)
           },
           "condition" = {
             cond <- render_expr(expr$condition)
             then <- render_expr(expr$then)
             paste0("(", then, ")\\;\\text{if }", cond)
           },
           "variable" = {
             dims <- paste(expr$dims, collapse = ",")
             paste0("\\mathit{", expr$name, "}(", dims, ")")
           },
           "parameter" = {
             dims <- paste(expr$dims, collapse = ",")
             paste0("\\mathsf{", expr$name, "}(", dims, ")")
           },
           "mapping" = {
             dims <- paste(expr$dims, collapse = ",")
             paste0("\\mathsf{", expr$name, "}(", dims, ")")
           },
           "set" = {
             dims <- paste(expr$dims, collapse = ",")
             paste0("\\mathcal{", expr$name, "}(", dims, ")")
           },
           "dims" = {
             dims <- paste(expr$dims, collapse = ",")
             paste0("\\mathcal{", expr$name, "}(", dims, ")")
           },
           "symbol" = paste0("\\texttt{", expr$value, "}"),
           "const" = as.character(expr$value),
           paste0("[Unsupported type: ", expr$type, "]")
    )
  }

  # Equation body
  lhs <- render_expr(eqn$lhs)
  rhs <- render_expr(eqn$rhs)
  rel <- switch(eqn$relation, "==" = "=", "<=" = "\\leq", ">=" = "\\geq", eqn$relation)

  # Header lines
  eq_name <- paste0("\\textbf{Equation:}~\\texttt{", eqn$name, "}(", paste(eqn$dims, collapse = ", "), ")")
  if (!is.null(eqn$desc) && nzchar(eqn$desc)) {
    eq_name <- paste0(eq_name, "\\quad\\textit{", as_latex(eqn$desc), "}")
  }

  domain_line <- if (!is.null(eqn$domain)) {
    dom_expr <- render_expr(eqn$domain)
    paste0("\\text{Domain: } $", dom_expr, "$ \\\\")
  } else ""


  # Equation rendering
  eq_body <- paste0(lhs, " &", rel, " ", rhs)

  math_env <- switch(
    tolower(math_env),
    "align" = c("\\begin{align*}", eq_body, "\\end{align*}"),
    "dmath" = c("\\begin{dmath*}", eq_body, "\\end{dmath*}"),
    "equation" = c("\\begin{equation*}", eq_body, "\\end{equation*}"),
    "none" = c("$$", eq_body, "$$"),
    stop("Unsupported math_env: ", math_env)
  )

  # Combine everything
  paste0(
    "\\begin{flushleft}\n",
    eq_name, "\\\\\n",
    domain_line,
    paste(math_env, collapse = "\n"),
    "\n\\end{flushleft}\n"
  )
}


export_multimod_to_latex <- function(model, file = "model_equations.tex",
                                     title = "Model Equations") {
  eqs <- model$equations
  if (length(eqs) == 0) stop("No equations found in model.")

  lines <- c(
    "\\documentclass{article}",
    "\\usepackage{amsmath}",
    "\\usepackage{amssymb}",
    "\\usepackage[margin=1in]{geometry}",
    "\\begin{document}",
    paste0("\\section*{", title, "}"),
    ""
  )

  for (eqn in eqs) {
    latex <- render_multimod_equation_to_latex(eqn)
    lines <- c(lines, latex, "")
  }

  lines <- c(lines, "\\end{document}")
  writeLines(lines, con = file)
  message("LaTeX file written to: ", normalizePath(file))
}

if (F) {

  symbols <- build_symbols_list(model_info)

  mod_eqn <- coerce_gams_equation(
    model_info$equations[[1]],
    symbols)

  render_multimod_equation_to_latex(mod_eqn)

  export_multimod_to_latex(multimod_model, "tmp/multimod_model.tex")
}


render_ast_to_pdf <- function(ast_node, file = "equation_ast.tex", width = 6, height = 4, compile_pdf = TRUE) {
  if (!requireNamespace("tikzDevice", quietly = TRUE)) {
    stop("Please install the 'tikzDevice' package.")
  }

  # Load amsmath for align environment
  options(tikzLatexPackages = c(getOption("tikzLatexPackages"),
                                "\\usepackage{amsmath}"))

  latex_str <- multimod::as_latex(ast_node)
  lines <- strwrap(latex_str, width = 80)
  wrapped <- paste(lines, collapse = " \\\\ \n")
  body <- paste0("\\begin{align*}\n", wrapped, "\n\\end{align*}")

  # Write TeX with tikzDevice
  tikzDevice::tikz(file = file, standAlone = TRUE, width = width, height = height)
  plot.new()
  title(main = body, line = -1, cex.main = 1)
  dev.off()

  message("TeX written to: ", normalizePath(file))

  if (compile_pdf && requireNamespace("tools", quietly = TRUE)) {
    pdf_file <- sub("\\.tex$", ".pdf", file)
    tools::texi2pdf(file, clean = TRUE)
    message("PDF compiled: ", normalizePath(pdf_file))
  } else if (compile_pdf) {
    message("Install 'tools' package or use LaTeX manually to compile.")
  }
}
if (F) {
  # Example usage
  # ast_node <- multimod::as_multimod_expr("x + y")
  # render_ast_to_pdf(ast_node, file = "equation_ast.tex", width = 6, height = 4)

  ast <- parse_gams_expr("x + y*z - w/a", symbols = list())
  render_ast_to_pdf(ast, file = "tmp/ast_equation.tex")
  tools::texi2pdf("tmp/equation_ast.tex", clean = TRUE)


}

format_latex_ast <- function(latex_str, max_width = 80, indent = "  ") {
  chars <- strsplit(latex_str, "")[[1]]
  level <- 0
  lines <- c()
  current <- ""

  for (i in seq_along(chars)) {
    ch <- chars[i]

    if (ch == "(" || ch == "{") {
      level <- level + 1
      current <- paste0(current, ch)
      next
    }

    if (ch == ")" || ch == "}") {
      level <- max(level - 1, 0)
      current <- paste0(current, ch)
      next
    }

    current <- paste0(current, ch)

    is_split_point <- ch %in% c("+", "-", "*", "/") &&
      nchar(current) > max_width &&
      level == 0

    if (is_split_point) {
      lines <- c(lines, current)
      current <- paste0(strrep(indent, level))
    }
  }

  lines <- c(lines, current)
  paste(lines, collapse = "\n")
}


format_latex_with_indent <- function(latex_str, max_width = 80, indent = "  ") {
  chars <- strsplit(latex_str, "")[[1]]
  lines <- c()
  current <- ""
  level <- 0
  in_frac <- FALSE
  in_supsub <- FALSE
  escaping <- FALSE

  for (i in seq_along(chars)) {
    ch <- chars[i]
    next_ch <- if (i < length(chars)) chars[i + 1] else ""

    # Detect escaped characters like \f
    if (ch == "\\") {
      escaping <- TRUE
      current <- paste0(current, ch)
      next
    }

    if (escaping) {
      current <- paste0(current, ch)
      escaping <- FALSE

      # Track entry into \frac{...}
      if (tolower(paste0("\\", ch)) == "\\frac") in_frac <- TRUE
      next
    }

    # Track superscripts and subscripts
    if (ch == "^" || ch == "_") {
      in_supsub <- TRUE
    } else if (ch == "{" && in_supsub) {
      level <- level + 1
      in_supsub <- FALSE
    } else if (ch == "{" && !in_supsub) {
      level <- level + 1
    } else if (ch == "}") {
      level <- max(level - 1, 0)
      if (in_frac && level == 0) in_frac <- FALSE
    }

    current <- paste0(current, ch)

    # Break only if not in \frac or superscript/subscript
    if (nchar(current) > max_width && level == 0 &&
        !in_frac && ch %in% c("+", "-", "*", "/", "=")) {
      lines <- c(lines, current)
      current <- indent
    }
  }

  lines <- c(lines, current)
  paste(lines, collapse = " \\\\\n")
}

format_latex_equation_split <- function(lhs, rhs, relation = "=",
                                        max_total_width = 80,
                                        max_side_width = 50,
                                        indent = "  ",
                                        symbol_map = NULL) {

  apply_symbol_map <- function(text, map) {
    if (is.null(map)) return(text)
    for (key in names(map)) {
      # Replace whole-word matches or symbol(dims) patterns
      text <- gsub(paste0("\\b", key, "\\b"), map[[key]], text)
      text <- gsub(paste0(key, "\\("), paste0(map[[key]], "("), text, fixed = TRUE)
    }
    return(text)
  }

  # Apply symbol renaming
  lhs <- apply_symbol_map(lhs, symbol_map)
  rhs <- apply_symbol_map(rhs, symbol_map)

  # Helper: indent-aware formatter
  format_side <- function(expr, max_width, indent) {
    chars <- strsplit(expr, "")[[1]]
    lines <- c()
    current <- ""
    level <- 0

    for (i in seq_along(chars)) {
      ch <- chars[i]
      current <- paste0(current, ch)

      if (ch == "{" || ch == "(") level <- level + 1
      if (ch == "}" || ch == ")") level <- max(level - 1, 0)

      if (nchar(current) > max_width && ch %in% c("+", "-", "*", "/", "=") && level == 0) {
        lines <- c(lines, current)
        current <- strrep(indent, level)
      }
    }

    lines <- c(lines, current)
    return(paste(lines, collapse = " \\\\\n"))
  }

  # Split long equations into 3 blocks if needed
  full_line <- paste0(lhs, " ", relation, " ", rhs)

  if (nchar(full_line) > max_total_width) {
    lhs_fmt <- format_side(lhs, max_side_width, indent)
    rhs_fmt <- format_side(rhs, max_side_width, indent)

    return(paste0(
      lhs_fmt, " \\\\\n",
      relation, " \\\\\n",
      rhs_fmt
    ))
  } else {
    return(full_line)
  }
}

if (F) {
  lhs <- "vTechInp_{t,c,r,y,s} * pTechCinp2use(t, c, r, y, s)"
  rhs <- "vTechOut(t, cp, r, y, s) / pTechUse2cact(t, cp, r, y, s) / pTechCact2cout(t, cp, r, y, s)"

  format_latex_equation_split(lhs, rhs, relation = "\\leq", symbol_map = list(
    t = "tech", c = "comm", cp = "commp", r = "region", y = "year", s = "slice",
    pTechCinp2use = "pC2use", pTechUse2cact = "pUse2a", pTechCact2cout = "pA2out"
  ))

}

# Render multimod_equation to LaTeX with formatting
# as_latex.multimod_equation <- function(
#     eqn,
#     eqn_name = TRUE,
#     eqn_domain = TRUE,
#     subscript_map = NULL,
#     max_width = 80,
#     indent = "  "
#     ) {
#   format_expr <- function(expr, subscript_map = NULL, indent_level = 0) {
#     if (is.null(expr)) return("")
#     pad <- paste(rep(indent, indent_level), collapse = "")
#
#     if (expr$type == "variable" || expr$type == "parameter") {
#       name <- if (!is.null(subscript_map) && expr$name %in% names(subscript_map)) subscript_map[[expr$name]] else expr$name
#       if (length(expr$dims) > 0 && !identical(subscript_map, FALSE)) {
#         dims <- sapply(expr$dims, function(a) if (!is.null(subscript_map) && a %in% names(subscript_map)) subscript_map[[a]] else a)
#         return(paste0(name, "_{", paste(dims, collapse = ","), "}"))
#       } else {
#         return(name)
#       }
#     }
#     if (expr$type == "constant") return(as.character(expr$value))
#     if (expr$type == "symbol") return(expr$value)
#     if (expr$type == "condition") {
#       then_txt <- format_expr(expr$then, subscript_map, indent_level)
#       cond_txt <- format_expr(expr$condition, subscript_map, indent_level)
#       return(paste0("(", then_txt, ")$", cond_txt))
#     }
#     if (expr$type %in% c("sum", "prod")) {
#       val_txt <- format_expr(expr$value, subscript_map, indent_level + 1)
#       dom_txt <- if (!is.null(expr$domain)) paste0("$", format_expr(expr$domain, subscript_map, indent_level + 1)) else ""
#       return(paste0(expr$type, "(", expr$index, dom_txt, ", ", val_txt, ")"))
#     }
#     if (expr$type == "expression") {
#       lhs <- format_expr(expr$lhs, subscript_map, indent_level)
#       rhs <- format_expr(expr$rhs, subscript_map, indent_level)
#       op <- expr$op
#       if (nchar(lhs) + nchar(rhs) + 3 > max_width) {
#         return(paste0(lhs, "\\\\\n", pad, op, " ", rhs))
#       } else {
#         return(paste(lhs, op, rhs))
#       }
#     }
#     return("<unknown expr>")
#   }
#
#   lhs <- format_expr(eqn$lhs, subscript_map)
#   rhs <- format_expr(eqn$rhs, subscript_map)
#   relation <- switch(eqn$relation, "==" = "=", "<=" = "\\le", ">=" = "\\ge")
#
#   # Split long equations
#   latex_body <- if (nchar(lhs) + nchar(rhs) + 3 > max_width) {
#     paste0(lhs, "\\\\\n", indent, relation, " ", rhs)
#   } else {
#     paste(lhs, relation, rhs)
#   }
#
#   # Wrap in LaTeX environment
#   lines <- c("\\begin{flushleft}")
#   if (eqn_name) lines <- c(lines, paste0("\\text{", eqn$name, "} \\\\"))
#   if (eqn_domain && !is.null(eqn$domain)) {
#     domain_str <- format_expr(eqn$domain, subscript_map)
#     lines <- c(lines, paste0("\\text{domain: } ", domain_str, " \\\\"))
#   }
#   lines <- c(lines,
#              "\\begin{equation*}",
#              "\\begin{aligned}",
#              latex_body,
#              "\\end{aligned}",
#              "\\end{equation*}",
#              "\\end{flushleft}"
#   )
#
#   return(paste(lines, collapse = "\n"))
# }

# as_latex.multimod_equation <- function(eqn, eqn_name = TRUE, eqn_domain = TRUE,
#                                   subscript_map = NULL, max_width = 80,
#                                   indent = "  ", bold_lhs = TRUE) {
#   format_expr <- function(expr, subscript_map = NULL, indent_level = 0, in_frac = FALSE) {
#     if (is.null(expr)) return("")
#     pad <- paste(rep(indent, indent_level), collapse = "")
#
#     if (expr$type %in% c("variable", "parameter")) {
#       name <- if (!is.null(subscript_map) && expr$name %in% names(subscript_map))
#         subscript_map[[expr$name]] else expr$name
#
#       if (length(expr$dims) > 0 && !identical(subscript_map, FALSE)) {
#         dims <- sapply(expr$dims, function(a)
#           if (!is.null(subscript_map) && a %in% names(subscript_map)) subscript_map[[a]] else a)
#         return(paste0(name, "_{", paste(dims, collapse = ","), "}"))
#       } else {
#         return(name)
#       }
#     }
#
#     if (expr$type == "symbol") return(expr$value)
#     if (expr$type == "constant") return(as.character(expr$value))
#
#     if (expr$type == "condition") {
#       then_txt <- format_expr(expr$then, subscript_map, indent_level)
#       cond_txt <- format_expr(expr$condition, subscript_map, indent_level)
#       return(paste0("(", then_txt, ")$", cond_txt))
#     }
#
#     if (expr$type %in% c("sum", "prod")) {
#       val_txt <- format_expr(expr$value, subscript_map, indent_level + 1)
#       dom_txt <- if (!is.null(expr$domain))
#         paste0("$", format_expr(expr$domain, subscript_map, indent_level + 1)) else ""
#       return(paste0(expr$type, "(", expr$index, dom_txt, ", ", val_txt, ")"))
#     }
#
#     if (expr$type == "expression") {
#       if (expr$op == "/") {
#         num <- format_expr(expr$lhs, subscript_map, indent_level, in_frac = TRUE)
#         den <- format_expr(expr$rhs, subscript_map, indent_level, in_frac = TRUE)
#         return(paste0("\\frac{", num, "}{", den, "}"))
#       }
#
#       lhs <- format_expr(expr$lhs, subscript_map, indent_level, in_frac)
#       rhs <- format_expr(expr$rhs, subscript_map, indent_level, in_frac)
#       op <- expr$op
#       return(paste(lhs, op, rhs))
#     }
#
#     return("<unknown>")
#   }
#
#   lhs <- format_expr(eqn$lhs, subscript_map)
#   if (bold_lhs && eqn$lhs$type == "variable") {
#     lhs <- paste0("\\mathbf{", lhs, "}")
#   }
#
#   rhs <- format_expr(eqn$rhs, subscript_map)
#   relation <- switch(eqn$relation, "==" = "=", "<=" = "\\le", ">=" = "\\ge")
#
#   # lines <- c()
#   # if (eqn_name) lines <- c(lines, paste0("\\textbf{Equation:} \\texttt{", eqn$name, "}\n"))
#   # if (eqn_domain && !is.null(eqn$domain)) {
#   #   domain_str <- format_expr(eqn$domain, subscript_map)
#   #   # lines <- c(lines, paste0("\\textbf{Domain:} ", domain_str))
#   #   lines <- c(lines, paste0("\\textbf{domain: } $", domain_str, "$ \\"))
#   # }
#
#   # Equation name with indices
#   declared_dims_str <- paste(eqn$dims, collapse = ",")
#   equation_header <- paste0("\\textbf{Equation:}~\\texttt{", eqn$name, "[", declared_dims_str, "]} ")
#
#   # Optional desc
#   description_line <- if (!is.null(eqn$desc) && nzchar(eqn$desc)) {
#     paste0("\\textit{", eqn$desc, "} \\\\")
#   } else {"\\\\"}
#
#   # Optional domain/mapping
#   domain_line <- if (!is.null(eqn$domain)) {
#     domain_str <- format_expr(eqn$domain, subscript_map)
#     paste0("\\textbf{\\textit{domain:}}~$", domain_str, "$ \\\\")
#   } else NULL
#
#   # # Optional domain/mapping
#   # if (!is.null(eqn$domain)) {
#   #   domain_str <- format_expr(eqn$domain, subscript_map)
#   #   domain_header <- paste0("\\textbf{Mapping:}~$", domain_str, "$ \\\\")
#   # } else {
#   #   domain_header <- NULL
#   # }
#
#   # browser()
#   lhs_str <- format_expr(eqn$lhs, subscript_map)
#   rhs_str <- format_expr(eqn$rhs, subscript_map)
#
#
#   # Break long LHS
#   lhs_lines <- if (nchar(lhs_str) > max_width) {
#     strwrap(lhs_str, width = max_width, exdent = 2)
#   } else {
#     lhs_str
#   }
#
#   # Combine with relation
#   lhs_block <- if (is.character(lhs_lines)) paste(lhs_lines, collapse = " \\\\\n") else lhs_lines
#
#   # Combine relation and RHS
#   if (nchar(rhs_str) + nchar(relation) + 3 <= max_width) {
#     rhs_block <- paste(relation, rhs_str)
#   } else {
#     rhs_wrapped <- strwrap(rhs_str, width = max_width, exdent = 2)
#     rhs_block <- paste(c(relation, rhs_wrapped), collapse = " \\\\\n")
#   }
#
#   # Final body
#   latex_body <- paste(lhs_block, "\\\\\n", rhs_block)
#
#
#
#   lines <- c(
#     # lines,
#     "\\begin{flushleft}",
#     equation_header,
#     description_line,
#     domain_line,
#     # domain_header,
#     "\\vspace{-1.em}",
#     # "\\begin{equation*}",
#     # "\\begin{align*}",
#     # "\\begin{fleqn}",
#     # "\\begin{minipage}{\\textwidth}",
#     "\\begin{equation*}",
#     "\\begin{aligned}",
#     # paste0(lhs, " ", relation, " ", rhs),
#     latex_body,
#     "\\end{aligned}",
#     "\\end{equation*}",
#     # "\\end{minipage}",
#     # "\\end{fleqn}",
#     # "\\end{align*}",
#     # "\\end{equation*}"
#     "\\end{flushleft}"
#   )
#
#   paste(lines, collapse = "\n")
# }

apply_aliases_to_ast <- function(ast, alias_map) {
  if (is.null(ast) || !is.list(ast)) return(ast)
  if (is.null(ast$type)) return(ast)  # defensive check

  out <- ast  # make a copy to avoid mutation

  # Rename symbol or dimension name
  if (out$type %in% c("variable", "parameter", "symbol")) {
    if (!is.null(out$name) && out$name %in% names(alias_map)) {
      out$name <- alias_map[[out$name]]
    }
    if (!is.null(out$dims)) {
      out$dims <- lapply(out$dims, function(arg) {
        if (arg %in% names(alias_map)) alias_map[[arg]] else arg
      })
    }
    return(out)
  }

  # Apply to expressions recursively
  if (out$type == "expression") {
    out$lhs <- apply_aliases_to_ast(out$lhs, alias_map)
    out$rhs <- apply_aliases_to_ast(out$rhs, alias_map)
    return(out)
  }

  if (out$type == "condition") {
    out$condition <- apply_aliases_to_ast(out$condition, alias_map)
    out$then <- apply_aliases_to_ast(out$then, alias_map)
    return(out)
  }

  if (out$type %in% c("sum", "prod")) {
    out$index <- if (out$index %in% names(alias_map)) alias_map[[out$index]] else out$index
    out$domain <- apply_aliases_to_ast(out$domain, alias_map)
    out$value <- apply_aliases_to_ast(out$value, alias_map)
    return(out)
  }

  return(out)  # fallback for other types like 'const'
}

apply_aliases_to_eqn <- function(eqn, alias_map) {
  eqn$lhs <- apply_aliases_to_ast(eqn$lhs, alias_map)
  eqn$rhs <- apply_aliases_to_ast(eqn$rhs, alias_map)
  if (!is.null(eqn$domain)) {
    eqn$domain <- apply_aliases_to_ast(eqn$domain, alias_map)
  }
  eqn$dims <- sapply(eqn$dims, function(dim) {
    if (dim %in% names(alias_map)) alias_map[[dim]] else dim
  })
  return(eqn)
}

#' @export
apply_aliases <- function(x, alias_map, ...) {
  UseMethod("apply_aliases")
}

#' @export
apply_aliases.multimod_equation <- function(x, alias_map, ...) {
  apply_aliases_to_eqn(x, alias_map)
}

# apply_aliases.multimod_structure <- function(x, alias_map, ...) {
#   x$equations <- lapply(x$equations, apply_aliases, alias_map = alias_map)
#   return(x)
# }
#' @export
apply_aliases.multimod_ast <- function(x, alias_map, ...) {
  apply_aliases_to_ast(x, alias_map)
}


#' @export
as_latex2.multimod_equation <- function(eqn,
                                  eqn_name = TRUE,
                                  eqn_domain = TRUE,
                                  eqn_desc = TRUE,
                                  subscript_map = NULL,
                                  max_width = 80,
                                  indent = "  ",
                                  math_env = "dmath") {

  format_expr <- function(expr, subscript_map = NULL, indent_level = 0) {
    if (is.null(expr)) return("")
    pad <- paste(rep(indent, indent_level), collapse = "")

    if (expr$type %in% c("variable", "parameter")) {
      name <- if (!is.null(subscript_map[[expr$name]])) subscript_map[[expr$name]] else expr$name
      dims <- expr$dims
      if (length(dims) > 0 && !identical(subscript_map, FALSE)) {
        dims <- sapply(dims, function(a) if (!is.null(subscript_map[[a]])) subscript_map[[a]] else a)
        return(paste0(name, "_{", paste(dims, collapse = ","), "}"))
      } else {
        return(name)
      }
    }

    if (expr$type == "constant") return(as.character(expr$value))

    if (expr$type == "symbol") return(expr$value)

    if (expr$type == "condition") {
      then_txt <- format_expr(expr$then, subscript_map, indent_level)
      cond_txt <- format_expr(expr$condition, subscript_map, indent_level)
      return(paste0("(", then_txt, ")$", cond_txt))
    }

    if (expr$type %in% c("sum", "prod")) {
      val_txt <- format_expr(expr$value, subscript_map, indent_level + 1)
      dom_txt <- if (!is.null(expr$domain)) paste0("$", format_expr(expr$domain, subscript_map, indent_level + 1)) else ""
      return(paste0(expr$type, "(", expr$index, dom_txt, ", ", val_txt, ")"))
    }

    if (expr$type == "expression") {
      lhs <- format_expr(expr$lhs, subscript_map, indent_level)
      rhs <- format_expr(expr$rhs, subscript_map, indent_level)
      op <- expr$op
      if (nchar(lhs) + nchar(rhs) + 3 > max_width) {
        return(paste0(lhs, "\\\\\n", pad, op, " ", rhs))
      } else {
        return(paste(lhs, op, rhs))
      }
    }

    return("<unknown expr>")
  }

  lhs <- format_expr(eqn$lhs, subscript_map)
  rhs <- format_expr(eqn$rhs, subscript_map)
  relation <- switch(eqn$relation, "==" = "=", "<=" = "\\le", ">=" = "\\ge")

  # Construct body, split if needed
  if (nchar(lhs) + nchar(rhs) + 3 > max_width) {
    latex_body <- paste0(lhs, "\\\\\n", indent, relation, " ", rhs)
  } else {
    latex_body <- paste(lhs, relation, rhs)
  }

  header <- character()
  if (eqn_name) header <- c(header, paste0("\\textbf{Equation:} \\texttt{", eqn$name, "} \\\\"))
  if (eqn_domain && !is.null(eqn$domain)) {
    domain_str <- format_expr(eqn$domain, subscript_map)
    header <- c(header, paste0("\\textit{Domain: } $", domain_str, "$ \\\\"))
  }
  if (eqn_desc && !is.null(eqn$desc)) {
    header <- c(header, paste0("\\textit{Description: } ", as_latex(eqn$desc), " \\\\"))
  }

  lines <- c(
    header,
    paste0("\\begin{", math_env, "}"),
    latex_body,
    paste0("\\end{", math_env, "}")
  )

  return(paste(lines, collapse = "\n"))
}

multimod_latex_preamble <- function(document_class = "article",
                                    font_size = "11pt",
                                    paper_size = "a4paper",
                                    packages = c("amsmath", "amssymb", "breqn", "geometry"),
                                    extra_lines = NULL,
                                    title = "Multimod Equations") {
  pkg_lines <- paste0("\\usepackage{", packages, "}")
  preamble <- c(
    paste0("\\documentclass[", font_size, ",", paper_size, "]{", document_class, "}"),
    pkg_lines,
    "\\geometry{margin=1in}",
    "\\title{", title, "}",
    "\\begin{document}",
    "\\maketitle"
  )

  if (!is.null(extra_lines)) {
    preamble <- c(preamble, extra_lines)
  }

  return(preamble)
}

#' @export
#'
#' @method write_latex multimod_model
#'
#' @rdname write_latex
#'
write_latex.multimod_model <- function(model,
                                     file = "model.tex",
                                     include_sets = TRUE,
                                     include_aliases = TRUE,
                                     include_parameters = TRUE,
                                     include_variables = TRUE,
                                     include_equations = TRUE,
                                     # math_env = "dmath",
                                     math_env = "align",
                                     preamble_fun = multimod_latex_preamble) {
  lines <- preamble_fun()

  if (include_sets && !is.null(model$sets)) {
    lines <- c(lines, "", "\\section*{Sets}")
    for (s in model$sets) {
      lines <- c(lines, paste0("\\texttt{", s$name, "} -- ",
                               as_latex(s$desc), "\\\\"))
    }
  }

  if (include_parameters && !is.null(model$parameters)) {
    lines <- c(lines, "", "\\section*{Parameters}")
    for (p in model$parameters) {
      lines <- c(lines, paste0("\\texttt{", p$name, "}(",
                               paste(p$dims, collapse = ","), ") -- ",
                               as_latex(p$desc), "\\\\"))
    }
  }

  if (include_variables && !is.null(model$variables)) {
    lines <- c(lines, "", "\\section*{Variables}")
    for (v in model$variables) {
      lines <- c(lines, paste0("\\texttt{", v$name, "}(",
                               paste(v$dims, collapse = ","), ") -- ",
                               as_latex(v$desc), "\\\\"))
    }
  }

  if (include_equations && !is.null(model$equations)) {
    lines <- c(lines, "", "\\section*{Equations}")
    for (eqn in model$equations) {
      lines <- c(lines, as_latex(eqn, math_env = math_env))
    }
  }

  lines <- c(lines, "", "\\end{document}")
  writeLines(lines, con = file)
  invisible(file)
}

