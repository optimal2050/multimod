
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

latex_brackets <- function(x, brackets = NULL, ...) {
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
  dims <- latex_brackets(dims, brackets, ...)
  # return(paste0("\\mathcal{", dims, "}"))
  # }
}

#' @export
#' @method as_latex expression
#' @rdname as_latex
as_latex.mapping <- function(x, brackets = "[]",
                             subscript_dims = is.null(brackets), ...) {
  # return(NULL)
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    # dims <- paste(x$dims, collapse = ",") |> latex_brackets(brackets, ...)
    dims <- as_latex(x$dims, brackets = brackets, sbscript_dims = subscript_dims, ...)
    if (subscript_dims) {
      dims <- paste0("_{", dims, "}")
    } else {
      dims <- paste0("(", dims, ")")
    }
    return(paste0("\\mathsf{", name, "}", dims, ""))
  }
}

#' @export
#' @method as_latex parameter
#' @rdname as_latex
as_latex.parameter <- function(x, brackets = "[]",
                              subscript_dims = is.null(brackets), ...) {
  if (is.null(x$name)) {
    return("\\emptyset")
  } else {
    name <- x$name
    # dims <- paste(x$dims, collapse = ",") |> latex_brackets(brackets)
    dims <- as_latex(x$dims, brackets = brackets, ...)
    if (subscript_dims) {
      dims <- paste0("_{", dims, "}")
    } else {
      dims <- paste0("(", dims, ")")
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
    # dims <- paste(x$dims, collapse = ",") |> latex_brackets(brackets)
    dims <- as_latex(x$dims, brackets = brackets, sbscript_dims = subscript_dims, ...)
    if (subscript_dims) {
      dims <- paste0("_{", dims, "}")
    } else {
      dims <- paste0("(", dims, ")")
    }
    return(paste0("\\mathit{\\bf ", name, "}", dims, ""))
  }
}

#' @export
#' @method as_latex symbol
#' @rdname as_latex
as_latex.symbol <- function(x, ...) {
  paste0("\\texttt{", x$value, "}")
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
  lhs <- x$lhs
  rhs <- x$rhs

  if (!inherits(lhs, "ast")) {
    warning("lhs in expression is not an AST: fallback to str()")
    lhs <- paste(capture.output(str(lhs)), collapse = "")
  } else {
    lhs <- as_latex(lhs, ...)
  }

  if (!inherits(rhs, "ast")) {
    warning("rhs in expression is not an AST: fallback to str()")
    rhs <- paste(capture.output(str(rhs)), collapse = "")
  } else {
    rhs <- as_latex(rhs, ...)
  }

  op <- latex_operators[[x$op]]
  if (is.null(op)) stop("Unknown operator: ", x$op)

  out <- paste0(lhs, " ", op, " ", rhs)
  latex_brackets(out, brackets)
}



as_latex0.expression <- function(x, brackets = NULL, ...) {
  lhs <- as_latex(x$lhs, brackets = NULL, ...)
  rhs <- as_latex(x$rhs, brackets = NULL, ...)
  op <- latex_operators[[x$op]]
  if (is.null(op)) {
    stop("Unrecognized operator: ", x$op)
  }
  out <- paste0(lhs, " ", op, " ", rhs)
  out <- latex_brackets(out, brackets)
  return(out)
}

#' @export
#' @method as_latex when
#' @rdname as_latex
as_latex.when <- function(x, brackets = NULL, ...) {
  # return(NULL)
  # browser()
  cond <- as_latex(x$condition, brackets = brackets, ...)
  then <- as_latex(x$then, brackets = brackets, ...)
  return(paste0(then, "\\quad \\text{if }", cond))
}

#' @export
#' @method as_latex sum
#' @rdname as_latex
as_latex.sum <- function(x, brackets = NULL, ...) {
  index <- x$index
  domain <- if (!is.null(x$domain)) {
    paste0(",\\; ", as_latex(x$domain, brackets = brackets, ...))
  } else {""}
  body <- as_latex(x$value, brackets = brackets, ...)
  return(paste0("\\sum_{", index, domain, "} ", body))
}

#' @export
#' @method as_latex prod
#' @rdname as_latex
as_latex.prod <- function(x, brackets = NULL, ...) {
  index <- x$index
  domain <- if (!is.null(x$domain)) {
    paste0(",\\; ", as_latex(x$domain, brackets = brackets, ...))
   } else {""}
  body <- as_latex(x$value, brackets = brackets, ...)
  return(paste0("\\prod_{", index, domain, "} ", body))
}

# export
# method as_latex where
# as_latex.where <- function(x, inline_where = NULL, ...) {
#   if (is.null(inline_where)) {
#     inline_where <- getOption("multimod.render_where_inline", FALSE)
#   }
#
#   # Defensive: ensure `x$name` is present
#   if (inline_where || is.null(x$content)) {
#     return(as_latex(x$content, inline_where = inline_where, ...))
#   }
#
#   if (!is.null(x$name)) {
#     return(paste0("\\texttt{", x$name, "}"))
#   } else {
#     warning("Missing `name` in ast_where object; falling back to hash.")
#     return(paste0("\\texttt{", substr(x$hash, 1, 8), "}"))
#   }
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
    paste0("\\texttt{", x$name, "}")
  } else {
    paste0("\\texttt{", substr(x$hash, 1, 8), "}")
  }
}

# extract_where_nodes <- function(ast) {
#   result <- list()
#
#   walk <- function(x) {
#     if (inherits(x, "where")) {
#       if (!is.null(x$name) && !is.null(x$content)) {
#         result[[x$name]] <<- x$content
#       }
#     }
#     if (is.list(x) && !is.data.frame(x)) {
#       lapply(x, walk)
#     }
#   }
#
#   walk(ast)
#   result
# }

# as_latex.where <- function(x, inline_where = NULL, ...) {
#   if (is.null(inline_where)) {
#     inline_where <- getOption("multimod.render_where_inline", FALSE)
#   }
#
#   if (inline_where || is.null(x$content)) {
#     return(as_latex(x$content, inline_where = inline_where, ...))
#   }
#
#   if (!is.null(x$name)) {
#     paste0("\\texttt{", x$name, "}")
#   } else {
#     warning("Missing `name` in ast_where object; falling back to hash.")
#     paste0("\\texttt{", substr(x$hash, 1, 8), "}")
#   }
# }


# @export
# @method as_latex equation
# @rdname as_latex
#
# as_latex0.equation <- function(x,
#                               # math_env = "align*",
#                               math_env = "dmath",
#                               brackets_dims = NULL,
#                               subscript_dims = is.null(brackets_dims),
#                               inline_where = NULL,
#                               ...) {
#   # browser()
#   # fallback for inline_where
#   if (is.null(inline_where)) {
#     inline_where <- getOption("multimod.render_where_inline", FALSE)
#   }
#
#   lhs <- as_latex(x$lhs,
#                   brackets = brackets_dims,
#                   subscript_dims = subscript_dims,
#                   inline_where = inline_where,
#                   ...)
#   rhs <- as_latex(x$rhs,
#                   brackets = brackets_dims,
#                   subscript_dims = subscript_dims,
#                   inline_where = inline_where,
#                   ...)
#   rel <- switch(x$relation, `==` = "=", `<=` = "\\le", `>=` = "\\ge", x$relation)
#
#   # Wrap relation line
#   body <- paste0(lhs, " ", rel, " ", rhs)
#
#   # Optional: equation name and domain info
#   # browser()
#   preamble <- character()
#   if (!is.null(x$name)) {
#     preamble <- c(
#       preamble,
#       paste0("\\textbf{\\bf Equation:}~\\texttt{", x$name, "}",
#              ifelse(subscript_dims,
#                     paste0("$_{",
#                            as_latex(x$dims, brackets = brackets_dims,
#                                     subscript_dims = subscript_dims, ...),
#                            "}$"),
#                     "")))
#   }
#   if (!is.null(x$desc)) {
#     preamble <- c(preamble, paste0("\\quad—\\textit{", as_latex(x$desc), "}"))
#   }
#   if (!is.null(x$domain)) {
#     # domain_idx <- as_latex(x$domain, brackets = brackets_dims,
#     #                        subscript_dims = subscript_dims, ...)
#     preamble <- c(
#       preamble,
#       paste0("\\\\\n\\text{Domain: }~$",
#              as_latex(x$domain, brackets = brackets_dims,
#                       subscript_dims = subscript_dims, ...),
#              # ifelse(subscript_dims, paste0("_{", domain_idx, "}"), domain_idx),
#              "$"))
#   }
#
#   # Format environments
#   if (length(math_env) > 0 && nzchar(math_env[1])) {
#     begin_env <- paste0("\\begin{", math_env[1], "}")
#     end_env   <- paste0("\\end{", math_env[1], "}")
#   } else {
#     begin_env <- ""
#     end_env <- ""
#   }
#
#   # adding where lines
#   # Gather "where" clauses
#   where_map <- extract_where_nodes(x)
#   where_lines <- character()
#   if (!inline_where) {
#     where_map <- extract_where_nodes(x)
#     if (length(where_map) > 0) {
#       where_lines <- c("\\textbf{where:} \\\\")
#       # for (nm in names(where_map)) {
#       #   def <- as_latex(where_map[[nm]], inline_where = inline_where, ...)
#       #   where_lines <- c(where_lines, paste0("\\texttt{", nm, "} = ", def, "\\\\"))
#       # }
#       for (nm in names(where_map)) {
#         def <- as_latex(where_map[[nm]], inline_where = TRUE, ...)
#         where_lines <- c(where_lines, paste0("\\texttt{", nm, "} = ", def, "\\\\"))
#       }
#     }
#   }
#
#
#   # Combine
#   # out <- c(
#   #   "\\begin{flushleft}",
#   #   paste(preamble, collapse = " "),
#   #   begin_env,
#   #   body,
#   #   where_lines,
#   #   end_env,
#   #   "\\end{flushleft}"
#   # )
#
#   out <- c(
#     "\\begin{flushleft}",
#     paste(preamble, collapse = " "),
#     begin_env,
#     body,
#     end_env,
#     where_lines,
#     "\\end{flushleft}"
#   )
#
#   paste(out, collapse = "\n")
# }

#' @export
#' @method as_latex equation
as_latex.equation <- function(x,
                              math_env = "dmath",
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
  body <- paste0(lhs, " ", rel, " ", rhs)

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
    preamble <- c(preamble, paste0("\\quad—\\textit{", as_latex(x$desc), "}"))
  }
  if (!is.null(x$domain)) {
    preamble <- c(
      preamble,
      paste0("\\\\\n\\text{Domain: }~$",
             as_latex(x$domain, brackets = brackets_dims,
                      subscript_dims = subscript_dims, ...), "$")
    )
  }

  # Prepare where: block if needed
  where_lines <- character()
  if (!inline_where) {
    where_map <- extract_where_nodes(x)
    if (length(where_map) > 0) {
      where_lines <- c("\\textbf{where:}", "\\begin{align*}")
      for (nm in names(where_map)) {
        def <- as_latex(where_map[[nm]], inline_where = TRUE, ...)
        where_lines <- c(where_lines,
                         paste0("\\texttt{", nm, "} &= ", def, " \\\\"))
      }
      where_lines <- c(where_lines, "\\end{align*}")
    }
  }

  # Math environment
  begin_env <- if (!is.null(math_env) && nzchar(math_env[1])) paste0("\\begin{", math_env[1], "}") else ""
  end_env   <- if (!is.null(math_env) && nzchar(math_env[1])) paste0("\\end{", math_env[1], "}") else ""

  # Final output
  out <- c(
    "\\begin{flushleft}",
    paste(preamble, collapse = " "),
    begin_env,
    body,
    end_env,
    where_lines,
    "\\end{flushleft}"
  )

  paste(out, collapse = "\n")
}


#' Render a multimod_equation as LaTeX
#'
#' @param eqn A multimod_equation object
#' @param simplify Logical, whether to simplify the equation body
#' @param show_conditions Logical, whether to show $-conditions
#' @param show_mappings Logical, whether to show mapping domains
#' @param math_env Character, math environment name (e.g., "align*")
#' @param annotation_env Character, text environment for annotations (e.g., "flushleft")
#' @param bracket_dims Bracket type to wrap subscripts (NULL, "[]", "{}", "()")
#' @return LaTeX string
#' @export
as_latex2.equation <- function(eqn,
                              simplify = TRUE,
                              show_conditions = FALSE,
                              show_mappings = FALSE,
                              math_env = "align*",
                              annotation_env = "flushleft",
                              bracket_dims = "[]") {

  omitted <- list()

  eq_simplified <- eqn

  if (simplify && !show_conditions) {
    omitted$conditions <- extract_conditions(eq_simplified)
    eq_simplified <- remove_mappings(eq_simplified)
  }

  if (simplify && !show_mappings) {
    omitted$mappings <- extract_conditions(eq_simplified)
    eq_simplified <- remove_mappings(eq_simplified)
  }

  lhs <- as_latex(eq_simplified$lhs, brackets = bracket_dims)
  rhs <- as_latex(eq_simplified$rhs, brackets = bracket_dims)
  rel <- switch(eq_simplified$relation, "==" = "=", "<=" = "\\le", ">=" = "\\ge", eq_simplified$relation)

  # Format header
  header <- paste0("\\textbf{Equation:}~\\texttt{", eqn$name, "}")
  if (!is.null(eqn$dims)) {
    dims_text <- paste0("$_{", paste(eqn$dims, collapse = ","), "}$")
    header <- paste0(header, dims_text)
  }
  if (!is.null(eqn$desc)) {
    header <- paste0(header, " \\quad\\textit{", as_latex.character(eqn$desc), "}")
  }

  # Equation body
  body <- paste0(lhs, " ", rel, " ", rhs)

  latex <- c(
    paste0("\\begin{", annotation_env, "}"),
    paste0(header, " \\"),
    paste0("\\begin{", math_env, "}"),
    body,
    paste0("\\end{", math_env, "}"),
    paste0("\\end{", annotation_env, "}")
  )

  # Add notes for omitted parts
  if (length(omitted) > 0) {
    latex <- c(latex, "\\textit{where:}")
    if (!is.null(omitted$mappings)) {
      for (m in omitted$mappings) {
        latex <- c(latex, paste0("\\quad ", as_latex(m)))
      }
    }
    if (!is.null(omitted$conditions)) {
      for (c in omitted$conditions) {
        latex <- c(latex, paste0("\\quad Conditional on: ", as_latex(c)))
      }
    }
  }

  paste(latex, collapse = "\n")
}

extract_and_replace_conditions <- function(ast, max_length = 80,
                                           types = c("sum", "when", "expression")
                                           ) {
  browser()
  mapping <- list()
  counter <- 1

  simplify <- function(node) {
    browser()
    if (inherits(node, "ast")) {
      type <- node_type(node)
      if (type %in% types) {
        latex_str <- as_latex(node)
        if (nchar(latex_str) > max_length) {
          alias <- paste0("set", counter)
          counter <<- counter + 1
          mapping[[alias]] <<- node
          return(ast_symbol(alias))
        }
      }
      # Recurse into fields of AST node
      for (field in names(node)) {
        node[[field]] <- simplify(node[[field]])
      }
    } else if (is.list(node)) {
      node <- lapply(node, simplify)
    }
    node
  }

  simplified_ast <- simplify(ast)
  list(ast = simplified_ast, mapping = mapping)
}

#' @export
#' @method as_latex equation
as_latex3.equation <- function(eq, ...) {
  # Simplify RHS with optional aliasing of large subtrees
  simpl <- extract_and_replace_conditions(eq$rhs, max_length = 100)
  rhs_str <- as_latex(simpl$ast)
  lhs_str <- as_latex(eq$lhs)

  # Convert relation (e.g., "==" -> "=")
  rel <- eq$relation
  if (rel == "==") rel <- "="
  rel <- latex_operators[[rel]] %||% rel  # fallback

  # Format domain (optional)
  domain_str <- if (!is.null(eq$domain)) {
    paste0("\\text{Domain: }~$", as_latex(eq$domain), "$\\\\\n")
  } else ""

  # Format the index subscript of LHS (for equation header)
  eq_index <- format_index(eq$lhs)

  # Where section (optional mappings)
  where_str <- ""
  if (length(simpl$mapping) > 0) {
    lines <- vapply(names(simpl$mapping), function(name) {
      def <- as_latex(simpl$mapping[[name]])
      paste0("\\texttt{", name, "} = ", def)
    }, character(1))
    where_str <- paste0("\\textbf{where:} \\\\\n", paste(lines, collapse = "\\\\\n"))
  }

  # Combine everything into a LaTeX block
  paste0(
    "\\begin{flushleft}\n",
    "\\textbf{\\bf Equation:}~\\texttt{", eq$name, "}", eq_index,
    " \\quad—\\textit{", eq$desc %||% "", "} \\\\\n",
    domain_str,
    "\\begin{align*}\n",
    lhs_str, " ", rel, " ", rhs_str, "\n",
    "\\end{align*}\n",
    where_str, "\n",
    "\\end{flushleft}"
  )
}

format_index <- function(lhs) {
  dims <- lhs$dims
  if (is.null(dims) || length(dims) == 0) return("")

  dims_str <- as_latex(dims)
  paste0("_{", dims_str, "}")
}





# # Helper: Extract condition expressions (e.g., dollar conditions or domain filters)
# extract_conditions <- function(expr) {
#   if (is.null(expr)) return(list())
#   if (inherits(expr, "when")) {
#     return(c(list(expr$condition), extract_conditions(expr$then)))
#   }
#   if (inherits(expr, "sum") || inherits(expr, "prod")) {
#     return(c(extract_conditions(expr$domain), extract_conditions(expr$value)))
#   }
#   if (inherits(expr, "expression")) {
#     return(c(extract_conditions(expr$lhs), extract_conditions(expr$rhs)))
#   }
#   return(list())
# }

# Helper: Remove mappings and conditions from an expression
# remove_mappings <- function(expr) {
#   if (is.null(expr)) return(NULL)
#   if (inherits(expr, "when")) {
#     return(remove_mappings(expr$then))
#   }
#   if (inherits(expr, "sum") || inherits(expr, "prod")) {
#     expr$domain <- NULL
#     expr$value <- remove_mappings(expr$value)
#     return(expr)
#   }
#   if (inherits(expr, "expression")) {
#     expr$lhs <- remove_mappings(expr$lhs)
#     expr$rhs <- remove_mappings(expr$rhs)
#     return(expr)
#   }
#   return(expr)
# }

#' Remove mapping nodes from an AST (including nested expressions)
#'
#' This function traverses a multimod AST and removes all mapping nodes,
#' including those inside conditions and logical/arithmetic expressions.
#'
#' @param ast A multimod AST object
#' @returns A cleaned AST object with mappings removed (NULL if fully removed)
#' @export
# remove_mappings <- function(ast) {
#   if (is.null(ast)) return(NULL)
#
#   if (inherits(ast, "mapping")) {
#     return(NULL)
#   }
#
#   if (inherits(ast, "when")) {
#     # Recursively remove mappings in the condition
#     then_clean <- remove_mappings(ast$then)
#     cond_clean <- remove_mappings(ast$condition)
#     if (is.null(then_clean)) return(NULL)
#     return(structure(list(type = "when", then = then_clean, condition = cond_clean), class = class(ast)))
#   }
#
#   if (inherits(ast, "expression")) {
#     lhs <- remove_mappings(ast$lhs)
#     rhs <- remove_mappings(ast$rhs)
#
#     # Remove mapping children
#     if (inherits(lhs, "mapping")) lhs <- NULL
#     if (inherits(rhs, "mapping")) rhs <- NULL
#
#     # Handle removed branches
#     if (is.null(lhs) && is.null(rhs)) return(NULL)
#     if (is.null(lhs)) return(rhs)
#     if (is.null(rhs)) return(lhs)
#
#     return(structure(list(type = "expression", op = ast$op, lhs = lhs, rhs = rhs), class = class(ast)))
#   }
#
#   if (inherits(ast, c("sum", "prod"))) {
#     return(structure(
#       list(
#         type = ast$type,
#         index = ast$index,
#         domain = remove_mappings(ast$domain),
#         value = remove_mappings(ast$value)
#       ),
#       class = class(ast)
#     ))
#   }
#
#   return(ast)
# }



# Helper: Collapse multiple LaTeX environments (string or character vector)
begin_math_env <- function(envs) {
  if (is.null(envs)) return("")
  paste(paste0("\\begin{", envs, "}"), collapse = "\n")
}

end_math_env <- function(envs) {
  if (is.null(envs)) return("")
  paste(rev(paste0("\\end{", envs, "}")), collapse = "\n")
}


# as_latex.equation <- function(x) {
#   eqn_label <- paste0("\\textbf{", x$name, "}", as_latex(x$dims))
#   domain <- if (!is.null(x$domain)) paste0(" \\quad \\text{domain: }", as_latex(x$domain)) else ""
#   lhs <- as_latex(x$lhs)
#   rhs <- as_latex(x$rhs)
#   rel <- switch(x$relation, "==" = "=", "<=" = "\\le", ">=" = "\\ge", x$relation)
#
#   paste0(
#     "\\begin{flushleft}\n",
#     eqn_label, domain, " \\\\ \n",
#     "\\begin{align*}\n",
#     lhs, " ", rel, " ", rhs, "\n",
#     "\\end{align*}\n",
#     "\\end{flushleft}"
#   )
# }



#' @export
write_latex <- function(x, file, ...) {
  UseMethod("write_latex")
}



