# Default LaTeX preamble and ending
default_preamble <- c(
  "\\documentclass{article}",
  # "\\usepackage[a4paper,margin=1in]{geometry}",
  "\\usepackage[letterpaper,margin=1in]{geometry}",
  "\\usepackage{amsmath,amssymb}",
  "\\usepackage{breqn}",
  "\\usepackage{graphicx}",
  "\\usepackage{longtable}",
  "\\usepackage{adjustbox}",
  "\\usepackage{multicol}",
  "\\usepackage{bm}",
  "\\usepackage[mathcal]{eucal}",
  "\\usepackage{tikz}",
  "\\begin{document}"
)
default_ending <- "\\end{document}"

#' Write LaTeX representation of an equation or model
#'
#' @param x An object of class `equation` or `model`.
#'
#' @param file Path to the output LaTeX file. If `NULL`, returns the LaTeX code as a character string.
#' @param ... Additional arguments passed to the specific method.
#'
#' @export
write_latex <- function(x, file, ...) {
  UseMethod("write_latex")
}

#' Write LaTeX representation of an equation
#'
#' @param append Logical; if `TRUE`, appends to the file instead of overwriting.
#' @param standalone Logical; if `TRUE`, writes a complete LaTeX document with preamble and ending.
#' @param preamble Character vector; LaTeX preamble to use. If `NULL`, uses the default preamble.
#' @param ending Character vector; LaTeX ending to use. If `NULL`, uses the default ending.
#' @param subsection_number Logical; if `TRUE`, includes subsection numbering in the LaTeX output.
#' @param eq_substitute Named list; substitutions for specific AST elements in the equation
#' with `ast_where` object to display conditions, indices, etc. below the equation.
#' @param verbose Logical; if `TRUE`, prints additional information during processing.
#' @param ... Additional arguments passed to the `as_latex` function for rendering the equation.
#'
#' @export
#' @method write_latex equation
write_latex.equation <- function(x,
                                 file,
                                 append = FALSE,
                                 standalone = !append,
                                 preamble = NULL,
                                 ending = NULL,
                                 subsection_number = FALSE,
                                 eq_substitute = list("when" = "condition"),
                                                      # "func" = "index",
                                                      # "sum" = "index",
                                                      # "prod" = "index"),
                                 alias_map = NULL,
                                 verbose = FALSE,
                                 ...) {

  # browser()
  if (!is_empty(eq_substitute)) {
    # Substitute elements in the AST
    x <- remap_ast_elements(x,
                            ast_type = eq_substitute,
                            verbose = verbose, ...)
  }

  # Apply alias mapping if provided
  if (!is.null(alias_map)) {
    x <- alias_ast_names(x, alias_map, verbose = verbose, ...)
  }

  if (is.null(preamble)) {
    # Default LaTeX preamble
    preamble <- default_preamble
  }
  if (is.null(ending)) {
    # Default LaTeX ending
    ending <- default_ending
  }

  # Generate LaTeX body
  latex_code <- as_latex(x, subsection_number = subsection_number, ...)

  # Assemble full content if standalone
  content <- character()
  if (standalone) {
    content <- c(
      if (is.null(preamble)) default_preamble else preamble,
      latex_code,
      if (is.null(ending)) default_ending else ending
    )
  } else {
    content <- latex_code
  }

  # Open connection manually to control append behavior
  con <- file(file, open = if (append) "a" else "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  writeLines(content, con = con, sep = "\n")

  invisible(file)
}

#' Write LaTeX representation of a model
#'
#' @inherit write_latex.equation
#'
#' @export
#' @method write_latex model
write_latex.model <- function(x,
                              file = NULL,
                              append = FALSE,
                              preamble = NULL,
                              ending = NULL,
                              title = paste0("Model: ", x$name),
                              subtitle = x$desc,
                              author = x$authors,
                              show_date = TRUE,
                              # math_env = "equation",
                              subsection_number = TRUE,
                              include_sets = TRUE,
                              include_aliases = TRUE,
                              include_parameters = TRUE,
                              include_variables = TRUE,
                              include_equations = TRUE,
                              eq_substitute = list("when" = "condition"
                                                   # "func" = "index",
                                                   # "sum" = "index",
                                                   # "prod" = "index"
                                                   ),
                              alias_map = NULL,
                              verbose = FALSE,
                              ...) {
  # browser()
  if (!is_empty(eq_substitute)) {
    # Substitute elements in the AST
    x$equations <- lapply(x$equations, remap_ast_elements,
                            ast_type = eq_substitute,
                            verbose = verbose, ...)
  }

  # Apply alias mapping if provided
  if (!is.null(alias_map)) {
    # !!! ToDo: apply to other model components and add to aliases
    # !!! if TRUE then use shortest alias name
    x$equations <- lapply(x$equations, alias_ast_names, alias_map = alias_map,
                          verbose = verbose, ...)
  }

  model <- x
  if (is.null(preamble)) {
    # Default LaTeX preamble
    preamble <- default_preamble
  }

  if (is.null(ending)) {
    # Default LaTeX ending
    ending <- default_ending
  }

  # Header with model name/description
  meta_block <- character()
  if (!is.null(title)) {
    meta_block <- c(
      meta_block,
      paste0("\\title{",
             title,
             paste0("\\\\\\large ", as_latex(subtitle), "}"))
    )
  }
  if (!is.null(author)) {
    meta_block <- c(meta_block, paste0("\\author{", author, "}"))
  }
  if (show_date) {
    meta_block <- c(meta_block, "\\date{\\today}")
  }
  meta_block <- c(meta_block, "\\maketitle")

  lines <- character()

  ## Sets ####
  if (include_sets && !is.null(model$sets)) {
    lines <- c(lines, "", "\\section{Sets}")
    for (s in model$sets) {
      set_lx <- paste0("\\texttt{", s$name, "}")
      if (!is_empty(s$desc)) {
        set_lx <- paste0(set_lx, " -- ", as_latex(s$desc))
      }
      lines <- c(lines,  set_lx, "\\\\")
    }
  }

  if (include_aliases && !is.null(model$aliases)) {
    lines <- c(lines, "", "\\section{Aliases}\\", "\\begin{flushleft}")
    # browser()
    for (a in model$aliases) {
      if (is.character(a)) {
        b <- paste0("\\texttt{", a, "}", collapse = ", ")
        # b <- latex_wrap_brackets(b, brackets = "{}")
        b <- paste0("\\{", b, "\\}")
      } else if (inherits(a, "ast")) {
        b <- as_latex(a, subsection_number = subsection_number, ...)
      } else {
        b <- as.character(a)
      }
      lines <- c(lines, paste0(b, "\\\\"))
    }
    lines <- c(lines, "\\end{flushleft}")
  }


  if (include_parameters && !is.null(model$parameters)) {
    lines <- c(lines, "", "\\section{Parameters}")
    for (p in model$parameters) {
      # lines <- c(lines, paste0("\\texttt{", p$name, "}(",
      #                          paste(p$dims, collapse = ","), ") -- ",
      #                          as_latex(p$desc), "\\\\"))
      p_tex <- paste0("$", as_latex(p), "$")
      if (!is_empty(p$desc) && nzchar(p$desc) > 0) {
        p_tex <- paste0(p_tex, " -- ", as_latex(p$desc))
      }
      lines <- c(lines, paste0(p_tex, "\\\\"))
    }
  }

  if (include_variables && !is.null(model$variables)) {
    lines <- c(lines, "", "\\section{Variables}")
    # for (v in model$variables) {
    #   lines <- c(lines, paste0("\\texttt{", v$name, "}(",
    #                            paste(v$dims, collapse = ","), ") -- ",
    #                            as_latex(v$desc), "\\\\"))
    # }
    for (v in model$variables) {
      v_tex <- paste0("$", as_latex(v), "$")
      if (!is_empty(v$desc) && nzchar(v$desc) > 0) {
        v_tex <- paste0(v_tex, " -- ", as_latex(v$desc))
      }
      lines <- c(lines, v_tex, "\\\\")
      # lines <- c(lines, paste0("$", as_latex(v), "$ -- ", as_latex(v$desc), "\\\\"))
    }
  }

  if (include_equations && !is.null(model$equations)) {
    lines <- c(lines, "", "\\section{Equations}\\")
    # Render all equations using as_latex()
    eq_blocks <- vapply(
      x$equations,
      function(eq) {
        if (verbose) message(eq$name)
        # eq <- remap_ast_elements(eq)
        eq_tex <- as_latex(eq,
                           # math_env = math_env,
                           subsection_number = subsection_number, ...)
        eq_tex <- paste(eq_tex, "\n")
        },
      character(1)
    )
    lines <- c(lines, eq_blocks)
  }

  # Assemble full LaTeX content
  body <- c(meta_block, lines)
  full_doc <- c(preamble, "", body, "", ending)
  output <- paste(full_doc, collapse = "\n")

  # Write or return
  if (!is.null(file)) {
    writeLines(output, con = file, useBytes = TRUE)
    return(invisible(file))
  } else {
    return(output)
  }
}
