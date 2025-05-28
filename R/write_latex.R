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


#' @export
write_latex <- function(x, file, ...) {
  UseMethod("write_latex")
}
#' @export
#' @method write_latex equation
write_latex.equation <- function(x,
                                 file,
                                 append = FALSE,
                                 standalone = !append,
                                 preamble = NULL,
                                 ending = NULL,
                                 subsection_number = FALSE,
                                 ...) {

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

#' @export
#' @method write_latex model
write_latex.model <- function(x,
                              file = NULL,
                              append = FALSE,
                              preamble = NULL,
                              ending = NULL,
                              # math_env = "equation",
                              subsection_number = TRUE,
                              include_sets = TRUE,
                              include_aliases = TRUE,
                              include_parameters = TRUE,
                              include_variables = TRUE,
                              include_equations = TRUE,
                              ...) {
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
  if (!is.null(x$name)) {
    meta_block <- c(meta_block, paste0("\\section{Model: ", x$name, "}"))
  }
  if (!is.null(x$desc)) {
    meta_block <- c(meta_block, paste0("\\textit{", x$desc, "}"))
  }

  lines <- character()
  if (include_sets && !is.null(model$sets)) {
    lines <- c(lines, "", "\\section{Sets}")
    for (s in model$sets) {
      lines <- c(lines, paste0("\\texttt{", s$name, "} -- ",
                               as_latex(s$desc), "\\\\"))
    }
  }

  if (include_parameters && !is.null(model$parameters)) {
    lines <- c(lines, "", "\\section{Parameters}")
    for (p in model$parameters) {
      # lines <- c(lines, paste0("\\texttt{", p$name, "}(",
      #                          paste(p$dims, collapse = ","), ") -- ",
      #                          as_latex(p$desc), "\\\\"))
      lines <- c(lines, paste0("$", as_latex(p), "$ -- ",
                               as_latex(p$desc), "\\\\"))
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
      lines <- c(lines, paste0("$", as_latex(v), "$ -- ", as_latex(v$desc), "\\\\"))
    }
  }

  # Render all equations using as_latex()
  eq_blocks <- vapply(
    x$equations,
    function(eq) {
      message(eq$name)
      # eq <- remap_ast_elements(eq)
      eq_tex <- as_latex(eq,
                         # math_env = math_env,
                         subsection_number = subsection_number, ...)
      eq_tex <- paste(eq_tex, "\n")
      },
    character(1)
  )
  lines <- c(lines, eq_blocks)

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
