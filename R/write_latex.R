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
                                 ...) {
  # Default LaTeX preamble and ending
  default_preamble <- c(
    "\\documentclass{article}",
    "\\usepackage[a4paper,margin=1in]{geometry}",
    "\\usepackage{amsmath,amssymb}",
    "\\usepackage{breqn}",
    "\\usepackage{graphicx}",
    "\\usepackage{longtable}",
    "\\begin{document}"
  )
  default_ending <- "\\end{document}"

  # Generate LaTeX body
  latex_code <- as_latex(x, ...)

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
                              math_env = "equation",
                              ...) {
  if (is.null(preamble)) {
    preamble <- c(
      "\\documentclass{article}",
      "\\usepackage[a4paper,margin=1in]{geometry}",
      "\\usepackage{amsmath}",
      "\\usepackage{amssymb}",
      "\\usepackage{graphicx}",
      "\\usepackage{breqn}",
      "\\begin{document}"
    )
  }

  if (is.null(ending)) {
    ending <- "\\end{document}"
  }

  # Header with model name/description
  meta_block <- character()
  if (!is.null(x$name)) {
    meta_block <- c(meta_block, paste0("\\section*{Model: ", x$name, "}"))
  }
  if (!is.null(x$desc)) {
    meta_block <- c(meta_block, paste0("\\textit{", x$desc, "}"))
  }

  # Render all equations using as_latex()
  eq_blocks <- vapply(
    x$equations,
    function(eq) {
      message(eq$name)
      eq_tex <- as_latex(eq, math_env = math_env, ...)
      eq_tex <- paste(eq_tex, "\n")
      },
    character(1)
  )

  # Assemble full LaTeX content
  body <- c(meta_block, eq_blocks)
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
