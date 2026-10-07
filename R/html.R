render_html <- function(node, title = "Multimod AST Equation", fontsize = "120%") {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Please install the 'htmltools' package.")
  }

  latex_str <- multimod::as_latex(node)

  html <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
    ),
    htmltools::tags$body(
      htmltools::tags$h2(title),
      htmltools::tags$div(style = paste("font-size:", fontsize, ";"),
                          htmltools::HTML(paste0("$$", latex_str, "$$"))
      )
    )
  )

  htmltools::browsable(html)
}


