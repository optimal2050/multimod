render_html <- function(node, title = "Multimod AST Equation", fontsize = "120%") {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Please install the 'htmltools' package.")
  }

  latex_str <- multimod::as_latex(node)

  html <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$script(src = "https://polyfill.io/v3/polyfill.min.js?features=es6"),
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

if (F) {
  ast <- parse_gams_expr("vTechOut(tech, comm, region, year, slice) / pTechUse2cact(tech, comm, region, year, slice)", symbols)
  render_html(ast, title = "Technology Output Equation")

}

render_equation_html <- function(eqn,
                                     title = NULL,
                                     fontsize = "120%",
                                     max_total_width = 80,
                                     max_side_width = 50,
                                     indent = "  ",
                                     symbol_map = NULL) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Please install the 'htmltools' package.")
  }

  # Internal: flatten latex AST -> latex string
  lhs_str <- as_latex(eqn$lhs)
  rhs_str <- as_latex(eqn$rhs)
  relation <- switch(eqn$relation,
                  "==" = "=",
                  "<=" = "\\leq",
                  ">=" = "\\geq",
                  eqn$relation)

  # Inject formatted latex equation (uses the enhanced formatter)
  formatted_eqn <- format_latex_equation_split(
    lhs = lhs_str,
    rhs = rhs_str,
    relation = relation,
    max_total_width = max_total_width,
    max_side_width = max_side_width,
    indent = indent,
    symbol_map = symbol_map
  )

  math_block <- paste0(
    "\\[\\begin{array}{l}\n\\begin{aligned}\n",
    formatted_eqn,
    "\n\\end{aligned}\n\\end{array}\\]"
  )

  domain_text <- if (!is.null(eqn$domain)) {
    paste0("Subject to: \\(", eqn$domain$name, "(", paste(eqn$domain$dims, collapse = ", "), ")\\)")
  } else NULL

  html <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$script(src = "https://polyfill.io/v3/polyfill.min.js?features=es6"),
      htmltools::tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
    ),
    htmltools::tags$body(
      htmltools::tags$h2(title %||% eqn$name),
      htmltools::tags$h4(paste("Declared over:", paste(eqn$dims, collapse = ", "))),
      if (!is.null(domain_text)) htmltools::tags$p(htmltools::HTML(domain_text)),
      htmltools::tags$div(
        style = paste("font-size:", fontsize, ";"),
        htmltools::HTML(math_block)
      )
    )
  )

  htmltools::browsable(html)
}
if (F) {
  render_equation_html(eq_obj,
                           title = "Technology Input–Output Equation",
                           max_total_width = 80,
                           max_side_width = 45,
                           symbol_map = list(
                             t = "tech", c = "comm", cp = "commp", r = "region", y = "year", s = "slice",
                             pTechCinp2use = "pC2use", pTechUse2cact = "pUse2a", pTechCact2cout = "pA2out"
                           )
  )

}
