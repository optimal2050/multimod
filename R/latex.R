render_multimod_eqn_to_latex <- function(eqn) {
  # Helper to render an expression recursively
  render_expr <- function(expr) {
    if (is.null(expr)) return("")
    switch(expr$type,
           "expr" = {
             left <- render_expr(expr$left)
             right <- render_expr(expr$right)
             op <- switch(expr$op,
                          "*" = " \\cdot ",
                          "/" = " \\div ",
                          "+" = " + ",
                          "-" = " - ",
                          expr$op)
             paste0("(", left, op, right, ")")
           },
           "sum" = {
             index <- expr$index
             domain <- if (!is.null(expr$domain)) paste0(",\\; ", render_expr(expr$domain)) else ""
             body <- render_expr(expr$value)
             paste0("\\sum_{", index, domain, "} ", body)
           },
           "prod" = {
             index <- expr$index
             domain <- if (!is.null(expr$domain)) paste0(",\\; ", render_expr(expr$domain)) else ""
             body <- render_expr(expr$value)
             paste0("\\prod_{", index, domain, "} ", body)
           },
           "cond" = {
             cond <- render_expr(expr$condition)
             then <- render_expr(expr$then)
             paste0(then, "\\quad \\text{if }", cond)
           },
           "var" = {
             args <- paste(expr$args, collapse = ",")
             paste0("\\mathit{", expr$name, "}(", args, ")")
           },
           "param" = {
             args <- paste(expr$args, collapse = ",")
             paste0("\\mathsf{", expr$name, "}(", args, ")")
           },
           "symbol" = {
             paste0("\\texttt{", expr$value, "}")
           },
           "compare" = {
             left <- render_expr(expr$left)
             right <- render_expr(expr$right)
             op <- paste0(" ", expr$op, " ")
             paste0(left, op, right)
           },
           "logic" = {
             left <- render_expr(expr$left)
             right <- render_expr(expr$right)
             op <- paste0(" \\text{", expr$op, "} ")
             paste0("(", left, op, right, ")")
           },
           paste0("[Unsupported type: ", expr$type, "]")
    )
  }

  lhs <- render_expr(eqn$lhs)
  rhs <- render_expr(eqn$rhs)
  sense <- switch(eqn$sense,
                  "==" = "=",
                  "<=" = "\\leq",
                  ">=" = "\\geq",
                  eqn$sense
  )

  eq_label <- paste0("\\textbf{", eqn$name, "}(", paste(eqn$declared_dims, collapse = ", "), ")")
  body <- paste(lhs, sense, rhs)

  paste0("\\begin{align*}\n",
         eq_label, " &:\\quad ", body, "\n",
         "\\end{align*}")
}


export_multimod_to_latex <- function(model, file = "model_equations.tex", title = "Model Equations") {
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
    latex <- render_multimod_eqn_to_latex(eqn)
    lines <- c(lines, latex, "")
  }

  lines <- c(lines, "\\end{document}")
  writeLines(lines, con = file)
  message("LaTeX file written to: ", normalizePath(file))
}

if (F) {

  symbols <- build_symbols_list(model_info)

  mod_eqn <- coerce_equation(
    model_info$equations[[1]],
    symbols)

  render_multimod_eqn_to_latex(mod_eqn)

  export_multimod_to_latex(multimod_model, "tmp/multimod_model.tex")
}
