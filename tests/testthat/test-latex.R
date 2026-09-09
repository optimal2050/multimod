# =============================================================================
# LaTeX Generation Tests - write_latex() and as_latex()
# =============================================================================
#
# This test suite validates the LaTeX generation functionality for converting
# multimod model objects to properly formatted LaTeX documents.
#
# Tests cover:
# • Basic LaTeX generation for models
# • Model view modes (reduced, full, both)
# • Data inclusion and display options
# • Set element formatting and underscore escaping
# • Index alias handling
# • Folded parameter annotations
# • Trimmed element annotations
# • Optimization summary sections
# • Table of contents generation
# • Mappings section visibility
# • Parameter/variable/equation rendering
# • Dimension display (folded vs. original)
#
# LaTeX generation concepts:
# - Model view: Display mode (reduced=skip trimmed, full=show all with annotations)
# - Data detail: Level of detail for data display (brief, detailed, none)
# - Active dimensions: Folded dimensions shown in reduced view
# - Index aliases: Short names for dimensions (e.g., comm → c)
# - Annotations: Visual markers for trimmed/folded elements
#
# Workflow:
# 1. Create or load model (optionally folded/trimmed)
# 2. Generate LaTeX using write_latex()
# 3. Verify structure and content
# 4. Check special features (folding, trimming, aliases)
#
# Dependencies:
# - Model must have valid structure with sets, parameters, variables, equations
# =============================================================================

test_that("write_latex generates valid LaTeX for basic model", {
  # Load test model
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Generate LaTeX
  tex <- write_latex(demo_model, file = NULL)

  # Check basic structure
  expect_type(tex, "character")
  expect_true(nchar(tex) > 1000)
  expect_true(grepl("\\\\documentclass", tex))
  expect_true(grepl("\\\\begin\\{document\\}", tex))
  expect_true(grepl("\\\\end\\{document\\}", tex))
  expect_true(grepl("\\\\maketitle", tex))
})

test_that("write_latex handles model_view modes correctly", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Create folded and trimmed model
  fold_spec <- create_fold_spec(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice"))
  )
  model_opt <- trim_model(fold_model(demo_model, fold_spec))

  # Test reduced view (skip trimmed elements)
  tex_reduced <- write_latex(model_opt, file = NULL, model_view = "reduced")
  expect_true(nchar(tex_reduced) < nchar(write_latex(model_opt, file = NULL, model_view = "full")))

  # Test full view (show trimmed with annotations)
  tex_full <- write_latex(model_opt, file = NULL, model_view = "full")
  expect_true(grepl("trimmed", tex_full))
  expect_true(grepl("textcolor", tex_full))
})

test_that("write_latex includes data correctly", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # With data (brief)
  tex_data <- write_latex(demo_model, file = NULL,
                         include_data = TRUE, data_detail = "brief")
  expect_true(grepl("elements", tex_data))
  expect_true(grepl("rows", tex_data))

  # Without data
  tex_no_data <- write_latex(demo_model, file = NULL,
                            include_data = FALSE)
  expect_true(nchar(tex_no_data) < nchar(tex_data))
})

test_that("write_latex escapes underscores in set elements", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  tex <- write_latex(demo_model, file = NULL, include_data = TRUE)

  # Check that underscores are escaped in italic text
  # Look for patterns like (elements: A\_D, A\_N, ...)
  if (grepl("A_D|RES_", tex)) {
    expect_true(grepl("A\\\\_D|RES\\\\_", tex))
  }
})

test_that("write_latex applies index aliases correctly", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Check if model has index_aliases
  if (!is.null(demo_model$index_aliases) &&
      length(demo_model$index_aliases) > 0) {

    tex <- write_latex(demo_model, file = NULL,
                      use_model_aliases = TRUE)

    # Check that aliases appear
    first_alias <- names(demo_model$index_aliases)[1]
    expect_true(grepl(first_alias, tex))

    # Check Index Aliases section exists
    expect_true(grepl("Index Aliases", tex))
  }
})

test_that("write_latex shows folded annotations", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Create folded model
  fold_spec <- create_fold_spec(
    demo_model,
    fold_dims = list(
      timeslice = list(tech = "mTechTimeslice"),
      region = list(tech = "mTechRegion")
    )
  )
  model_folded <- fold_model(demo_model, fold_spec)

  # Generate full view to see annotations
  tex <- write_latex(model_folded, file = NULL, model_view = "full", folded_color = "green")

  # Check for folded annotations
  expect_true(grepl("folded:", tex))
  expect_true(grepl("textcolor\\{green\\}", tex))
  expect_true(grepl("D \\$\\\\rightarrow\\$ \\d+D", tex))
})

test_that("write_latex includes optimization summary", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Create optimized model
  fold_spec <- create_fold_spec(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice"))
  )
  model_opt <- trim_model(fold_model(demo_model, fold_spec))

  tex <- write_latex(model_opt, file = NULL)

  # Check for optimization summary section
  expect_true(grepl("Model Optimization Summary", tex))
  expect_true(grepl("Folding Applied:", tex))
  expect_true(grepl("Trimming Applied:", tex))

  # Check for explanatory text
  expect_true(grepl("reduces redundancy", tex))
  expect_true(grepl("removes empty and unused", tex))
})

test_that("write_latex includes table of contents when requested", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # With TOC
  tex_toc <- write_latex(demo_model, file = NULL, include_toc = TRUE)
  expect_true(grepl("\\\\tableofcontents", tex_toc))
  expect_true(grepl("\\\\newpage", tex_toc))

  # Without TOC
  tex_no_toc <- write_latex(demo_model, file = NULL, include_toc = FALSE)
  expect_false(grepl("\\\\tableofcontents", tex_no_toc))
})

test_that("write_latex includes mappings section by default", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Default (should include)
  tex_default <- write_latex(demo_model, file = NULL)
  expect_true(grepl("section\\{Mappings\\}", tex_default))

  # Explicitly disabled
  tex_no_map <- write_latex(demo_model, file = NULL, include_mappings = FALSE)
  expect_false(grepl("section\\{Mappings\\}", tex_no_map))
})

test_that("write_latex handles section visibility flags", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # All sections enabled
  tex_all <- write_latex(demo_model, file = NULL,
                        include_sets = TRUE,
                        include_parameters = TRUE,
                        include_variables = TRUE,
                        include_equations = TRUE)
  expect_true(grepl("section\\{Sets\\}", tex_all))
  expect_true(grepl("section\\{Parameters\\}", tex_all))
  expect_true(grepl("section\\{Variables\\}", tex_all))
  expect_true(grepl("section\\{Equations\\}", tex_all))

  # Only equations
  tex_eq_only <- write_latex(demo_model, file = NULL,
                            include_sets = FALSE,
                            include_parameters = FALSE,
                            include_variables = FALSE,
                            include_equations = TRUE)
  expect_false(grepl("section\\{Sets\\}", tex_eq_only))
  expect_false(grepl("section\\{Parameters\\}", tex_eq_only))
  expect_false(grepl("section\\{Variables\\}", tex_eq_only))
  expect_true(grepl("section\\{Equations\\}", tex_eq_only))
})

test_that("split_at_top_level_operators splits at binary '-' like '+'", {
  expr <- "a - b - c - d - e - f"
  parts <- split_at_top_level_operators(expr, max_len = 10)

  expect_true(length(parts) > 1)
  expect_true(any(grepl("^\\s*-", parts[-1])))
})

test_that("split_at_top_level_operators does not split at unary '-'", {
  expr <- "-a + b + c + d + e"
  parts <- split_at_top_level_operators(expr, max_len = 8)

  expect_true(length(parts) > 1)
  expect_true(startsWith(trimws(parts[1]), "-a"))
  expect_false(any(grepl("^\\s*-$", parts)))
})

test_that("split_at_top_level_operators does not split in scientific notation", {
  expr <- "a + 1e-3 + b + c + d + e"
  parts <- split_at_top_level_operators(expr, max_len = 8)

  expect_true(length(parts) > 1)
  expect_false(any(grepl("^\\s*-\\s*3", parts)))
})

test_that("write_latex handles use_folded parameter correctly", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Create folded model
  fold_spec <- create_fold_spec(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice"))
  )
  model_folded <- fold_model(demo_model, fold_spec)

  # Reduced view should use folded dimensions
  tex_folded <- write_latex(model_folded, file = NULL, model_view = "reduced")

  # Full view should use original dimensions
  tex_original <- write_latex(model_folded, file = NULL, model_view = "full")

  # Full view should be longer (more dimensions)
  expect_true(nchar(tex_original) >= nchar(tex_folded))
})

test_that("as_latex handles parameter objects", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  param <- demo_model$parameters[[1]]
  latex_str <- as_latex(param)

  expect_type(latex_str, "character")
  expect_true(nchar(latex_str) > 0)
  expect_true(grepl("\\\\mathsf", latex_str))
})

test_that("as_latex handles variable objects", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  var <- demo_model$variables[[1]]
  latex_str <- as_latex(var)

  expect_type(latex_str, "character")
  expect_true(nchar(latex_str) > 0)
  expect_true(grepl("\\\\bm\\{\\\\mathit", latex_str))
})

test_that("as_latex handles equation objects", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  eq <- demo_model$equations[[1]]
  latex_str <- as_latex(eq)

  expect_type(latex_str, "character")
  expect_true(nchar(latex_str) > 0)
})

test_that("write_latex generates compilable LaTeX", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not available")

  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Create temporary file
  temp_tex <- tempfile(fileext = ".tex")
  temp_pdf <- sub("\\.tex$", ".pdf", temp_tex)

  # Generate LaTeX
  write_latex(demo_model, file = temp_tex)

  # Try to compile
  result <- system2("pdflatex",
                   args = c("-interaction=nonstopmode",
                           "-output-directory", dirname(temp_tex),
                           temp_tex),
                   stdout = FALSE, stderr = FALSE)

  # Check if PDF was created - !!! check
  expect_true(file.exists(temp_pdf))

  # Cleanup
  unlink(c(temp_tex, temp_pdf))
})

test_that("write_latex handles trimmed color parameter", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  model_trimmed <- trim_model(demo_model)

  # Default blue
  tex_blue <- write_latex(model_trimmed, file = NULL,
                         model_view = "full", trimmed_color = "blue")
  expect_true(grepl("textcolor\\{blue\\}", tex_blue))

  # Custom red
  tex_red <- write_latex(model_trimmed, file = NULL,
                        model_view = "full", trimmed_color = "red")
  expect_true(grepl("textcolor\\{red\\}", tex_red))
})

test_that("write_latex preserves model structure", {
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  tex <- write_latex(demo_model, file = NULL)

  # Check order of sections
  sets_pos <- regexpr("\\\\section\\{Sets\\}", tex)[1]
  params_pos <- regexpr("\\\\section\\{Parameters\\}", tex)[1]
  vars_pos <- regexpr("\\\\section\\{Variables\\}", tex)[1]
  eqs_pos <- regexpr("\\\\section\\{Equations\\}", tex)[1]

  # Sets should come before parameters
  if (sets_pos > 0 && params_pos > 0) {
    expect_true(sets_pos < params_pos)
  }

  # Parameters should come before variables
  if (params_pos > 0 && vars_pos > 0) {
    expect_true(params_pos < vars_pos)
  }

  # Variables should come before equations
  if (vars_pos > 0 && eqs_pos > 0) {
    expect_true(vars_pos < eqs_pos)
  }
})

