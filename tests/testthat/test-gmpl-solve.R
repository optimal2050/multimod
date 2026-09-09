# =============================================================================
# GMPL Solver Tests - solve_gmpl() function
# =============================================================================
#
# This test suite validates the solve_gmpl() function for solving multimod
# models using GMPL (GNU MathProg Language) via glpkAPI.
#
# Tests cover:
# • Basic solve functionality with UTOPIA model
# • Solving from model_dir without model object
# • Verbose output mode
# • Error handling with missing files
# • Objective value consistency across runs
# • Status reporting (optimal, infeasible, unbounded, etc.)
#
# solve_gmpl() workflow:
# 1. Read GMPL code (model.mod) and data (data.dat)
# 2. Build problem with glpkAPI
# 3. Solve with specified method (simplex/interior/mixed)
# 4. Extract solution (objective, status, timing)
# 5. Optionally load results back into model object
#
# Dependencies:
# - glpkAPI package (GLPK solver interface)
# - save_model, write_gmpl for model preparation
#
# Note: Warning tracking is enabled to catch excessive warnings that may
# indicate underlying issues (stops after 50 warnings).
# =============================================================================

# Track warnings
warning_log <- character()
warning_handler <- function(w) {
  warning_log <<- c(warning_log, conditionMessage(w))
  if (length(warning_log) > 50) {
    cat("ERROR: Too many warnings (", length(warning_log), "). First 10:\n")
    print(head(warning_log, 10))
    stop("Test aborted due to excessive warnings")
  }
  invokeRestart("muffleWarning")
}

withCallingHandlers({

test_that("solve_gmpl solves UTOPIA model successfully", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Create temp directory
  temp_dir <- tempfile("gmpl_solve_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Save model and generate GMPL code
  save_model(demo_model, temp_dir, format = "csv", verbose = FALSE)
  write_gmpl(demo_model, model_dir = temp_dir)
  
  # Solve with glpsol
  result <- solve_gmpl(demo_model, temp_dir, verbose = FALSE, load_results = FALSE)
  
  # Check result structure
  expect_type(result, "list")
  expect_true("success" %in% names(result), label = "Result should have 'success' field")
  expect_true("objective" %in% names(result), label = "Result should have 'objective' field")
  expect_true("solve_time" %in% names(result), label = "Result should have 'solve_time' field")
  expect_true("status" %in% names(result), label = "Result should have 'status' field")
  
  # Check solution success
  expect_true(result$success, label = "Solver should succeed")
  expect_false(is.na(result$objective), label = "Objective should not be NA")
  expect_gt(result$objective, 0, label = "Objective should be positive")
  
  # Check that solver output files exist
  gmpl_dir <- file.path(temp_dir, "solvers", "gmpl")
  expect_true(file.exists(file.path(gmpl_dir, "model.mod")))
  expect_true(file.exists(file.path(gmpl_dir, "data.dat")))
  expect_true(dir.exists(file.path(gmpl_dir, "solution")))
  
  # Check for variable data
  if ("variables" %in% names(result)) {
    expect_s3_class(result$variables, "data.frame")
    expect_gt(nrow(result$variables), 0, label = "Should have parsed variable values")
  }
})

test_that("solve_gmpl handles model_dir without model object", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Create temp directory
  temp_dir <- tempfile("gmpl_nomodel_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Save and generate
  save_model(demo_model, temp_dir, format = "csv", verbose = FALSE)
  write_gmpl(demo_model, model_dir = temp_dir)
  
  # Solve without model object (model = NULL)
  result <- solve_gmpl(model = NULL, model_dir = temp_dir, verbose = FALSE)
  
  expect_true(result$success)
  expect_false(is.na(result$objective))
  
  # Should not have loaded results (no model provided)
  expect_false("model" %in% names(result))
})

test_that("solve_gmpl verbose output works", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("gmpl_verbose_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  save_model(demo_model, temp_dir, format = "csv", verbose = FALSE)
  write_gmpl(demo_model, model_dir = temp_dir)
  
  # Capture output with verbose = TRUE
  output <- capture.output({
    result <- solve_gmpl(demo_model, temp_dir, verbose = TRUE, load_results = FALSE)
  })
  
  expect_true(result$success)
  expect_gt(length(output), 0, label = "Should produce verbose output")
})

test_that("solve_gmpl fails gracefully with missing files", {
  skip_on_cran()
  
  temp_dir <- tempfile("gmpl_missing_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Try to solve without any model files
  expect_error(
    solve_gmpl(model = NULL, model_dir = temp_dir, verbose = FALSE),
    regexp = "GMPL directory not found",
    label = "Should error when GMPL directory is missing"
  )
})

test_that("solve_gmpl returns consistent objective value", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("gmpl_consistent_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  save_model(demo_model, temp_dir, format = "csv", verbose = FALSE)
  write_gmpl(demo_model, model_dir = temp_dir)
  
  # Solve twice
  result1 <- solve_gmpl(demo_model, temp_dir, verbose = FALSE, load_results = FALSE)
  result2 <- solve_gmpl(demo_model, temp_dir, verbose = FALSE, load_results = FALSE)
  
  expect_equal(result1$objective, result2$objective, tolerance = 1e-6,
               label = "Should get identical objective on repeated solves")
})

test_that("solve_gmpl status reporting works correctly", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("gmpl_status_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  save_model(demo_model, temp_dir, format = "csv", verbose = FALSE)
  write_gmpl(demo_model, model_dir = temp_dir)
  
  result <- solve_gmpl(demo_model, temp_dir, verbose = FALSE, load_results = FALSE)
  
  expect_true(result$success)
  expect_true(grepl("OPTIMAL|FEASIBLE", result$status, ignore.case = TRUE),
              label = "Status should indicate optimal or feasible solution")
})

}, warning = warning_handler)

# Report warning count at end
if (length(warning_log) > 0) {
  cat("\nTotal warnings in test file:", length(warning_log), "\n")
  if (length(warning_log) <= 20) {
    cat("Unique warnings:\n")
    print(unique(warning_log))
  }
}

