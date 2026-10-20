# =============================================================================
# JuMP Solver Tests - solve_jump() function
# =============================================================================
#
# This test suite validates the solve_jump() function for solving multimod
# models using JuMP (Julia for Mathematical Programming).
#
# Tests cover:
# • Basic solve functionality with UTOPIA model
# • Solving from model_dir without model object
# • Verbose output mode
# • Error handling with missing files
# • Objective value consistency across runs
#
# solve_jump() workflow:
# 1. Launch Julia process with model.jl script
# 2. Julia loads data (Arrow IPC format)
# 3. Julia builds JuMP model and solves with HiGHS
# 4. Julia writes solution to CSV files
# 5. R extracts solution (objective, status, timing)
# 6. Optionally load results back into model object
#
# Method options:
# - "system": Call Julia via system command (default)
# - "JuliaConnectoR": Use JuliaConnectoR package for direct interface
#
# Dependencies:
# - Julia with JuMP.jl, HiGHS.jl, Arrow.jl packages
# - arrow package (R) for IPC format
# - save_model, write_jump for model preparation
# =============================================================================

test_that("solve_jump solves UTOPIA model successfully", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Create temp directory
  temp_dir <- tempfile("jump_solve_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Save model and generate JuMP code
  save_model(demo_model, temp_dir, format = "ipc", verbose = FALSE)
  write_jump(demo_model, model_dir = temp_dir)
  
  # Solve with default method (system)
  result <- solve_jump(demo_model, temp_dir, 
                       method = "system", verbose = FALSE, load_results = FALSE)
  
  # Check result structure
  expect_type(result, "list")
  expect_true("success" %in% names(result), label = "Result should have 'success' field")
  expect_true("objective" %in% names(result), label = "Result should have 'objective' field")
  expect_true("solve_time" %in% names(result), label = "Result should have 'solve_time' field")
  
  # Check solution success
  expect_true(result$success, label = "Solver should succeed")
  expect_false(is.na(result$objective), label = "Objective should not be NA")
  expect_gt(result$objective, 0, label = "Objective should be positive")
  
  # Check that solver output files exist
  julia_dir <- file.path(temp_dir, "solvers", "jump")
  expect_true(file.exists(file.path(julia_dir, "model.jl")))
})

test_that("solve_jump handles model_dir without model object", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Create temp directory
  temp_dir <- tempfile("jump_nomodel_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Save and generate
  save_model(demo_model, temp_dir, format = "ipc", verbose = FALSE)
  write_jump(demo_model, model_dir = temp_dir)
  
  # Solve without model object (model = NULL)
  result <- solve_jump(model = NULL, model_dir = temp_dir,
                       method = "system", verbose = FALSE)
  
  expect_true(result$success)
  expect_false(is.na(result$objective))
  
  # Should not have loaded results (no model provided)
  expect_false("model" %in% names(result))
})

test_that("solve_jump verbose output works", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  temp_dir <- tempfile("jump_verbose_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  save_model(demo_model, temp_dir, format = "ipc", verbose = FALSE)
  write_jump(demo_model, model_dir = temp_dir)
  
  # Capture output with verbose = TRUE
  output <- capture.output({
    result <- solve_jump(demo_model, temp_dir,
                         method = "system", verbose = TRUE, load_results = FALSE)
  })
  
  expect_true(result$success)
  expect_gt(length(output), 0, label = "Should produce verbose output")
})

test_that("solve_jump fails gracefully with missing files", {
  skip_on_cran()
  
  temp_dir <- tempfile("jump_missing_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Try to solve without any model files
  expect_error(
    solve_jump(model = NULL, model_dir = temp_dir, verbose = FALSE),
    regexp = "Julia model not found",
    label = "Should error when model.jl is missing"
  )
})

test_that("solve_jump returns consistent objective value", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("jump_consistent_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  save_model(demo_model, temp_dir, format = "ipc", verbose = FALSE)
  write_jump(demo_model, model_dir = temp_dir)
  
  # Solve twice
  result1 <- solve_jump(demo_model, temp_dir, 
                        method = "system", verbose = FALSE, load_results = FALSE)
  result2 <- solve_jump(demo_model, temp_dir,
                        method = "system", verbose = FALSE, load_results = FALSE)
  
  expect_equal(result1$objective, result2$objective, tolerance = 1e-6,
               label = "Should get identical objective on repeated solves")
})

