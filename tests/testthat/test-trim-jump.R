# =============================================================================
# Trimming and Folding Integration Tests - JuMP/Julia Solver
# =============================================================================
#
# This test suite validates trimming and folding transformations with the
# JuMP (Julia for Mathematical Programming) solver backend.
#
# Tests cover:
# • Trimmed model objective preservation (vs. original)
# • Folded model objective preservation (vs. original)
# • Fold+Trim model objective preservation (vs. original)
# • Multiple solver integration scenarios
#
# Workflow for each test:
# 1. Save and solve original UTOPIA model → baseline objective
# 2. Apply transformation (trim/fold/both)
# 3. Save and solve transformed model → transformed objective
# 4. Compare objectives (must match within tolerance)
# 5. Verify solve statistics (status, time, element counts)
#
# Success criteria:
# - Objectives match original (within 0.01 tolerance)
# - Solve status is optimal/success
# - Model structure correctly reflects transformations
# - JuMP code generation handles trimmed/folded elements
# - Arrow IPC format data transfer works correctly
#
# Dependencies:
# - Julia with JuMP.jl and HiGHS.jl packages
# - arrow package for IPC format
# - save_model, write_jump, solve_jump functions
# - trim_model, fold_model transformation functions
# =============================================================================

test_that("Trimmed UTOPIA matches original objective - JuMP", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Create temp directories
  temp_original <- tempfile("original_")
  temp_trimmed <- tempfile("trimmed_")
  dir.create(temp_original, recursive = TRUE)
  dir.create(temp_trimmed, recursive = TRUE)
  on.exit(unlink(c(temp_original, temp_trimmed), recursive = TRUE))
  
  # Save and solve original model
  save_model(demo_model, temp_original, format = "ipc", verbose = FALSE)
  write_jump(demo_model, model_dir = temp_original)
  result_original <- solve_jump(demo_model, temp_original, 
                                 method = "system", verbose = FALSE, load_results = FALSE)
  
  # Trim model
  utopia_trimmed <- trim_model(demo_model, verbose = FALSE)
  
  # Save and solve trimmed model
  save_model(utopia_trimmed, temp_trimmed, format = "ipc", verbose = FALSE)
  write_jump(utopia_trimmed, model_dir = temp_trimmed)
  result_trimmed <- solve_jump(utopia_trimmed, temp_trimmed,
                                method = "system", verbose = FALSE, load_results = FALSE)
  
  # Compare objectives
  expect_true(result_original$success, label = "Original model should solve successfully")
  expect_true(result_trimmed$success, label = "Trimmed model should solve successfully")
  expect_equal(result_original$objective, result_trimmed$objective, tolerance = 1e-4,
               label = "Trimmed model objective should match original")
  
  # Check that trimming actually removed elements
  trim_stats <- get_trim_summary(utopia_trimmed, format = "list")
  expect_gt(trim_stats$parameters$trimmed, 0, label = "Should have trimmed some parameters")
  expect_gt(trim_stats$variables$trimmed, 0, label = "Should have trimmed some variables")
})

test_that("Slice-Folded UTOPIA matches original objective - JuMP", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Create temp directories
  temp_original <- tempfile("original_")
  temp_folded <- tempfile("folded_")
  dir.create(temp_original, recursive = TRUE)
  dir.create(temp_folded, recursive = TRUE)
  on.exit(unlink(c(temp_original, temp_folded), recursive = TRUE))
  
  # Save and solve original model
  save_model(demo_model, temp_original, format = "ipc", verbose = FALSE)
  write_jump(demo_model, model_dir = temp_original)
  result_original <- solve_jump(demo_model, temp_original,
                                 method = "system", verbose = FALSE, load_results = FALSE)
  
  # Create fold specification for slice dimension only
  fold_spec <- create_fold_spec(
    demo_model,
    fold_dims = list(
      timeslice = list(
        tech = "mTechTimeslice",
        comm = "mCommTimeslice",
        sup = "mSupTimeslice"
      )
    ),
    verbose = FALSE
  )
  
  # Fold model
  utopia_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Save and solve folded model
  save_model(utopia_folded, temp_folded, format = "ipc", verbose = FALSE)
  write_jump(utopia_folded, model_dir = temp_folded, use_folded = TRUE)
  result_folded <- solve_jump(utopia_folded, temp_folded,
                               method = "system", verbose = FALSE, load_results = FALSE)
  
  # Compare objectives
  expect_true(result_original$success, label = "Original model should solve successfully")
  expect_true(result_folded$success, label = "Folded model should solve successfully")
  expect_equal(result_original$objective, result_folded$objective, tolerance = 1e-4,
               label = "Folded model objective should match original")
  
  # Check that folding actually reduced data
  fold_stats <- fold_spec[fold_spec$can_fold, ]
  expect_gt(nrow(fold_stats), 0, label = "Should have folded some parameters")
})

test_that("Fold+Trim UTOPIA matches original objective - JuMP", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Create temp directories
  temp_original <- tempfile("original_")
  temp_fold_trim <- tempfile("fold_trim_")
  dir.create(temp_original, recursive = TRUE)
  dir.create(temp_fold_trim, recursive = TRUE)
  on.exit(unlink(c(temp_original, temp_fold_trim), recursive = TRUE))
  
  # Save and solve original model
  save_model(demo_model, temp_original, format = "ipc", verbose = FALSE)
  write_jump(demo_model, model_dir = temp_original)
  result_original <- solve_jump(demo_model, temp_original,
                                 method = "system", verbose = FALSE, load_results = FALSE)
  
  # Create comprehensive 3D fold specification (slice + region + year)
  fold_spec <- create_fold_spec(
    demo_model,
    fold_dims = list(
      timeslice = list(
        tech = "mTechTimeslice",
        sup = "mSupTimeslice",
        comm = "mCommTimeslice",
        stg = c("mStorageComm", "mCommTimeslice")
      ),
      region = list(
        tech = "mTechRegion",
        sup = "mSupRegion",
        dem = "mDemRegion",
        stg = "mStorageRegion",
        trade = "mTradeRegion",
        imp = "mImportRegion"
      ),
      year = list(
        tech = "mTechYear",
        sup = "mSupYear",
        dem = "mDemYear",
        stg = "mStorageYear",
        trade = "mTradeYear",
        imp = "mImportYear"
      )
    ),
    verbose = FALSE
  )
  
  # Fold then trim
  utopia_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  utopia_fold_trim <- trim_model(utopia_folded, verbose = FALSE)
  
  # Save and solve fold+trim model
  save_model(utopia_fold_trim, temp_fold_trim, format = "ipc", verbose = FALSE)
  write_jump(utopia_fold_trim, model_dir = temp_fold_trim, use_folded = TRUE)
  result_fold_trim <- solve_jump(utopia_fold_trim, temp_fold_trim,
                                  method = "system", verbose = FALSE, load_results = FALSE)
  
  # Compare objectives
  expect_true(result_original$success, label = "Original model should solve successfully")
  expect_true(result_fold_trim$success, label = "Fold+Trim model should solve successfully")
  expect_equal(result_original$objective, result_fold_trim$objective, tolerance = 1e-4,
               label = "Fold+Trim model objective should match original")
  
  # Check that both operations occurred
  trim_stats <- get_trim_summary(utopia_fold_trim, format = "list")
  expect_gt(trim_stats$parameters$trimmed, 0, label = "Should have trimmed some parameters")
  expect_gt(trim_stats$variables$trimmed, 0, label = "Should have trimmed some variables")
})

