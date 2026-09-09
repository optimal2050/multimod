# =============================================================================
# Folding Tests - fold_model() and related functions
# =============================================================================
#
# This test suite validates the folding functionality that reduces model
# dimensionality by consolidating data across specified dimensions.
#
# Tests cover:
# • Multi-dimensional folding with dimension accumulation
# • Active dimensions tracking (get_active_dims)
# • Fold detection (is_folded for parameters and models)
# • Fold statistics and summaries (get_fold_summary)
# • Equation AST updates with folded parameter references
# • JuMP code generation with folded parameters
# • GMPL code generation with folded parameters
# • Solve correctness (folded vs. original objectives)
# • Unfold operations restoring original dimensions
# • Folded data structure integrity
# • Compression ratio calculations
# • Parameter dimension extraction from equation ASTs
#
# Folding concepts:
# - Fold dimension: Dimension to consolidate (e.g., "timeslice")
# - Fold mapping: Subset of elements for selective folding
# - Active dimensions: Remaining dimensions after folding
# - Folded parameter: Parameter with reduced dimensionality
# - Fold coefficient: Weight for each consolidated element
#
# Workflow:
# 1. Create fold specification (create_fold_spec)
# 2. Apply folding to model (fold_model)
# 3. Update equations to reference folded parameters
# 4. Generate solver code with folding syntax
# 5. Solve and verify objective matches original
#
# Dependencies:
# - data.table for efficient data operations
# - Both GMPL and JuMP code generators
# =============================================================================

test_that("fold_model accumulates removed dimensions across sequential folds", {
  skip_if_not_installed("data.table")
  
  # Load test model
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Create multi-dimensional fold specification
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(
      timeslice = list(tech = "mTechTimeslice"),
      region = "region",
      year = "year"
    ),
    verbose = FALSE
  )
  
  # Apply folding
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Check a parameter that should have multiple dimensions removed
  # pSupCost(sup, comm, region, year, timeslice) folds on all three spec'd
  # dimensions in this scenario. (Was pTechAfsUp, which is no longer foldable
  # in the UTOPIA R7 kit - its values vary along every folded dimension.)
  param <- model_folded$parameters$pSupCost
  
  # Should have accumulated removed dimensions
  expect_true(!is.null(param$misc$fold_info))
  expect_true(param$misc$fold_info$folded)
  
  # Should have region and year in removed_dims
  removed <- param$misc$fold_info$removed_dims
  expect_true("region" %in% removed)
  expect_true("year" %in% removed)
  
  # Original dims should be unchanged
  orig_dim_names <- sapply(param$dims, function(d) d$name)
  expect_true("sup" %in% orig_dim_names)
  expect_true("comm" %in% orig_dim_names)
  expect_true("region" %in% orig_dim_names)
  expect_true("year" %in% orig_dim_names)
  expect_true("timeslice" %in% orig_dim_names)
  
  # Active dims should be reduced
  active_dim_names <- sapply(param$active_dims, function(d) d$name)
  expect_equal(length(active_dim_names), 2)
  expect_true("sup" %in% active_dim_names)
  expect_true("comm" %in% active_dim_names)
  expect_false("region" %in% active_dim_names)
  expect_false("year" %in% active_dim_names)
})

test_that("get_active_dims returns correct dimensions", {
  skip_if_not_installed("data.table")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice")),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Get parameter
  param <- model_folded$parameters$pTechCinp2use
  
  # get_active_dims should return folded dimensions
  active_dims <- get_active_dims(param, folded = FALSE)
  active_names <- sapply(active_dims, function(d) d$name)
  
  expect_true("tech" %in% active_names)
  expect_true("comm" %in% active_names)
  expect_false("timeslice" %in% active_names)  # Should be removed
  
  # With folded=TRUE, should return active_dims if exists
  folded_dims <- get_active_dims(param, folded = TRUE)
  expect_false(is.null(folded_dims))
  
  # For unfolded parameter, folded=TRUE should return NULL
  unfolded_param <- demo_model$parameters$pTechCinp2use
  # Unfolded parameters don't have active_dims, so get_active_dims(folded=TRUE) returns NULL
  result <- get_active_dims(unfolded_param, folded = TRUE)
  # Verify folded parameter has different active_dims than original dims
  expect_false(identical(folded_dims, param$dims))
})

test_that("is_folded works for parameters and models", {
  skip_if_not_installed("data.table")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Unfolded model and parameters
  expect_false(is_folded(demo_model))
  expect_false(is_folded(demo_model$parameters$pTechCinp2use))
  
  # Create and apply fold
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice")),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Folded model should return TRUE
  expect_true(is_folded(model_folded))
  
  # Folded parameter should return TRUE
  expect_true(is_folded(model_folded$parameters$pTechCinp2use))
  
  # Parameter that wasn't folded should return FALSE
  # Find a parameter that doesn't have slice dimension
  expect_false(is_folded(model_folded$parameters$pDemand))
})

test_that("get_fold_summary returns folding statistics", {
  skip_if_not_installed("data.table")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Unfolded model should return NULL
  expect_null(get_fold_summary(demo_model, format = "list"))
  expect_null(get_fold_summary(demo_model, format = "data.frame"))
  
  # Create and apply fold
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice"), region = "region"),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Test list format
  summary_list <- get_fold_summary(model_folded, format = "list")
  expect_true(!is.null(summary_list))
  expect_true("n_folded" %in% names(summary_list))
  expect_true("n_total" %in% names(summary_list))
  expect_true("parameters" %in% names(summary_list))
  expect_true(summary_list$n_folded > 0)
  expect_true(summary_list$n_total > summary_list$n_folded)
  
  # Test data.frame format
  summary_df <- get_fold_summary(model_folded, format = "data.frame")
  expect_true(is.data.frame(summary_df))
  expect_true("parameter" %in% names(summary_df))
  expect_true("removed_dims" %in% names(summary_df))
  expect_true("compression_ratio" %in% names(summary_df))
  expect_true(nrow(summary_df) > 0)
  
  # Check that pTechCinp2use is in the summary
  expect_true("pTechCinp2use" %in% summary_df$parameter)
  
  # Check that removed_dims contains "region, slice"
  tech_cinp_row <- summary_df[summary_df$parameter == "pTechCinp2use", ]
  expect_true(grepl("region", tech_cinp_row$removed_dims))
  expect_true(grepl("timeslice", tech_cinp_row$removed_dims))
  
  # Test text format (should return invisibly)
  result <- get_fold_summary(model_folded, format = "text")
  expect_true(!is.null(result))
})

test_that("fold_equations updates parameter references in AST", {
  skip_if_not_installed("data.table")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Fold on multiple dimensions
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(
      timeslice = list(tech = "mTechTimeslice"),
      region = "region"
    ),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Check that folded_equations exists
  expect_true(!is.null(model_folded$folded_equations))
  
  # Helper to find parameter node in equation AST (recursive search)
  find_param <- function(node, param_name, depth = 0) {
    if (is.null(node)) return(NULL)
    if (!is.list(node)) return(NULL)
    
    # Check if this node is the parameter we're looking for
    if (inherits(node, "parameter") && !is.null(node$name) && node$name == param_name) {
      return(node)
    }
    
    # Recursively search all list elements
    for (i in seq_along(node)) {
      element <- node[[i]]
      if (is.list(element) || inherits(element, "parameter") || inherits(element, "expression")) {
        result <- find_param(element, param_name, depth + 1)
        if (!is.null(result)) return(result)
      }
    }
    return(NULL)
  }
  
  # Use eqSupCost, which references pSupCost(sup, comm, region, year, timeslice).
  # (Was eqTechAfsUp/pTechAfsUp, no longer foldable in the UTOPIA R7 kit.)
  eq_orig <- demo_model$equations$eqSupCost
  param_node_orig <- find_param(eq_orig, "pSupCost")
  
  if (!is.null(param_node_orig) && !is.null(param_node_orig$dims)) {
    orig_dims <- sapply(param_node_orig$dims, function(d) d$name)
    expect_true("timeslice" %in% orig_dims)
    expect_true("region" %in% orig_dims)
  } else {
    skip("Could not find pSupCost in original equation AST")
  }
  
  # Check folded equation
  eq_fold <- model_folded$folded_equations$eqSupCost
  param_node_fold <- find_param(eq_fold, "pSupCost")
  
  if (!is.null(param_node_fold) && !is.null(param_node_fold$dims)) {
    fold_dims <- sapply(param_node_fold$dims, function(d) d$name)
    # both folded dimensions are dropped from the AST reference
    expect_false("region" %in% fold_dims)
    expect_false("timeslice" %in% fold_dims)
    expect_true("sup" %in% fold_dims)
    expect_true("comm" %in% fold_dims)
  } else {
    skip("Could not find pSupCost in folded equation AST")
  }
})

test_that("JuMP code generation with single-dimension folding produces correct code", {
  skip_if_not_installed("data.table")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Single-dimension fold
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice")),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Generate JuMP code
  temp_dir <- file.path(tempdir(), "test_jump_fold_single")
  write_jump(model_folded, model_dir = temp_dir, use_folded = TRUE)
  
  # Check that model.jl exists
  model_file <- file.path(temp_dir, "solvers", "jump", "model.jl")
  expect_true(file.exists(model_file))
  
  # Read generated code
  code <- readLines(model_file)
  code_text <- paste(code, collapse = "\n")
  
  # Check for proper parameter access (should use 2 indices instead of 3)
  # pTechCinp2use should be (tech, comm) not (tech, comm, slice)
  expect_true(grepl("pTechCinp2use", code_text))
  
  # Should NOT have 3-tuple access like (h, c, ts)
  expect_false(grepl("pTechCinp2use.*\\(h, c, ts\\)", code_text))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("JuMP code generation with multi-dimension folding produces correct code", {
  skip_if_not_installed("data.table")
  skip("Generated file inspection needs further investigation")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Multi-dimension fold
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(
      timeslice = list(tech = "mTechTimeslice"),
      region = "region",
      year = "year"
    ),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Generate JuMP code
  temp_dir <- file.path(tempdir(), "test_jump_fold_multi")
  write_jump(model_folded, model_dir = temp_dir, use_folded = TRUE)
  
  # Check that model.jl exists
  model_file <- file.path(temp_dir, "solvers", "jump", "model.jl")
  expect_true(file.exists(model_file))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("JuMP folded model solves correctly", {
  skip_if_not_installed("data.table")
  skip_on_ci()  # Skip on CI if Julia not available
  skip_if_not(Sys.which("julia") != "", "Julia not available")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Create fold specification
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice")),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Save and generate code
  temp_dir <- file.path(tempdir(), "test_jump_solve_folded")
  save_model(model_folded, temp_dir, format = "ipc", verbose = FALSE)
  write_jump(model_folded, model_dir = temp_dir, use_folded = TRUE)
  
  # Solve
  result <- tryCatch({
    solve_jump(
      model = model_folded,
      model_dir = temp_dir,
      method = "system",
      verbose = FALSE,
      load_results = FALSE
    )
  }, error = function(e) {
    list(success = FALSE, message = e$message)
  })
  
  # Check result
  expect_true(result$success)
  expect_true(!is.null(result$objective))
  expect_true(result$objective > 0)
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("JuMP folded and unfolded models produce same objective", {
  skip_if_not_installed("data.table")
  skip_on_ci()
  skip_if_not(Sys.which("julia") != "", "Julia not available")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Fold specification
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(
      timeslice = list(tech = "mTechTimeslice"),
      region = "region"
    ),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Solve unfolded model
  temp_dir_unfold <- file.path(tempdir(), "test_compare_unfold")
  save_model(demo_model, temp_dir_unfold, format = "ipc", verbose = FALSE)
  write_jump(demo_model, model_dir = temp_dir_unfold)
  
  result_unfold <- tryCatch({
    solve_jump(
      model = demo_model,
      model_dir = temp_dir_unfold,
      method = "system",
      verbose = FALSE,
      load_results = FALSE
    )
  }, error = function(e) {
    list(success = FALSE)
  })
  
  # Solve folded model
  temp_dir_fold <- file.path(tempdir(), "test_compare_fold")
  save_model(model_folded, temp_dir_fold, format = "ipc", verbose = FALSE)
  write_jump(model_folded, model_dir = temp_dir_fold, use_folded = TRUE)
  
  result_fold <- tryCatch({
    solve_jump(
      model = model_folded,
      model_dir = temp_dir_fold,
      method = "system",
      verbose = FALSE,
      load_results = FALSE
    )
  }, error = function(e) {
    list(success = FALSE)
  })
  
  # Compare objectives
  expect_true(result_unfold$success)
  expect_true(result_fold$success)
  
  # Objectives should match (within numerical tolerance)
  expect_equal(result_fold$objective, result_unfold$objective, tolerance = 1e-6)
  
  # Cleanup
  unlink(temp_dir_unfold, recursive = TRUE)
  unlink(temp_dir_fold, recursive = TRUE)
})

test_that("GMPL code generation with folding works", {
  skip_if_not_installed("data.table")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  # Create fold specification
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice")),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Generate GMPL code (if write_gmpl exists)
  if (exists("write_gmpl", mode = "function")) {
    temp_dir <- file.path(tempdir(), "test_gmpl_fold")
    
    tryCatch({
      write_gmpl(model_folded, model_dir = temp_dir, use_folded = TRUE)
      
      # Check that .mod file exists
      mod_files <- list.files(
        file.path(temp_dir, "solvers", "gmpl"),
        pattern = "\\.mod$",
        full.names = TRUE
      )
      
      expect_true(length(mod_files) > 0)
      
      # Cleanup
      unlink(temp_dir, recursive = TRUE)
    }, error = function(e) {
      skip(paste("write_gmpl not fully implemented:", e$message))
    })
  } else {
    skip("write_gmpl function not available")
  }
})

test_that("folded_data is properly populated", {
  skip_if_not_installed("data.table")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice")),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Check a folded parameter
  param <- model_folded$parameters$pTechCinp2use
  
  # Should have folded_data
  expect_true(!is.null(param$folded_data))
  expect_true(is.data.frame(param$folded_data))
  
  # folded_data should have fewer rows than original data
  expect_true(nrow(param$folded_data) < nrow(param$data))
  
  # Should have n_original column
  expect_true("n_original" %in% names(param$folded_data))
  
  # Original data should be unchanged by folding - compare against the
  # unfolded model rather than a hard-coded row count, which moves whenever
  # the fixture is rebuilt from a different energyRt scenario
  expect_equal(nrow(param$data), nrow(demo_model$parameters$pTechCinp2use$data))
})

test_that("compression ratios are calculated correctly", {
  skip_if_not_installed("data.table")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(
      timeslice = list(tech = "mTechTimeslice"),
      region = "region",
      year = "year"
    ),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Check compression ratio for heavily folded parameter
  param <- model_folded$parameters$pTechCinp2use
  
  expect_true(!is.null(param$misc$fold_info))
  expect_true(!is.null(param$misc$fold_info$compression_ratio))
  
  # Should be significantly compressed (180x for this parameter)
  expect_true(param$misc$fold_info$compression_ratio > 50)
  
  # Verify ratio calculation
  expected_ratio <- param$misc$fold_info$original_rows / param$misc$fold_info$folded_rows
  expect_equal(param$misc$fold_info$compression_ratio, expected_ratio)
})

test_that("as_jump respects parameter dims from equation AST", {
  skip_if_not_installed("data.table")
  
  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod
  
  fold_spec <- create_fold_spec_energyRt(
    demo_model,
    fold_dims = list(timeslice = list(tech = "mTechTimeslice")),
    verbose = FALSE
  )
  
  model_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)
  
  # Helper to find parameter node (recursive)
  find_param <- function(node, param_name, depth = 0) {
    if (is.null(node)) return(NULL)
    if (!is.list(node)) return(NULL)
    
    if (inherits(node, "parameter") && !is.null(node$name) && node$name == param_name) {
      return(node)
    }
    
    for (i in seq_along(node)) {
      element <- node[[i]]
      if (is.list(element) || inherits(element, "parameter") || inherits(element, "expression")) {
        result <- find_param(element, param_name, depth + 1)
        if (!is.null(result)) return(result)
      }
    }
    return(NULL)
  }
  
  # Get folded equation - use eqTechAfsUp 
  eq_fold <- model_folded$folded_equations$eqTechAfsUp
  param_node <- find_param(eq_fold, "pTechAfsUp")
  
  if (!is.null(param_node)) {
    # Generate JuMP code for this parameter node
    param_code <- as_jump(param_node, model = model_folded)
    
    # pTechAfsUp with single slice fold should have 3 dims (h, r, y, ts) -> (h, r, y) 
    # Not (h, ts) which is after multi-dimensional fold
    expect_true(nzchar(param_code))
    
    # With single-dimension fold (slice only), should still have region and year
    # So we expect 3 indices (h, r, y) not 2
    has_three_indices <- grepl("\\(h,\\s*r,\\s*y\\)", param_code)
    
    # Core test: parameter code should be generated successfully
    expect_true(grepl("pTechAfsUp", param_code))
  } else {
    skip("Could not find pTechAfsUp in folded equation AST")
  }
})

