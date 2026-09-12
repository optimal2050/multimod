# =============================================================================
# Core Trimming Tests - trim_model() functionality
# =============================================================================
#
# This test suite validates the core trimming functionality of multimod models.
# Trimming removes unused/empty model elements to reduce problem size.
#
# Tests cover:
# • Phase 1: Marking empty sets, parameters, and mappings
# • Phase 2: Marking variables with empty domains
# • Phase 3: Marking equations with empty domains
# • Phase 4: Untrimming elements required by non-trimmed equations
# • Trim summary and statistics (get_trim_summary, is_trimmed)
# • Untrimming operations (untrim_model)
# • Integration with code generators (write_jump, write_gmpl)
# • Integration with model storage (save_model)
# • Full workflow tests with UTOPIA model (trim, fold, solve)
# • Objective value preservation across transformations
#
# Key concepts:
# - Empty domain: Set/mapping with no data (length 0)
# - Trimmed element: Marked with $trimmed = TRUE flag
# - Required element: Used in non-trimmed equations (untrimmed in Phase 4)
# - NET trim count: Elements still trimmed after Phase 4 untrimming
#
# Note: Phase 5 (unused variable removal) was removed - variables only
# trimmed if they have empty domains, not if merely unused in equations.
# =============================================================================

test_that("trim_model marks empty sets", {
  # Create model with empty and non-empty sets
  model <- new_model_structure(
    sets = list(
      region = new_set("region", data = c("R1", "R2")),
      empty_set = new_set("empty_set", data = character(0)),
      year = new_set("year", data = c("2020", "2021"))
    )
  )

  # Trim the model
  result <- trim_model(model, verbose = FALSE)

  # Check that empty set is marked as trimmed
  expect_true(isTRUE(result$sets$empty_set$trimmed))

  # Check that non-empty sets are not trimmed
  expect_false(isTRUE(result$sets$region$trimmed))
  expect_false(isTRUE(result$sets$year$trimmed))
})

test_that("trim_model marks empty parameters", {
  # Create model with empty and non-empty parameters
  model <- new_model_structure(
    sets = list(
      region = new_set("region", data = c("R1", "R2"))
    ),
    parameters = list(
      pDemand = new_parameter("pDemand", dims = "region",
                              data = data.frame(region = c("R1", "R2"), value = c(100, 200))),
      pEmpty = new_parameter("pEmpty", dims = "region",
                            data = data.frame(region = character(0), value = numeric(0)))
    )
  )

  # Trim the model
  result <- trim_model(model, verbose = FALSE)

  # Check that empty parameter is marked as trimmed
  expect_true(isTRUE(result$parameters$pEmpty$trimmed))

  # Check that non-empty parameter is not trimmed
  expect_false(isTRUE(result$parameters$pDemand$trimmed))
})

test_that("trim_model marks empty mappings", {
  # Create model with empty and non-empty mappings
  model <- new_model_structure(
    sets = list(
      tech = new_set("tech", data = c("T1", "T2")),
      region = new_set("region", data = c("R1", "R2"))
    ),
    mappings = list(
      mTechRegion = new_mapping("mTechRegion", dims = c("tech", "region"),
                                data = data.frame(tech = c("T1", "T2"), region = c("R1", "R2"))),
      mEmptyMapping = new_mapping("mEmptyMapping", dims = c("tech", "region"),
                                  data = data.frame(tech = character(0), region = character(0)))
    )
  )

  # Trim the model
  result <- trim_model(model, verbose = FALSE)

  # Check that empty mapping is marked as trimmed
  expect_true(isTRUE(result$mappings$mEmptyMapping$trimmed))

  # Check that non-empty mapping is not trimmed
  expect_false(isTRUE(result$mappings$mTechRegion$trimmed))
})

test_that("trim_model marks variables with empty domains", {
  # Create model with variable depending on empty set
  model <- new_model_structure(
    sets = list(
      region = new_set("region", data = c("R1", "R2")),
      empty_tech = new_set("empty_tech", data = character(0))
    ),
    variables = list(
      vCapacity = new_variable("vCapacity", dims = c("region"), vtype = "continuous"),
      vEmptyVar = new_variable("vEmptyVar", dims = c("empty_tech", "region"), vtype = "continuous")
    )
  )

  # Trim the model
  result <- trim_model(model, verbose = FALSE)

  # Check that variable with empty dimension is marked as trimmed
  expect_true(isTRUE(result$variables$vEmptyVar$trimmed))

  # Check that variable with only non-empty dimensions is not trimmed
  # Note: Phase 5 (unused variable removal) was removed, so vCapacity is NOT trimmed
  # Variables are only trimmed if they have empty domains
  expect_false(isTRUE(result$variables$vCapacity$trimmed))
})

test_that("trim_model marks equations with empty domains", {
  skip("This test expects Phase 3 equation trimming which needs domain field fixes")
  # Create simple model with equations
  model <- new_model_structure(
    sets = list(
      region = new_set("region", data = c("R1", "R2")),
      empty_set = new_set("empty_set", data = character(0))
    ),
    variables = list(
      vDemand = new_variable("vDemand", dims = "region", vtype = "continuous"),
      vEmpty = new_variable("vEmpty", dims = "empty_set", vtype = "continuous")
    ),
    equations = list(
      eqDemand = new_equation(
        name = "eqDemand",
        dims = "region",
        lhs = ast_variable("vDemand", dims = "region"),
        rhs = ast_constant(100),
        relation = ">="
      ),
      eqEmpty = new_equation(
        name = "eqEmpty",
        dims = "empty_set",
        lhs = ast_variable("vEmpty", dims = "empty_set"),
        rhs = ast_constant(0),
        relation = "=="
      )
    )
  )

  # Trim the model
  result <- trim_model(model, verbose = FALSE)

  # Check that equation with empty dimension is marked as trimmed
  expect_true(isTRUE(result$equations$eqEmpty$trimmed))

  # Check that equation with non-empty dimension is not trimmed
  expect_false(isTRUE(result$equations$eqDemand$trimmed))
})

test_that("trim_model untrim required elements", {
  # Create model where parameter is empty but used in non-trimmed equation
  model <- new_model_structure(
    sets = list(
      region = new_set("region", data = c("R1", "R2"))
    ),
    parameters = list(
      pDemand = new_parameter("pDemand", dims = "region",
                             data = data.frame(region = character(0), value = numeric(0)))
    ),
    variables = list(
      vSupply = new_variable("vSupply", dims = "region", vtype = "continuous")
    ),
    equations = list(
      eqBalance = new_equation(
        name = "eqBalance",
        dims = "region",
        lhs = ast_variable("vSupply", dims = "region"),
        rhs = ast_parameter("pDemand", dims = "region"),
        relation = ">="
      )
    )
  )

  # Trim the model
  result <- trim_model(model, verbose = FALSE)

  # pDemand is initially marked as trimmed (empty)
  # But then untrimmed because it's used in a non-trimmed equation
  expect_false(isTRUE(result$parameters$pDemand$trimmed))

  # Equation should not be trimmed
  expect_false(isTRUE(result$equations$eqBalance$trimmed))
})

test_that("trim_model marks unused variables", {
  skip("Phase 5 (unused variable removal) was removed - variables trimmed by domain only")
  # Create model with variable not appearing in any equation
  model <- new_model_structure(
    sets = list(
      region = new_set("region", data = c("R1", "R2"))
    ),
    variables = list(
      vUsed = new_variable("vUsed", dims = "region", vtype = "continuous"),
      vUnused = new_variable("vUnused", dims = "region", vtype = "continuous")
    ),
    equations = list(
      eqBalance = new_equation(
        name = "eqBalance",
        dims = "region",
        lhs = ast_variable("vUsed", dims = "region"),
        rhs = ast_constant(100),
        relation = "=="
      )
    )
  )

  # Trim the model
  result <- trim_model(model, verbose = FALSE)

  # Check that unused variable is marked as trimmed
  expect_true(isTRUE(result$variables$vUnused$trimmed))

  # Check that used variable is not trimmed
  expect_false(isTRUE(result$variables$vUsed$trimmed))
})

test_that("is_trimmed works correctly", {
  # Create and trim a model
  model <- new_model_structure(
    sets = list(
      empty_set = new_set("empty_set", data = character(0))
    )
  )

  # Before trimming
  expect_false(is_trimmed(model))

  # After trimming
  result <- trim_model(model, verbose = FALSE)
  expect_true(is_trimmed(result))
  expect_true(is_trimmed(result$sets$empty_set))
})

test_that("get_trim_summary returns correct statistics", {
  # Create model with mix of empty and non-empty elements
  model <- new_model_structure(
    sets = list(
      region = new_set("region", data = c("R1", "R2")),
      empty_set = new_set("empty_set", data = character(0))
    ),
    parameters = list(
      pDemand = new_parameter("pDemand", dims = "region",
                             data = data.frame(region = c("R1", "R2"), value = c(100, 200))),
      pEmpty = new_parameter("pEmpty", dims = "region",
                            data = data.frame(region = character(0), value = numeric(0)))
    )
  )

  # Trim the model
  result <- trim_model(model, verbose = FALSE)

  # Get summary
  summary_list <- get_trim_summary(result, format = "list")
  expect_equal(summary_list$sets$total, 2)
  expect_equal(summary_list$sets$trimmed, 1)
  expect_equal(summary_list$parameters$total, 2)
  expect_equal(summary_list$parameters$trimmed, 1)

  # Get data.frame summary
  summary_df <- get_trim_summary(result, format = "data.frame")
  expect_s3_class(summary_df, "data.frame")
  expect_true("element_type" %in% names(summary_df))
  expect_true("trimmed" %in% names(summary_df))

  # Get text summary
  summary_text <- get_trim_summary(result, format = "text")
  expect_type(summary_text, "character")
  expect_true(grepl("sets", summary_text))
})

test_that("untrim_model removes trimmed flags", {
  # Create and trim a model
  model <- new_model_structure(
    sets = list(
      empty_set = new_set("empty_set", data = character(0)),
      region = new_set("region", data = c("R1", "R2"))
    )
  )

  # Trim
  trimmed <- trim_model(model, verbose = FALSE)
  expect_true(is_trimmed(trimmed))
  expect_true(isTRUE(trimmed$sets$empty_set$trimmed))

  # Untrim
  untrimmed <- untrim_model(trimmed)
  expect_false(is_trimmed(untrimmed))
  expect_false(isTRUE(untrimmed$sets$empty_set$trimmed))
  expect_false(isTRUE(untrimmed$sets$region$trimmed))
})

test_that("untrim_model can selectively untrim elements", {
  # Create and trim a model
  model <- new_model_structure(
    sets = list(
      empty_set = new_set("empty_set", data = character(0))
    ),
    parameters = list(
      pEmpty = new_parameter("pEmpty", dims = "empty_set",
                            data = data.frame(empty_set = character(0), value = numeric(0)))
    )
  )

  # Trim
  trimmed <- trim_model(model, verbose = FALSE)
  expect_true(isTRUE(trimmed$sets$empty_set$trimmed))
  expect_true(isTRUE(trimmed$parameters$pEmpty$trimmed))

  # Untrim only parameters
  partial_untrim <- untrim_model(trimmed, elements = "parameters")
  expect_true(isTRUE(partial_untrim$sets$empty_set$trimmed))  # Still trimmed
  expect_false(isTRUE(partial_untrim$parameters$pEmpty$trimmed))  # Untrimmed
})

test_that("write_jump skips trimmed elements", {
  skip("This test needs updating for new trimming behavior")
  skip_if_not_installed("multimod")

  # Create model with trimmed elements
  model <- new_model_structure(
    sets = list(
      region = new_set("region", data = c("R1", "R2")),
      empty_tech = new_set("empty_tech", data = character(0))
    ),
    parameters = list(
      pDemand = new_parameter("pDemand", dims = "region",
                             data = data.frame(region = c("R1", "R2"), value = c(100, 200)))
    ),
    variables = list(
      vSupply = new_variable("vSupply", dims = "region", vtype = "continuous"),
      vEmpty = new_variable("vEmpty", dims = "empty_tech", vtype = "continuous")
    )
  )

  # Trim
  trimmed <- trim_model(model, verbose = FALSE)

  # Generate JuMP code
  jump_code <- write_jump(trimmed)
  jump_text <- paste(jump_code, collapse = "\n")

  # Check that trimmed variable is commented out
  expect_true(grepl("# vEmpty trimmed", jump_text) || !grepl("vEmpty", jump_text))

  # Check that non-trimmed variable is present
  expect_true(grepl("vSupply", jump_text))
})

test_that("write_gmpl skips trimmed elements", {
  skip_if_not_installed("multimod")

  # Create model with trimmed elements
  model <- new_model_structure(
    sets = list(
      region = new_set("region", data = c("R1", "R2")),
      empty_tech = new_set("empty_tech", data = character(0))
    ),
    variables = list(
      vSupply = new_variable("vSupply", dims = "region", vtype = "continuous"),
      vEmpty = new_variable("vEmpty", dims = "empty_tech", vtype = "continuous")
    )
  )
  # KNOWN BUG: as_multimod() drops $data from sets, so `region` arrives with
  # zero members and trim_model() then correctly trims it as empty. That is
  # why the 'set region is declared' assertion below fails. The failure is
  # real and in as_multimod(), not in this test -- it was hidden until now
  # behind a validation error that fired first.
  model <- as_multimod(model)

  # Trim
  trimmed <- trim_model(model, verbose = FALSE)

  # check = FALSE: this fixture is a structural stub with no equations and no
  # objective, which validate() rejects. Validation is not what is under test
  # here -- whether trimmed elements are omitted from the emitted GMPL is.
  gmpl_code <- write_gmpl(trimmed, include_solve = FALSE, check = FALSE)
  gmpl_text <- paste(gmpl_code, collapse = "\n")

  # Check that trimmed set is not declared
  expect_false(grepl("set empty_tech", gmpl_text))

  # Check that non-trimmed set is declared
  expect_true(grepl("set region", gmpl_text))
})

test_that("save_model skips trimmed elements in metadata CSVs", {
  skip_if_not_installed("multimod")

  # Create model with trimmed elements
  model <- new_model_structure(
    sets = list(
      region = new_set("region", data = c("R1", "R2")),
      empty_set = new_set("empty_set", data = character(0))
    ),
    parameters = list(
      pDemand = new_parameter("pDemand", dims = "region",
                             data = data.frame(region = c("R1", "R2"), value = c(100, 200))),
      pEmpty = new_parameter("pEmpty", dims = "region",
                            data = data.frame(region = character(0), value = numeric(0)))
    )
  )

  # Add 'model' class for save_model compatibility
  class(model) <- c("model", class(model))

  # Trim
  trimmed <- trim_model(model, verbose = FALSE)

  # Save to temp directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  save_model(trimmed, path = temp_dir, format = "csv")

  # Check sets.csv
  sets_csv <- file.path(temp_dir, "sets", "sets.csv")
  if (file.exists(sets_csv)) {
    sets_df <- read.csv(sets_csv, stringsAsFactors = FALSE)
    expect_true("region" %in% sets_df$name)
    expect_false("empty_set" %in% sets_df$name)
  }

  # Check parameters.csv
  params_csv <- file.path(temp_dir, "parameters", "parameters.csv")
  if (file.exists(params_csv)) {
    params_df <- read.csv(params_csv, stringsAsFactors = FALSE)
    expect_true("pDemand" %in% params_df$name)
    expect_false("pEmpty" %in% params_df$name)
  }
})

# ==============================================================================
# Objective Comparison Tests with UTOPIA
# ==============================================================================

test_that("Trimming UTOPIA preserves solution - JuMP", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  skip_if_not(system2("julia", "--version", stdout = FALSE, stderr = FALSE) == 0,
              "Julia not available")

  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Test directories
  temp_base <- tempfile("trim_utopia_jump_")
  dir_original <- file.path(temp_base, "original")
  dir_trimmed <- file.path(temp_base, "trimmed")

  # Solve original model
  save_model(demo_model, dir_original, format = "ipc", verbose = FALSE)
  write_jump(demo_model, model_dir = dir_original)
  result_orig <- solve_jump(demo_model, dir_original, method = "system",
                            verbose = FALSE, load_results = FALSE)

  # Trim and solve
  utopia_trimmed <- trim_model(demo_model, verbose = FALSE)
  save_model(utopia_trimmed, dir_trimmed, format = "ipc", verbose = FALSE)
  write_jump(utopia_trimmed, model_dir = dir_trimmed)
  result_trim <- solve_jump(utopia_trimmed, dir_trimmed, method = "system",
                           verbose = FALSE, load_results = FALSE)

  # Compare results
  expect_true(result_orig$success)
  expect_true(result_trim$success)
  expect_equal(result_orig$status, "OPTIMAL")
  expect_equal(result_trim$status, "OPTIMAL")

  # Objectives should match exactly
  expect_equal(result_trim$objective, result_orig$objective, tolerance = 1e-6)

  # Check trimming statistics
  summary <- get_trim_summary(utopia_trimmed, format = "list")
  expect_gt(summary$sets$trimmed, 0)
  expect_gt(summary$parameters$trimmed, 0)
  expect_gt(summary$variables$trimmed, 0)
  expect_gt(summary$equations$trimmed, 0)

  # Cleanup
  unlink(temp_base, recursive = TRUE)
})

test_that("Trimming UTOPIA preserves solution - GMPL", {
  skip("GMPL solver integration not yet implemented - use JuMP test instead")
})

test_that("Fold+Trim UTOPIA preserves solution - JuMP", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  skip_if_not(system2("julia", "--version", stdout = FALSE, stderr = FALSE) == 0,
              "Julia not available")

  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Test directories
  temp_base <- tempfile("fold_trim_utopia_jump_")
  dir_original <- file.path(temp_base, "original")
  dir_folded <- file.path(temp_base, "folded")
  dir_fold_trim <- file.path(temp_base, "fold_trim")

  # Solve original model
  save_model(demo_model, dir_original, format = "ipc", verbose = FALSE)
  write_jump(demo_model, model_dir = dir_original)
  result_orig <- solve_jump(demo_model, dir_original, method = "system",
                            verbose = FALSE, load_results = FALSE)

  # Fold model (3x: tech-slice, comm-slice, sup-slice)
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
  utopia_folded <- fold_model(demo_model, fold_spec = fold_spec, verbose = FALSE)

  # Solve folded model
  save_model(utopia_folded, dir_folded, format = "ipc", verbose = FALSE)
  write_jump(utopia_folded, model_dir = dir_folded, use_folded = TRUE)
  result_fold <- solve_jump(utopia_folded, dir_folded, method = "system",
                            verbose = FALSE, load_results = FALSE)

  # Fold+Trim model
  utopia_fold_trim <- trim_model(utopia_folded, verbose = FALSE)
  save_model(utopia_fold_trim, dir_fold_trim, format = "ipc", verbose = FALSE)
  write_jump(utopia_fold_trim, model_dir = dir_fold_trim, use_folded = TRUE)
  result_fold_trim <- solve_jump(utopia_fold_trim, dir_fold_trim, method = "system",
                                 verbose = FALSE, load_results = FALSE)

  # Compare results
  expect_true(result_orig$success)
  expect_true(result_fold$success)
  expect_true(result_fold_trim$success)

  expect_equal(result_orig$status, "OPTIMAL")
  expect_equal(result_fold$status, "OPTIMAL")
  expect_equal(result_fold_trim$status, "OPTIMAL")

  # All three should have same objective
  expect_equal(result_fold$objective, result_orig$objective, tolerance = 1e-6)
  expect_equal(result_fold_trim$objective, result_orig$objective, tolerance = 1e-6)

  # Check that folding and trimming both occurred
  fold_summary <- get_fold_summary(utopia_folded, format = "list")
  expect_gt(fold_summary$n_folded, 0)

  trim_summary <- get_trim_summary(utopia_fold_trim, format = "list")
  expect_gt(trim_summary$parameters$trimmed, 0)
  expect_gt(trim_summary$variables$trimmed, 0)

  # Cleanup
  unlink(temp_base, recursive = TRUE)
})

test_that("Fold+Trim UTOPIA preserves solution - GMPL", {
  skip("GMPL solver integration not yet implemented - use JuMP test instead")
})

# =============================================================================
# Integration tests with solvers have been moved to separate files:
# - test-trim-jump.R: JuMP/Julia solver integration tests
# - test-trim-gmpl.R: GMPL/glpsol solver integration tests
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

# =============================================================================
# GMPL Solver Tests
# =============================================================================

test_that("Trimmed UTOPIA matches original objective - GMPL", {
  skip_on_cran()

  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Create temp directories
  temp_original <- tempfile("original_gmpl_")
  temp_trimmed <- tempfile("trimmed_gmpl_")
  dir.create(temp_original, recursive = TRUE)
  dir.create(temp_trimmed, recursive = TRUE)
  on.exit(unlink(c(temp_original, temp_trimmed), recursive = TRUE))

  # Save and solve original model
  save_model(demo_model, temp_original, format = "csv", verbose = FALSE)
  write_gmpl(demo_model, model_dir = temp_original)
  result_original <- solve_gmpl(demo_model, temp_original,
                                 verbose = FALSE, load_results = FALSE)

  # Trim model
  utopia_trimmed <- trim_model(demo_model, verbose = FALSE)

  # Save and solve trimmed model
  save_model(utopia_trimmed, temp_trimmed, format = "csv", verbose = FALSE)
  write_gmpl(utopia_trimmed, model_dir = temp_trimmed)
  result_trimmed <- solve_gmpl(utopia_trimmed, temp_trimmed,
                                verbose = FALSE, load_results = FALSE)

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

test_that("Slice-Folded UTOPIA matches original objective - GMPL", {
  skip_on_cran()
  skip("GMPL folding not yet implemented")

  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Create temp directories
  temp_original <- tempfile("original_gmpl_")
  temp_folded <- tempfile("folded_gmpl_")
  dir.create(temp_original, recursive = TRUE)
  dir.create(temp_folded, recursive = TRUE)
  on.exit(unlink(c(temp_original, temp_folded), recursive = TRUE))

  # Save and solve original model
  save_model(demo_model, temp_original, format = "csv", verbose = FALSE)
  write_gmpl(demo_model, model_dir = temp_original)
  result_original <- solve_gmpl(demo_model, temp_original,
                                 verbose = FALSE, load_results = FALSE)

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
  save_model(utopia_folded, temp_folded, format = "csv", verbose = FALSE)
  write_gmpl(utopia_folded, model_dir = temp_folded, use_folded = TRUE)
  result_folded <- solve_gmpl(utopia_folded, temp_folded,
                               verbose = FALSE, load_results = FALSE)

  # Compare objectives
  expect_true(result_original$success, label = "Original model should solve successfully")
  expect_true(result_folded$success, label = "Folded model should solve successfully")
  expect_equal(result_original$objective, result_folded$objective, tolerance = 1e-4,
               label = "Folded model objective should match original")

  # Check that folding actually reduced data
  fold_stats <- fold_spec[fold_spec$can_fold, ]
  expect_gt(nrow(fold_stats), 0, label = "Should have folded some parameters")
})

test_that("Fold+Trim UTOPIA matches original objective - GMPL", {
  skip_on_cran()
  skip("GMPL folding not yet implemented")

  data(example_models, package = "multimod")
  demo_model <- example_models$energyRt$multimod

  # Create temp directories
  temp_original <- tempfile("original_gmpl_")
  temp_fold_trim <- tempfile("fold_trim_gmpl_")
  dir.create(temp_original, recursive = TRUE)
  dir.create(temp_fold_trim, recursive = TRUE)
  on.exit(unlink(c(temp_original, temp_fold_trim), recursive = TRUE))

  # Save and solve original model
  save_model(demo_model, temp_original, format = "csv", verbose = FALSE)
  write_gmpl(demo_model, model_dir = temp_original)
  result_original <- solve_gmpl(demo_model, temp_original,
                                 verbose = FALSE, load_results = FALSE)

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
  save_model(utopia_fold_trim, temp_fold_trim, format = "csv", verbose = FALSE)
  write_gmpl(utopia_fold_trim, model_dir = temp_fold_trim, use_folded = TRUE)
  result_fold_trim <- solve_gmpl(utopia_fold_trim, temp_fold_trim,
                                  verbose = FALSE, load_results = FALSE)

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



