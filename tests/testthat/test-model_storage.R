# =============================================================================
# Model Storage Tests - save_model() and load_model() functions
# =============================================================================
#
# This test suite validates model persistence and lazy loading functionality
# for multimod models.
#
# Tests cover:
# • Directory structure creation
# • Model saving with CSV and Arrow IPC formats
# • Model loading with structure restoration
# • Format conversion (CSV ↔ Arrow IPC)
# • Lazy loading (on-demand data loading)
# • Parameter updates with change tracking
# • Auto-save on parameter modifications
# • Memory management (keep_in_memory option)
# • model.rds structure completeness
# • Metadata file organization
#
# Storage architecture:
# - model.rds: Complete model structure (sets, params, equations, etc.)
# - metadata/: CSV files with element metadata (names, dims, descriptions)
# - data/: Parameter and mapping data files (CSV or Arrow IPC)
# - solvers/: Generated solver code (GMPL, JuMP)
#
# Lazy loading workflow:
# 1. Save model with save_model()
# 2. Load structure with load_model(lazy = TRUE)
# 3. Data loaded on-demand when accessed
# 4. Modified data can be saved back
#
# Format options:
# - "csv": Human-readable, universal compatibility
# - "ipc": Arrow IPC format, faster I/O, preserves types
#
# Dependencies:
# - arrow package for IPC format support
# =============================================================================

# Helper function to create test model with proper class
create_test_model <- function(name = "test", sets = NULL, parameters = NULL, mappings = NULL,
                              equations = NULL, variables = NULL) {
  model <- list(
    name = name,
    sets = sets %||% list(),
    parameters = parameters %||% list(),
    mappings = mappings %||% list(),
    equations = equations %||% list(),
    variables = variables %||% list()
  )
  class(model) <- c("multimod", "model", "list")
  model
}

test_that("save_model creates directory structure", {
  skip_if_not_installed("arrow")
  
  # Create a simple test model
  model <- create_test_model(
    name = "test_model",
    sets = list(
      region = list(data = c("R1", "R2")),
      year = list(data = c("2020", "2025", "2030"))
    ),
    parameters = list(
      pDemand = list(
        name = "pDemand",
        sets = c("region", "year"),
        data = data.frame(
          region = c("R1", "R1", "R1", "R2", "R2", "R2"),
          year = c("2020", "2025", "2030", "2020", "2025", "2030"),
          value = c(100, 110, 120, 200, 220, 240)
        )
      )
    ),
    mappings = list(
      mRegionYear = list(
        name = "mRegionYear",
        sets = c("region", "year"),
        data = data.frame(
          region = c("R1", "R1", "R1", "R2", "R2", "R2"),
          year = c("2020", "2025", "2030", "2020", "2025", "2030")
        )
      )
    )
  )
  
  # Save model
  temp_dir <- file.path(tempdir(), "test_model_csv")
  model_saved <- save_model(
    model,
    path = temp_dir,
    format = "csv",
    verbose = FALSE
  )
  
  # Check directory structure
  expect_true(dir.exists(temp_dir))
  expect_true(file.exists(file.path(temp_dir, "model.rds")))
  expect_true(file.exists(file.path(temp_dir, "metadata.json")))
  expect_true(file.exists(file.path(temp_dir, "format.txt")))
  expect_true(dir.exists(file.path(temp_dir, "sets")))
  expect_true(dir.exists(file.path(temp_dir, "parameters")))
  expect_true(dir.exists(file.path(temp_dir, "mappings")))
  
  # Check set files (stored in subdirectories)
  expect_true(file.exists(file.path(temp_dir, "sets", "region", "data.csv")))
  expect_true(file.exists(file.path(temp_dir, "sets", "year", "data.csv")))
  
  # Check parameter files
  expect_true(file.exists(file.path(temp_dir, "parameters", "pDemand", "data.csv")))
  expect_false(file.exists(file.path(temp_dir, "parameters", "pDemand", "metadata.json")))
  
  # Check mapping files
  expect_true(file.exists(file.path(temp_dir, "mappings", "mRegionYear", "data.csv")))
  expect_false(file.exists(file.path(temp_dir, "mappings", "mRegionYear", "metadata.json")))
  
  # Check format file content
  format_content <- readLines(file.path(temp_dir, "format.txt"))
  expect_equal(format_content, "csv")
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("load_model restores model correctly", {
  skip_if_not_installed("arrow")
  
  # Create and save test model
  model <- create_test_model(
    name = "test_load",
    sets = list(
      region = list(members = c("R1", "R2"))
    ),
    parameters = list(
      pCost = list(
        name = "pCost",
        sets = c("region"),
        data = data.frame(
          region = c("R1", "R2"),
          value = c(10, 20)
        )
      )
    )
  )
  
  temp_dir <- file.path(tempdir(), "test_load_model")
  save_model(model, temp_dir, format = "csv", verbose = FALSE)
  
  # Load model
  loaded_model <- load_model(temp_dir, verbose = FALSE)
  
  # Check structure
  expect_equal(loaded_model$name, "test_load")
  expect_true("region" %in% names(loaded_model$sets))
  expect_true("pCost" %in% names(loaded_model$parameters))
  
  # Check data
  expect_equal(nrow(loaded_model$parameters$pCost$data), 2)
  expect_equal(loaded_model$parameters$pCost$data$value, c(10, 20))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("save_model works with different formats", {
  skip_if_not_installed("arrow")
  
  model <- create_test_model(
    name = "test_formats",
    sets = list(tech = list(members = c("T1", "T2"))),
    parameters = list(
      pValue = list(
        name = "pValue",
        sets = c("tech"),
        data = data.frame(tech = c("T1", "T2"), value = c(1, 2))
      )
    )
  )
  
  # Test CSV
  csv_dir <- file.path(tempdir(), "test_format_csv")
  save_model(model, csv_dir, format = "csv", verbose = FALSE)
  expect_true(file.exists(file.path(csv_dir, "parameters", "pValue", "data.csv")))
  
  # Test IPC
  ipc_dir <- file.path(tempdir(), "test_format_ipc")
  save_model(model, ipc_dir, format = "ipc", verbose = FALSE)
  expect_true(file.exists(file.path(ipc_dir, "parameters", "pValue", "data.arrow")))
  
  # Test Parquet
  parquet_dir <- file.path(tempdir(), "test_format_parquet")
  save_model(model, parquet_dir, format = "parquet", verbose = FALSE)
  expect_true(file.exists(file.path(parquet_dir, "parameters", "pValue", "data.parquet")))
  
  # Load and verify all formats work
  csv_loaded <- load_model(csv_dir, verbose = FALSE)
  ipc_loaded <- load_model(ipc_dir, verbose = FALSE)
  parquet_loaded <- load_model(parquet_dir, verbose = FALSE)
  
  expect_equal(csv_loaded$parameters$pValue$data$value, c(1, 2))
  expect_equal(ipc_loaded$parameters$pValue$data$value, c(1, 2))
  expect_equal(parquet_loaded$parameters$pValue$data$value, c(1, 2))
  
  # Cleanup
  unlink(csv_dir, recursive = TRUE)
  unlink(ipc_dir, recursive = TRUE)
  unlink(parquet_dir, recursive = TRUE)
})

test_that("lazy loading works correctly", {
  skip_if_not_installed("arrow")
  
  model <- create_test_model(
    name = "test_lazy",
    sets = list(node = list(members = c("N1", "N2", "N3"))),
    parameters = list(
      pParam1 = list(
        name = "pParam1",
        sets = c("node"),
        data = data.frame(node = c("N1", "N2", "N3"), value = c(10, 20, 30))
      ),
      pParam2 = list(
        name = "pParam2",
        sets = c("node"),
        data = data.frame(node = c("N1", "N2", "N3"), value = c(100, 200, 300))
      )
    )
  )
  
  temp_dir <- file.path(tempdir(), "test_lazy_load")
  save_model(model, temp_dir, format = "ipc", verbose = FALSE)
  
  # Load without data
  lazy_model <- load_model(temp_dir, load_data = FALSE, verbose = FALSE)
  
  # Structure should be present
  expect_true("pParam1" %in% names(lazy_model$parameters))
  expect_true("pParam2" %in% names(lazy_model$parameters))
  
  # With lazy loading, data may be loaded from model.rds structure but is thinned
  # The key test is that get_data() can reload it from disk
  
  # Use get_data to ensure data is available
  param1_data <- get_data(lazy_model, "pParam1", type = "parameter")
  
  # Data should be loaded
  expect_equal(nrow(param1_data), 3)
  expect_equal(param1_data$value, c(10, 20, 30))
  
  # get_data should work for param2 too
  param2_data <- get_data(lazy_model, "pParam2", type = "parameter")
  expect_equal(nrow(param2_data), 3)
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("update_parameter modifies and tracks changes", {
  skip_if_not_installed("arrow")
  
  model <- create_test_model(
    name = "test_update",
    sets = list(tech = list(members = c("T1", "T2"))),
    parameters = list(
      pCost = list(
        name = "pCost",
        sets = c("tech"),
        data = data.frame(tech = c("T1", "T2"), value = c(100, 200))
      )
    )
  )
  model$storage <- list()
  
  # Update parameter
  new_data <- data.frame(tech = c("T1", "T2"), value = c(150, 250))
  model <- update_parameter(model, "pCost", new_data)
  
  # Check data updated
  expect_equal(model$parameters$pCost$data$value, c(150, 250))
  
  # Check tracking
  expect_true("pCost" %in% model$storage$modified$parameters)
})

test_that("update_parameter with auto_save persists changes", {
  skip_if_not_installed("arrow")
  
  model <- create_test_model(
    name = "test_autosave",
    sets = list(region = list(members = c("R1"))),
    parameters = list(
      pDemand = list(
        name = "pDemand",
        sets = c("region"),
        data = data.frame(region = "R1", value = 100)
      )
    )
  )
  
  # Save initial model
  temp_dir <- file.path(tempdir(), "test_autosave")
  model <- save_model(model, temp_dir, format = "csv", verbose = FALSE)
  
  # Update with auto_save
  new_data <- data.frame(region = "R1", value = 150)
  model <- update_parameter(model, "pDemand", new_data, auto_save = TRUE)
  
  # Load fresh from disk
  reloaded <- load_model(temp_dir, verbose = FALSE)
  
  # Check changes persisted
  expect_equal(reloaded$parameters$pDemand$data$value, 150)
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("keep_in_memory option controls data retention", {
  skip_if_not_installed("arrow")
  
  model <- create_test_model(
    name = "test_memory",
    sets = list(tech = list(members = c("T1"))),
    parameters = list(
      pValue = list(
        name = "pValue",
        sets = c("tech"),
        data = data.frame(tech = "T1", value = 42)
      )
    )
  )
  
  temp_dir <- file.path(tempdir(), "test_memory")
  
  # Save with keep_in_memory = FALSE
  model_thinned <- save_model(
    model,
    temp_dir,
    format = "csv",
    keep_in_memory = FALSE,
    verbose = FALSE
  )
  
  # Data should be NULL
  expect_true(is.null(model_thinned$parameters$pValue$data) ||
              nrow(model_thinned$parameters$pValue$data) == 0)
  
  # But files should exist
  expect_true(file.exists(file.path(temp_dir, "parameters", "pValue", "data.csv")))
  
  # Can still load it back
  reloaded <- load_model(temp_dir, verbose = FALSE)
  expect_equal(reloaded$parameters$pValue$data$value, 42)
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("model.rds contains complete structure", {
  skip_if_not_installed("arrow")
  
  model <- create_test_model(
    name = "test_structure",
    sets = list(tech = list(members = c("T1", "T2"))),
    parameters = list(
      pCost = list(
        name = "pCost",
        desc = "Technology cost",
        sets = c("tech"),
        data = data.frame(tech = c("T1", "T2"), value = c(10, 20))
      )
    ),
    mappings = list(
      mTech = list(
        name = "mTech",
        sets = c("tech"),
        data = data.frame(tech = c("T1", "T2"))
      )
    ),
    equations = list(
      eqCost = list(name = "eqCost", desc = "Cost equation")
    ),
    variables = list(
      vActivity = list(name = "vActivity", desc = "Activity variable")
    )
  )
  model$description <- "Test model"
  
  temp_dir <- file.path(tempdir(), "test_structure")
  save_model(model, temp_dir, format = "csv", keep_in_memory = FALSE, verbose = FALSE)
  
  # Load just the RDS file directly
  model_rds <- readRDS(file.path(temp_dir, "model.rds"))
  
  # Check all structure is present
  expect_equal(model_rds$name, "test_structure")
  expect_equal(model_rds$description, "Test model")
  expect_true("tech" %in% names(model_rds$sets))
  expect_true("pCost" %in% names(model_rds$parameters))
  expect_equal(model_rds$parameters$pCost$desc, "Technology cost")
  expect_true("mTech" %in% names(model_rds$mappings))
  expect_true("eqCost" %in% names(model_rds$equations))
  expect_true("vActivity" %in% names(model_rds$variables))
  
  # Data should be NULL (was saved with keep_in_memory = FALSE)
  expect_true(is.null(model_rds$parameters$pCost$data) ||
              nrow(model_rds$parameters$pCost$data) == 0)
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("no metadata files in subdirectories", {
  skip_if_not_installed("arrow")
  
  model <- create_test_model(
    name = "test_no_metadata",
    sets = list(region = list(members = c("R1"))),
    parameters = list(
      pValue = list(
        name = "pValue",
        sets = c("region"),
        data = data.frame(region = "R1", value = 100)
      )
    ),
    mappings = list(
      mRegion = list(
        name = "mRegion",
        sets = c("region"),
        data = data.frame(region = "R1")
      )
    )
  )
  
  temp_dir <- file.path(tempdir(), "test_no_metadata")
  save_model(model, temp_dir, format = "csv", verbose = FALSE)
  
  # Check no metadata.json or metadata.rds in parameter directories
  expect_false(file.exists(file.path(temp_dir, "parameters", "pValue", "metadata.json")))
  expect_false(file.exists(file.path(temp_dir, "parameters", "pValue", "metadata.rds")))
  
  # Check no metadata files in mapping directories
  expect_false(file.exists(file.path(temp_dir, "mappings", "mRegion", "metadata.json")))
  expect_false(file.exists(file.path(temp_dir, "mappings", "mRegion", "metadata.rds")))
  
  # Only model.rds in root should exist
  expect_true(file.exists(file.path(temp_dir, "model.rds")))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})




# =============================================================================
# A detached table that is genuinely empty is not a missing table
# =============================================================================
#
# energyRt detaches a parameter's data and leaves a summary in misc$onDisk,
# including the row count. A map that legitimately holds no tuples (e.g.
# mvTechPhaseOut in a scenario with no phase-outs) therefore claims external
# data while having nothing to load. That is not the same failure as a path
# that cannot be read, and reporting it as one halted the import of a
# perfectly good scenario.
# =============================================================================

test_that("a recorded-empty table reads back as empty, not as an error", {
  m <- new_model(
    name = "empty_ondisk",
    mappings = list(
      mGone = new_mapping("mGone", desc = "no tuples", dims = c("tech", "year"),
                          active_dims = c("tech", "year"), data = data.frame())
    )
  )
  # what energyRt's detach leaves behind for an empty table: the record is
  # keyed by slot name
  m$mappings$mGone$misc <- list(onDisk = list(data = list(dim = c(0L, 2L))),
                                inMemory = FALSE)

  d <- get_data(m, "mGone", type = "mapping")
  expect_false(is.null(d))
  expect_equal(nrow(d), 0L)
  # columns come from the declared dims, so arity checks downstream still mean
  # something
  expect_equal(names(d), c("tech", "year"))
})

test_that("a table that claims rows but cannot be loaded still errors", {
  m <- new_model(
    name = "lost_ondisk",
    mappings = list(
      mLost = new_mapping("mLost", desc = "unreachable", dims = c("tech", "year"),
                          active_dims = c("tech", "year"), data = data.frame())
    )
  )
  m$mappings$mLost$misc <- list(onDisk = list(data = list(dim = c(239L, 2L))),
                                path = file.path(tempdir(), "no_such_dir"),
                                inMemory = FALSE)

  expect_error(get_data(m, "mLost", type = "mapping"),
               "declares data outside")
})

test_that("a parameter's empty read carries a value column", {
  m <- new_model(
    name = "empty_param",
    parameters = list(
      pGone = new_parameter("pGone", desc = "no rows", dims = c("tech", "year"),
                            active_dims = c("tech", "year"),
                            data = data.frame(), defVal = NULL)
    )
  )
  m$parameters$pGone$misc <- list(onDisk = list(data = list(dim = c(0L, 3L))),
                                  inMemory = FALSE)

  d <- get_data(m, "pGone", type = "parameter")
  expect_equal(nrow(d), 0L)
  expect_equal(names(d), c("tech", "year", "value"))
})
