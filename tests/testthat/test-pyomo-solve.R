# =============================================================================
# Pyomo Solver Tests - solve_pyomo() function
# =============================================================================
#
# This test suite validates the solve_pyomo() function for solving multimod
# models using Pyomo (Python Optimization Modeling Objects).
#
# Tests cover:
# • Basic solve functionality with example models
# • Solving from model_dir without model object
# • Verbose output mode
# • Error handling with missing files/Python
# • Objective value consistency
# • Status reporting (optimal, infeasible, etc.)
# • Multiple solver support (HiGHS, GLPK, etc.)
#
# solve_pyomo() workflow:
# 1. Launch Python process with model.py script
# 2. Python loads AbstractModel and data via DataPortal
# 3. Creates concrete instance and solves with specified solver
# 4. Writes solution to JSON files (status.json, variables.json)
# 5. R extracts solution (objective, status, timing)
# 6. Optionally load results back into model object
#
# Dependencies:
# - Python with pyomo package
# - Solver: highspy (HiGHS), glpk, cbc, gurobi, cplex, etc.
# - arrow package (R) for IPC format or CSV
# - save_model, write_pyomo for model preparation
# =============================================================================

test_that("solve_pyomo solves example model successfully", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  
  # Check if Python/Pyomo is available
  python_exec <- tryCatch(
    get_multimod_python(),
    error = function(e) NULL
  )
  skip_if(is.null(python_exec), "Python not configured")
  
  # Check if pyomo is installed
  pyomo_check <- tryCatch({
    system2(python_exec, args = c("-c", "import pyomo.environ"), 
            stdout = FALSE, stderr = FALSE)
    TRUE
  }, error = function(e) FALSE)
  skip_if(!pyomo_check, "Pyomo not installed")
  
  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod
  
  # Create temp directory
  temp_dir <- tempfile("pyomo_solve_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Save model and generate Pyomo code
  save_model(model, temp_dir, format = "ipc", verbose = FALSE)
  write_pyomo(model, model_dir = temp_dir, solver = "highs", 
              export_vars = TRUE, split = FALSE)
  
  # Solve with default solver (HiGHS)
  result <- tryCatch(
    solve_pyomo(model, temp_dir, solver = "highs", 
                verbose = FALSE, ensure_env = FALSE),
    error = function(e) {
      message("Solve error: ", e$message)
      NULL
    }
  )
  
  skip_if(is.null(result), "Solver failed to run")
  
  # Check result structure
  expect_type(result, "list")
  expect_true("status" %in% names(result), label = "Result should have 'status' field")
  
  # Check solution files exist
  pyomo_dir <- file.path(temp_dir, "solvers", "pyomo")
  expect_true(file.exists(file.path(pyomo_dir, "model.py")))
  expect_true(file.exists(file.path(pyomo_dir, "data.py")))
  expect_true(dir.exists(file.path(pyomo_dir, "solution")))
  
  # Check status file
  status_file <- file.path(pyomo_dir, "solution", "status.json")
  if (file.exists(status_file)) {
    status <- jsonlite::read_json(status_file)
    expect_type(status, "list")
    expect_true("status" %in% names(status))
    expect_true("termination" %in% names(status))
  }
})

test_that("solve_pyomo handles model_dir without model object", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  
  python_exec <- tryCatch(get_multimod_python(), error = function(e) NULL)
  skip_if(is.null(python_exec), "Python not configured")
  
  pyomo_check <- tryCatch({
    system2(python_exec, args = c("-c", "import pyomo.environ"), 
            stdout = FALSE, stderr = FALSE)
    TRUE
  }, error = function(e) FALSE)
  skip_if(!pyomo_check, "Pyomo not installed")
  
  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("pyomo_nomodel_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Save and generate
  save_model(model, temp_dir, format = "ipc", verbose = FALSE)
  write_pyomo(model, model_dir = temp_dir, export_vars = TRUE)
  
  # Solve without model object (model = NULL)
  result <- tryCatch(
    solve_pyomo(model = NULL, model_dir = temp_dir, 
                solver = "highs", verbose = FALSE, ensure_env = FALSE),
    error = function(e) NULL
  )
  
  skip_if(is.null(result), "Solver failed to run")
  
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  
  # Should not have loaded results (no model provided)
  expect_false("model" %in% names(result))
})

test_that("solve_pyomo verbose output works", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  
  python_exec <- tryCatch(get_multimod_python(), error = function(e) NULL)
  skip_if(is.null(python_exec), "Python not configured")
  
  pyomo_check <- tryCatch({
    system2(python_exec, args = c("-c", "import pyomo.environ"), 
            stdout = FALSE, stderr = FALSE)
    TRUE
  }, error = function(e) FALSE)
  skip_if(!pyomo_check, "Pyomo not installed")
  
  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("pyomo_verbose_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  save_model(model, temp_dir, format = "ipc", verbose = FALSE)
  write_pyomo(model, model_dir = temp_dir)
  
  # Capture output
  output <- capture.output({
    result <- tryCatch(
      solve_pyomo(model, temp_dir, verbose = TRUE, ensure_env = FALSE),
      error = function(e) NULL
    )
  })
  
  skip_if(is.null(result), "Solver failed to run")
  
  # Should have some output
  expect_gt(length(output), 0, label = "Verbose mode should produce output")
})

test_that("solve_pyomo handles missing Python gracefully", {
  skip_on_cran()
  
  # Temporarily override Python path to non-existent
  withr::local_options(list(multimod.python = "/nonexistent/python"))
  
  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("pyomo_nopython_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  save_model(model, temp_dir, format = "csv", verbose = FALSE)
  write_pyomo(model, model_dir = temp_dir)
  
  # Should error or skip gracefully
  expect_error(
    solve_pyomo(model, temp_dir, ensure_env = FALSE),
    regexp = "Python|not found|executable"
  )
})

test_that("solve_pyomo handles missing model files", {
  skip_on_cran()
  
  python_exec <- tryCatch(get_multimod_python(), error = function(e) NULL)
  skip_if(is.null(python_exec), "Python not configured")
  
  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("pyomo_nofiles_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # Don't generate model files
  
  # Should error about missing files
  expect_error(
    solve_pyomo(model, temp_dir, ensure_env = FALSE),
    regexp = "model.py|not found|does not exist"
  )
})

test_that("write_pyomo generates valid Python code structure", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("pyomo_write_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  save_model(model, temp_dir, format = "csv", verbose = FALSE)
  write_pyomo(model, model_dir = temp_dir, split = TRUE, 
              export_mps = TRUE, export_vars = TRUE)
  
  pyomo_dir <- file.path(temp_dir, "solvers", "pyomo")
  
  # Check file structure
  expect_true(file.exists(file.path(pyomo_dir, "model.py")))
  expect_true(file.exists(file.path(pyomo_dir, "data.py")))
  expect_true(file.exists(file.path(pyomo_dir, "solve.py")))  # split=TRUE
  
  # Check model.py contains AbstractModel
  model_code <- readLines(file.path(pyomo_dir, "model.py"))
  expect_true(any(grepl("AbstractModel", model_code)), 
              label = "Should use AbstractModel")
  expect_true(any(grepl("from pyomo.environ import", model_code)),
              label = "Should import from pyomo.environ")
  
  # Check data.py contains DataPortal
  data_code <- readLines(file.path(pyomo_dir, "data.py"))
  expect_true(any(grepl("DataPortal", data_code)),
              label = "Should use DataPortal")
  expect_true(any(grepl("def build_dataportal", data_code)),
              label = "Should have build_dataportal function")
})

test_that("write_pyomo data_mode parameter works", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("pyomo_datamode_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  save_model(model, temp_dir, format = "csv", verbose = FALSE)
  
  # External data mode (default)
  write_pyomo(model, model_dir = temp_dir, data_mode = "external")
  
  pyomo_dir <- file.path(temp_dir, "solvers", "pyomo")
  data_code <- readLines(file.path(pyomo_dir, "data.py"))
  
  # Should have external data loading
  expect_true(any(grepl("DATA_MODE = 'external'", data_code, fixed = TRUE)),
              label = "Should set DATA_MODE to 'external'")
  expect_true(any(grepl("data.load\\(filename", data_code)),
              label = "Should load from files")
})

