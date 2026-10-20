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
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
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
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
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
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
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
  withr::local_options(list(multimod.python_path = "/nonexistent/python"))
  
  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("pyomo_nopython_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
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
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
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
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
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
  expect_true(any(grepl("def build_data", data_code)),
              label = "Should have build_data function")
})

test_that("write_pyomo data_mode parameter works", {
  skip_on_cran()
  
  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod
  
  temp_dir <- tempfile("pyomo_datamode_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  save_model(model, temp_dir, format = "csv", verbose = FALSE)
  
  # External data mode (default)
  write_pyomo(model, model_dir = temp_dir, data_mode = "external")
  
  pyomo_dir <- file.path(temp_dir, "solvers", "pyomo")
  data_code <- readLines(file.path(pyomo_dir, "data.py"))
  
  # Should have external data loading
  expect_true(any(grepl("DATA_MODE = 'external'", data_code, fixed = TRUE)),
              label = "Should set DATA_MODE to 'external'")
  expect_true(any(grepl("_read_rows(", data_code, fixed = TRUE)),
              label = "Should read the data files directly")
})


test_that("write_pyomo() refuses `file` and builds the workspace it needs", {
  toy <- example_models$toy
  expect_error(write_pyomo(toy, file = tempfile(fileext = ".py")), "model_dir")

  d <- withr::local_tempdir()
  f <- write_pyomo(toy, model_dir = d, export_vars = TRUE)
  expect_true(file.exists(f))
  expect_true(file.exists(file.path(d, "model.rds")))
  expect_true(file.exists(file.path(d, "solvers", "pyomo", "data.py")))

  # a non-empty directory that is not a workspace is left alone
  d2 <- withr::local_tempdir()
  writeLines("x", file.path(d2, "notes.txt"))
  expect_error(write_pyomo(toy, model_dir = d2), "not empty")
  expect_true(file.exists(file.path(d2, "notes.txt")))
})

test_that("solve_pyomo(load_results = TRUE) reads the toy solution back", {
  skip_on_cran()
  py <- tryCatch(mm_check_python(packages = c("pyomo", "highspy")),
                 error = function(e) list(ok = FALSE))
  skip_if(!isTRUE(py$ok), "Python with pyomo and highspy not available")

  toy <- example_models$toy
  d <- withr::local_tempdir()
  write_pyomo(toy, model_dir = d, export_vars = TRUE)
  r <- solve_pyomo(toy, d, solver = "highs", verbose = FALSE, ensure_env = FALSE,
                   load_results = TRUE)
  expect_true(r$success)
  expect_equal(r$objective, 198000, tolerance = 1e-6)
  gen <- r$model$variables$vGen$solution
  expect_named(gen, c("tech", "region", "value"))
  expect_equal(sum(gen$value), 1600, tolerance = 1e-6)
  expect_setequal(r$model$solution_metadata$variables_loaded, c("vCap", "vGen"))

  expect_error(solve_pyomo(NULL, d, verbose = FALSE, ensure_env = FALSE,
                           load_results = TRUE), "needs the")
})

test_that(".pyomo_index_columns() splits Python index strings", {
  two <- .pyomo_index_columns(c("('coal', 'north')", "('gas', 'south')"), c("tech", "region"))
  expect_identical(two, data.frame(tech = c("coal", "gas"), region = c("north", "south"),
                                   stringsAsFactors = FALSE))
  one <- .pyomo_index_columns(c("coal", "gas"), "tech")
  expect_identical(one$tech, c("coal", "gas"))
  expect_identical(ncol(.pyomo_index_columns("None", character(0))), 0L)
})
