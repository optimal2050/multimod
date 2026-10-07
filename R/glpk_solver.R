#' Solve GLPK model using glpsol command line tool
#'
#' @param mod_file Path to .mod file
#' @param dat_file Path to .dat file
#' @param output_dir Directory for solution output
#' @param glpsol_path Path to glpsol executable. If `NULL`, uses `get_multimod_glpsol()`.
#' @return List with solution data and status
#' @export
solve_glpk <- function(mod_file, dat_file, output_dir = NULL, glpsol_path = NULL) {

  # Get glpsol executable path from config if not provided
  if (is.null(glpsol_path)) {
    glpsol_path <- get_multimod_glpsol()
  }

  # glpsol runs in the model directory (below), so every path handed to it
  # must be absolute or it would resolve against the wrong place.
  mod_file <- normalizePath(mod_file, winslash = "/", mustWork = TRUE)
  dat_file <- normalizePath(dat_file, winslash = "/", mustWork = TRUE)

  # Set output directory
  if (is.null(output_dir)) {
    output_dir <- dirname(mod_file)
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  # Create output file path
  sol_file <- file.path(output_dir, "solution.sol")

  # Build glpsol command
  args <- c(
    "-m", mod_file,
    "-d", dat_file,
    "-o", sol_file
  )

  # Run glpsol from the model directory. The generated GMPL writes its
  # diagnostics (constraint_stats.csv, variable_stats.csv) to bare relative
  # filenames, which glpsol resolves against its own working directory.
  message("Solving with GLPK...")
  old_dir <- getwd()
  on.exit(setwd(old_dir), add = TRUE)
  setwd(dirname(mod_file))
  result <- system2(glpsol_path, args = args, stdout = TRUE, stderr = TRUE)
  setwd(old_dir)

  # Check if solution file was created
  if (!file.exists(sol_file)) {
    stop("Solver failed to produce solution file")
  }

  # Parse solution file
  sol_text <- readLines(sol_file)

  # Extract status
  status_line <- grep("Status:", sol_text, value = TRUE)
  status <- if (length(status_line) > 0) {
    gsub(".*Status:\\s+", "", status_line[1])
  } else {
    "UNKNOWN"
  }

  # Extract objective value
  obj_line <- grep("Objective:", sol_text, value = TRUE)
  objective <- if (length(obj_line) > 0) {
    as.numeric(gsub(".*=\\s+([0-9.-]+).*", "\\1", obj_line[1]))
  } else {
    NA
  }

  # Parse variables (columns)
  col_start <- which(grepl("Column name", sol_text))
  col_end <- which(grepl("End of output", sol_text))

  variables <- NULL
  if (length(col_start) > 0 && length(col_end) > 0) {
    var_lines <- sol_text[(col_start[1] + 1):(col_end[1] - 1)]
    var_lines <- var_lines[nchar(trimws(var_lines)) > 0]

    variables <- lapply(var_lines, function(line) {
      parts <- strsplit(trimws(line), "\\s+")[[1]]
      if (length(parts) >= 4) {
        data.frame(
          name = parts[2],
          value = as.numeric(parts[4]),
          stringsAsFactors = FALSE
        )
      }
    })

    variables <- do.call(rbind, variables[!sapply(variables, is.null)])
  }

  # Return results
  list(
    status = status,
    objective = objective,
    variables = variables,
    solution_file = sol_file
  )
}

#' Solve GMPL model using glpsol command line tool
#'
#' Solves an optimization model by running GLPK's `glpsol` executable.
#'
#' `glpsol` must be on the system: [get_multimod_glpsol()] resolves it from
#' `options("multimod.glpsol_path")`, `MULTIMOD_GLPSOL`, the YAML config, the
#' conda environment `mm_setup_python()` installs GLPK into, and finally the
#' bare name `"glpsol"` on `PATH`.
#'
#' @param model Optional model object (for loading results back)
#' @param model_dir Directory containing model files (model.mod, data.dat, etc.)
#' @param method Retained for compatibility; only `"glpsol"` is supported.
#'   `"glpkAPI"` was retired when glpkAPI left CRAN - see
#'   `drafts/glpkAPI-retired.R`.
#' @param verbose Logical; print progress messages (default: TRUE)
#' @param load_results Logical; load solution CSV results back into model object (default: !is.null(model))
#' @param glpsol_path Path to the glpsol executable; defaults to [get_multimod_glpsol()].
#' @param timeout Numeric; timeout in seconds (default: NULL, no timeout)
#' @return List with: model (if load_results=TRUE), success, objective, solve_time, status
#' @export
solve_gmpl <- function(model = NULL,
                       model_dir,
                       method = "glpsol",
                       verbose = TRUE,
                       load_results = !is.null(model),
                       glpsol_path = get_multimod_glpsol(),
                       timeout = NULL) {
  # Only the glpsol executable remains. glpkAPI was the other option until it
  # was archived from CRAN; its implementation is in drafts/glpkAPI-retired.R.
  method <- tolower(method)
  if (identical(method, "glpkapi")) {
    stop("method = 'glpkAPI' was retired: glpkAPI is no longer on CRAN.\n",
         "  Use method = 'glpsol' (the default), which needs the glpsol ",
         "executable - see get_multimod_glpsol().", call. = FALSE)
  }
  if (!identical(method, "glpsol")) {
    stop("Invalid method '", method, "'. Only 'glpsol' is supported.",
         call. = FALSE)
  }


  # Validate paths
  gmpl_dir <- file.path(model_dir, "solvers", "gmpl")
  
  if (!dir.exists(gmpl_dir)) {
    stop("GMPL directory not found: ", gmpl_dir)
  }
  
  # Read solver configuration if it exists
  config_file <- file.path(gmpl_dir, ".solver_config.rds")
  if (file.exists(config_file)) {
    config <- readRDS(config_file)
    export_vars <- isTRUE(config$export_vars)
    export_mps <- isTRUE(config$export_mps)
  } else {
    export_vars <- FALSE
    export_mps <- FALSE
  }
  

  start_time <- Sys.time()

  {
    # Use glpsol via system2
    if (verbose) cat("Solving with glpsol executable...\n")
    
    result <- .solve_gmpl_glpsol(
      gmpl_dir = gmpl_dir,
      glpsol_path = glpsol_path,
      verbose = verbose,
      timeout = timeout
    )
  }
  
  end_time <- Sys.time()
  result$solve_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Load results if requested
  if (isTRUE(load_results) && !is.null(model) && isTRUE(result$success)) {
    if (verbose) cat("Loading solution CSV results...\n")
    
    model <- read_csv_results(model, model_dir, verbose = verbose)
    result$model <- model
  }
  
  result
}

#' Internal function to solve with glpsol executable
#' @return A list with elements `success`, `objective`, `solve_time`, `status`,
#'   `exit_code`, `solver_output` and, when the solution file was parsed,
#'   `variables` (a data frame of `name` and `value`).
#' @keywords internal
.solve_gmpl_glpsol <- function(gmpl_dir, glpsol_path, verbose, timeout) {

  # Build glpsol command - use relative paths since we'll run from gmpl_dir
  args <- c(
    "-m", "model.mod",
    "-d", "data.dat",
    "-o", "solution.sol",
    "--log", "solver.log"
  )

  # Run glpsol from gmpl_dir so relative paths in model file work
  if (verbose) cat("Solving with GLPK glpsol...\n")

  start_time <- Sys.time()

  # Save current directory and change to gmpl_dir
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(gmpl_dir)

  # Use relative paths now that we're in gmpl_dir
  sol_file <- "solution.sol"
  log_file <- "solver.log"

  if (is.null(timeout)) {
    output <- system2(glpsol_path, args = args,
                     stdout = TRUE, stderr = TRUE, wait = TRUE)
    exit_code <- attr(output, "status")
    if (is.null(exit_code)) exit_code <- 0
  } else {
    # Use timeout (platform-specific)
    if (.Platform$OS.type == "windows") {
      # Windows timeout is tricky with glpsol, just run normally
      warning("Timeout not supported on Windows for glpsol")
      output <- system2(glpsol_path, args = args,
                       stdout = TRUE, stderr = TRUE, wait = TRUE)
      exit_code <- attr(output, "status")
      if (is.null(exit_code)) exit_code <- 0
    } else {
      # Unix: use timeout command
      output <- system2("timeout", args = c(as.character(timeout),
                                            glpsol_path, args),
                       stdout = TRUE, stderr = TRUE, wait = TRUE)
      exit_code <- attr(output, "status")
      if (is.null(exit_code)) exit_code <- 0
    }
  }

  end_time <- Sys.time()
  solve_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Print output if verbose
  if (verbose && length(output) > 0) {
    cat(paste(output, collapse = "\n"), "\n")
  }

  # Initialize result
  result <- list(
    success = FALSE,
    objective = NA_real_,
    solve_time = solve_time,
    status = "UNKNOWN",
    exit_code = exit_code,
    solver_output = output
  )

  # Check if solution file was created
  if (!file.exists(sol_file)) {
    if (verbose) cat("Solver failed to produce solution file\n")
    return(result)
  }

  # Parse solution file
  sol_text <- readLines(sol_file)

  # Extract status
  status_line <- grep("Status:", sol_text, value = TRUE)
  if (length(status_line) > 0) {
    result$status <- trimws(gsub(".*Status:\\s+", "", status_line[1]))
  }

  # Check for optimal/feasible solution
  result$success <- grepl("OPTIMAL|FEASIBLE", result$status, ignore.case = TRUE)

  # Extract objective value
  obj_line <- grep("Objective:", sol_text, value = TRUE)
  if (length(obj_line) > 0) {
    # Parse "Objective:  OBJ = 1234.56 (MINimum)"
    obj_match <- regexec("=\\s*([0-9eE.+-]+)", obj_line[1])
    if (obj_match[[1]][1] > 0) {
      result$objective <- as.numeric(regmatches(obj_line[1], obj_match)[[1]][2])
    }
  }

  # Parse variables (columns) - simplified version
  col_start <- which(grepl("Column name", sol_text))
  col_end <- which(grepl("^\\s*$", sol_text[seq(from = max(1, col_start[1] + 1),
                                                  to = length(sol_text))]))[1]

  if (length(col_start) > 0 && !is.na(col_end)) {
    col_end_line <- col_start[1] + col_end
    var_lines <- sol_text[(col_start[1] + 2):(col_end_line - 1)]
    var_lines <- var_lines[nchar(trimws(var_lines)) > 0]

    if (length(var_lines) > 0) {
      variables <- lapply(var_lines, function(line) {
        # Parse fixed-width format: No. Column name Activity ...
        parts <- strsplit(trimws(line), "\\s+")[[1]]
        if (length(parts) >= 4) {
          data.frame(
            name = parts[2],
            value = as.numeric(parts[4]),
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      })

      variables <- do.call(rbind, variables[!sapply(variables, is.null)])
      result$variables <- variables
    }
  }

  if (verbose) {
    cat(sprintf("Status: %s\n", result$status))
    cat(sprintf("Objective: %.2f\n", result$objective))
    cat(sprintf("Solve time: %.2f seconds\n", result$solve_time))
  }

  result
}

#' Read CSV results from GMPL solver solution directory and save to model variables
#'
#' Reads variable solution values from CSV files in the solver solution directory,
#' copies them to the model's variables/ directory, and attaches them to the model's 
#' variable objects in the same format as model data.
#'
#' @param model A multimod model object
#' @param model_dir Path to model root directory
#' @param verbose Logical; print progress messages (default: TRUE)
#' @return Updated model object with solution data attached to variables
#' @export
read_csv_results <- function(model, model_dir, verbose = TRUE) {
  
  # Solution directory is in solvers/gmpl/solution
  solution_dir <- file.path(model_dir, "solvers", "gmpl", "solution")
  
  if (!dir.exists(solution_dir)) {
    warning("Solution directory not found: ", solution_dir)
    return(model)
  }
  
  if (verbose) cat("Loading solutions from: solvers/gmpl/solution\n")
  
  # Destination directory for solution copies
  dest_dir <- file.path(model_dir, "variables")
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # Find all CSV files in solution directory
  csv_files <- list.files(solution_dir, pattern = "\\.csv$", full.names = FALSE)
  
  if (length(csv_files) == 0) {
    warning("No CSV files found in solution directory: ", solution_dir)
    return(model)
  }
  
  # Filter to variable CSV files (exclude log.csv, etc.)
  var_csv_files <- csv_files[!grepl("(log|raw_data_set|variable_list|table_output)\\.csv$", csv_files)]
  
  if (length(var_csv_files) == 0) {
    if (verbose) cat("No variable CSV files found\n")
    return(model)
  }
  
  if (verbose) cat(sprintf("Reading %d variable CSV files...\n", length(var_csv_files)))
  
  variables_loaded <- character(0)
  
  for (csv_file in var_csv_files) {
    # Extract variable name from filename
    var_name <- tools::file_path_sans_ext(csv_file)
    
    # Check if variable exists in model
    if (!var_name %in% names(model$variables)) {
      if (verbose) cat(sprintf("  Warning: Variable '%s' not found in model\n", var_name))
      next
    }
    
    # Read CSV file
    tryCatch({
      csv_path <- file.path(solution_dir, csv_file)
      solution_data <- read.csv(csv_path, stringsAsFactors = FALSE)
      
      # Skip empty files
      if (nrow(solution_data) == 0) {
        next
      }
      
      # Store in model
      model$variables[[var_name]]$solution <- solution_data
      variables_loaded <- c(variables_loaded, var_name)
      
      # Copy solution file to model root variables directory
      dest_var_dir <- file.path(dest_dir, var_name)
      if (!dir.exists(dest_var_dir)) {
        dir.create(dest_var_dir, recursive = TRUE)
      }
      dest_file <- file.path(dest_var_dir, "data.csv")
      file.copy(csv_path, dest_file, overwrite = TRUE)
      
      if (verbose) cat(sprintf("  \u2713 Loaded %s: %d rows\n", var_name, nrow(solution_data)))
      
    }, error = function(e) {
      if (verbose) cat(sprintf("  Error reading %s: %s\n", var_name, e$message))
    })
  }
  
  # Log the solution loading
  log_solution_load(model_dir, "gmpl", variables_loaded, verbose = verbose)
  
  # Add solution metadata
  model$solution_metadata <- list(
    solver = "GMPL",
    loaded_at = Sys.time(),
    variables_loaded = variables_loaded,
    format = ".csv",
    source = "solvers/gmpl/solution",
    destination = "variables/"
  )
  
  if (verbose) cat(sprintf("Solution loading complete: %d/%d variables loaded\n", 
                          length(variables_loaded), length(var_csv_files)))
  
  model
}
