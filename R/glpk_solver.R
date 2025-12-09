#' Solve GLPK model using glpkAPI
#'
#' @param model_dir Path to directory containing .mod and .dat files
#' @param mod_file Name of .mod file (default: searches for *.mod)
#' @param dat_file Name of .dat file (default: searches for *.dat)
#' @param output_dir Directory for solution output (default: model_dir/output)
#' @param verbose Logical, print solver output (default: TRUE)
#' @param save_solution Logical, save solution to CSV files (default: FALSE)
#' @param export_mps Logical, export model to MPS format (default: FALSE)
#' @return List with solution data and status
#' @export
execute_glpkAPI <- function(model_dir,
                            mod_file = NULL,
                            dat_file = NULL,
                            output_dir = NULL,
                            verbose = TRUE,
                            save_solution = FALSE,
                            export_vars = FALSE,
                            export_mps = FALSE) {

  # Check if glpkAPI is available
  if (!requireNamespace("glpkAPI", quietly = TRUE)) {
    stop("Package 'glpkAPI' is required but not installed. Install it with: install.packages('glpkAPI')")
  }

  # Validate model directory
  if (!dir.exists(model_dir)) {
    stop("Model directory does not exist: ", model_dir)
  }

  # Find .mod file if not specified
  if (is.null(mod_file)) {
    mod_files <- list.files(model_dir, pattern = "\\.mod$", full.names = FALSE)
    if (length(mod_files) == 0) {
      stop("No .mod file found in ", model_dir)
    }
    if (length(mod_files) > 1) {
      warning("Multiple .mod files found, using: ", mod_files[1])
    }
    mod_file <- mod_files[1]
  }

  # Find .dat file if not specified
  if (is.null(dat_file)) {
    dat_files <- list.files(model_dir, pattern = "\\.dat$", full.names = FALSE)
    if (length(dat_files) == 0) {
      stop("No .dat file found in ", model_dir)
    }
    if (length(dat_files) > 1) {
      warning("Multiple .dat files found, using: ", dat_files[1])
    }
    dat_file <- dat_files[1]
  }

  # Set output directory (use absolute path from the start)
  if (is.null(output_dir)) {
    output_dir <- file.path(normalizePath(model_dir, mustWork = TRUE), "output")
  } else {
    output_dir <- normalizePath(output_dir, mustWork = FALSE)
  }

  # Create output directory if needed (before changing directories)
  if (save_solution && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) cat("Created output directory:", output_dir, "\n")
  }

  # Full paths
  mod_path <- file.path(model_dir, mod_file)
  dat_path <- file.path(model_dir, dat_file)

  if (!file.exists(mod_path)) {
    stop("Model file not found: ", mod_path)
  }
  if (!file.exists(dat_path)) {
    stop("Data file not found: ", dat_path)
  }

  if (verbose) {
    cat("Model directory:", model_dir, "\n")
    cat("Model file:", mod_file, "\n")
    cat("Data file:", dat_file, "\n")
    cat("Output directory:", output_dir, "\n\n")
  }

  # Save current directory and change to model directory
  old_dir <- getwd()
  on.exit(setwd(old_dir))
  setwd(model_dir)

  # Initialize GLPK problem
  lp <- glpkAPI::initProbGLPK()

  # Control terminal output
  if (!verbose) {
    # Disable terminal output for model reading/generation
    glpkAPI::termOutGLPK(glpkAPI::GLP_OFF)
  }

  # Create MathProg translator workspace
  tran <- glpkAPI::mplAllocWkspGLPK()

  # Ensure cleanup on exit
  on.exit({
    glpkAPI::mplFreeWkspGLPK(tran)
    glpkAPI::delProbGLPK(lp)
    setwd(old_dir)
  }, add = TRUE)

  # Read model file
  if (verbose) cat("Reading model file...\n")
  ret <- glpkAPI::mplReadModelGLPK(tran, mod_file, skip = 0)
  if (!is.null(ret) && ret != 0) {
    stop("Error reading model file")
  }

  # Read data file
  if (verbose) cat("Reading data file...\n")
  ret <- glpkAPI::mplReadDataGLPK(tran, dat_file)
  if (!is.null(ret) && ret != 0) {
    stop("Error reading data file")
  }

  # Generate the model
  if (verbose) cat("Generating model...\n")
  ret <- glpkAPI::mplGenerateGLPK(tran, fname = NULL)
  if (!is.null(ret) && ret != 0) {
    stop("Error generating model")
  }

  # Build the problem
  if (verbose) cat("Building problem...\n")
  glpkAPI::mplBuildProbGLPK(tran, lp)

  # Get problem size
  n_rows <- glpkAPI::getNumRowsGLPK(lp)
  n_cols <- glpkAPI::getNumColsGLPK(lp)
  if (verbose) {
    cat("Problem size:", n_rows, "rows,", n_cols, "columns\n\n")
  }

  # Export MPS file if requested
  if (export_mps) {
    mps_file <- file.path(model_dir, "model.mps")
    if (verbose) {
      cat("Exporting MPS file...\n")
    }
    glpkAPI::writeMPSGLPK(lp, fmt = glpkAPI::GLP_MPS_FILE, fname = basename(mps_file))
    if (verbose) {
      cat("MPS file saved:", mps_file, "\n\n")
    }
  }

  # Configure solver parameters
  if (verbose) {
    cat("Solving with simplex method...\n")
  }

  # Enable presolver (key for performance and numerical stability)
  glpkAPI::setSimplexParmGLPK(glpkAPI::PRESOLVE, glpkAPI::GLP_ON)

  # Set message level
  if (verbose) {
    glpkAPI::setSimplexParmGLPK(glpkAPI::MSG_LEV, glpkAPI::GLP_MSG_ALL)
  } else {
    glpkAPI::setSimplexParmGLPK(glpkAPI::MSG_LEV, glpkAPI::GLP_MSG_OFF)
  }

  # Solve
  solve_status <- glpkAPI::solveSimplexGLPK(lp)

  # Get solution status
  status <- glpkAPI::getSolStatGLPK(lp)

  status_msg <- switch(as.character(status),
                       "1" = "undefined",
                       "2" = "feasible",
                       "3" = "infeasible",
                       "4" = "no feasible solution exists",
                       "5" = "optimal",
                       "6" = "unbounded",
                       "unknown")

  if (verbose) {
    cat("\n=== SOLUTION RESULTS ===\n")
    cat("Solution status:", status, "(", status_msg, ")\n")
  }

  # Get objective value
  obj_val <- glpkAPI::getObjValGLPK(lp)
  if (verbose) {
    cat("Objective value:", obj_val, "\n")
  }

  # Execute post-solve processing (output statements in .mod file)
  if (verbose) cat("\nExecuting post-solve processing...\n")
  glpkAPI::mplPostsolveGLPK(tran, lp, sol = glpkAPI::GLP_SOL)
  if (verbose) cat("Post-solve processing completed.\n")

  # Extract solution
  if (verbose) cat("\nExtracting solution...\n")
  solution <- glpkAPI::getColsPrimGLPK(lp)
  var_names <- sapply(1:n_cols, function(i) glpkAPI::getColNameGLPK(lp, i))

  # Create solution data frame
  solution_df <- data.frame(
    variable = var_names,
    value = solution,
    stringsAsFactors = FALSE
  )

  # Non-zero variables
  non_zero <- solution_df[abs(solution_df$value) > 1e-6, ]

  if (verbose) {
    cat("Number of non-zero variables:", nrow(non_zero), "out of", n_cols, "\n")
  }

  # Save results
  if (save_solution) {
    # Output directory is already absolute path
    solution_file <- file.path(output_dir, "solution.csv")
    nonzero_file <- file.path(output_dir, "solution_nonzero.csv")

    write.csv(solution_df, solution_file, row.names = FALSE)
    write.csv(non_zero, nonzero_file, row.names = FALSE)

    if (verbose) {
      cat("\nSolution saved to:", solution_file, "\n")
      cat("Non-zero variables saved to:", nonzero_file, "\n")
    }
    
    # Export individual variable CSV files if requested
    if (export_vars) {
      if (verbose) cat("Exporting individual variable CSV files...\n")
      
      .export_variable_csvs(non_zero, output_dir, verbose)
      
      if (verbose) cat("Variable CSV export complete.\n")
    }
  }

  # Return results
  invisible(list(
    status = status,
    status_message = status_msg,
    objective = obj_val,
    n_variables = n_cols,
    n_constraints = n_rows,
    solution = solution_df,
    solution_nonzero = non_zero
  ))
}


#' Export individual variable CSV files from solution
#' @keywords internal
.export_variable_csvs <- function(solution_df, output_dir, verbose = FALSE) {
  # Parse variable names to extract base name and indices
  # Format: VarName[index1,index2,...] or VarName (scalar)
  
  # Split by variable name
  var_pattern <- "^([A-Za-z_][A-Za-z0-9_]*)(?:\\[(.+)\\])?$"
  
  parsed <- do.call(rbind, lapply(seq_len(nrow(solution_df)), function(i) {
    var_full <- solution_df$variable[i]
    value <- solution_df$value[i]
    
    matches <- regmatches(var_full, regexec(var_pattern, var_full))[[1]]
    
    if (length(matches) >= 2) {
      var_name <- matches[2]
      indices <- if (length(matches) >= 3 && nchar(matches[3]) > 0) {
        matches[3]
      } else {
        NA_character_
      }
      
      data.frame(
        var_name = var_name,
        indices = indices,
        value = value,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  }))
  
  if (is.null(parsed) || nrow(parsed) == 0) {
    if (verbose) cat("No variables to export\n")
    return(invisible(NULL))
  }
  
  # Group by variable name
  var_names <- unique(parsed$var_name)
  
  n_exported <- 0
  for (vname in var_names) {
    var_data <- parsed[parsed$var_name == vname, ]
    
    # Scalar variable (no indices)
    if (all(is.na(var_data$indices))) {
      df <- data.frame(value = var_data$value, stringsAsFactors = FALSE)
    } else {
      # Multi-dimensional variable
      # Parse indices: "i1,i2,i3" -> separate columns
      indices_list <- strsplit(var_data$indices, ",", fixed = TRUE)
      max_dims <- max(sapply(indices_list, length))
      
      # Create matrix of indices
      indices_matrix <- t(sapply(indices_list, function(x) {
        c(x, rep(NA_character_, max_dims - length(x)))
      }))
      
      # Create dataframe with index columns + value
      df <- as.data.frame(indices_matrix, stringsAsFactors = FALSE)
      names(df) <- paste0("dim", seq_len(ncol(df)))
      df$value <- var_data$value
    }
    
    # Write to CSV
    csv_file <- file.path(output_dir, paste0(vname, ".csv"))
    write.csv(df, csv_file, row.names = FALSE)
    n_exported <- n_exported + 1
  }
  
  if (verbose) {
    cat("Exported", n_exported, "variable CSV files\n")
  }
  
  invisible(n_exported)
}



#' Get GLPK model statistics without solving
#'
#' @param model_dir Path to directory containing .mod and .dat files
#' @param mod_file Name of .mod file (default: searches for *.mod)
#' @param dat_file Name of .dat file (default: searches for *.dat)
#' @param verbose Logical, print statistics (default: TRUE)
#' @return List with model statistics (n_rows, n_cols, n_nonzeros)
#' @export
get_glpk_model_stats <- function(model_dir,
                                  mod_file = NULL,
                                  dat_file = NULL,
                                  verbose = TRUE) {

  # Check if glpkAPI is available
  if (!requireNamespace("glpkAPI", quietly = TRUE)) {
    stop("Package 'glpkAPI' is required but not installed. Install it with: install.packages('glpkAPI')")
  }

  # Validate model directory
  if (!dir.exists(model_dir)) {
    stop("Model directory does not exist: ", model_dir)
  }

  # Find .mod file if not specified
  if (is.null(mod_file)) {
    mod_files <- list.files(model_dir, pattern = "\\.mod$", full.names = FALSE)
    if (length(mod_files) == 0) {
      stop("No .mod file found in ", model_dir)
    }
    if (length(mod_files) > 1) {
      warning("Multiple .mod files found, using: ", mod_files[1])
    }
    mod_file <- mod_files[1]
  }

  # Find .dat file if not specified
  if (is.null(dat_file)) {
    dat_files <- list.files(model_dir, pattern = "\\.dat$", full.names = FALSE)
    if (length(dat_files) == 0) {
      stop("No .dat file found in ", model_dir)
    }
    if (length(dat_files) > 1) {
      warning("Multiple .dat files found, using: ", dat_files[1])
    }
    dat_file <- dat_files[1]
  }

  # Full paths
  mod_path <- file.path(model_dir, mod_file)
  dat_path <- file.path(model_dir, dat_file)

  if (!file.exists(mod_path)) {
    stop("Model file not found: ", mod_path)
  }
  if (!file.exists(dat_path)) {
    stop("Data file not found: ", dat_path)
  }

  if (verbose) {
    cat("=== GLPK Model Statistics ===\n")
    cat("Model directory:", model_dir, "\n")
    cat("Model file:", mod_file, "\n")
    cat("Data file:", dat_file, "\n\n")
  }

  # Save current directory and change to model directory
  old_dir <- getwd()
  on.exit(setwd(old_dir))
  setwd(model_dir)

  # Initialize GLPK problem
  lp <- glpkAPI::initProbGLPK()

  # Create MathProg translator workspace
  tran <- glpkAPI::mplAllocWkspGLPK()

  # Ensure cleanup on exit
  on.exit({
    glpkAPI::mplFreeWkspGLPK(tran)
    glpkAPI::delProbGLPK(lp)
    setwd(old_dir)
  }, add = TRUE)

  # Read model file
  if (verbose) cat("Reading model file...\n")
  ret <- glpkAPI::mplReadModelGLPK(tran, mod_file, skip = 0)
  if (!is.null(ret) && ret != 0) {
    stop("Error reading model file")
  }

  # Read data file
  if (verbose) cat("Reading data file...\n")
  ret <- glpkAPI::mplReadDataGLPK(tran, dat_file)
  if (!is.null(ret) && ret != 0) {
    stop("Error reading data file")
  }

  # Generate the model
  if (verbose) cat("Generating model...\n")
  ret <- glpkAPI::mplGenerateGLPK(tran, fname = NULL)
  if (!is.null(ret) && ret != 0) {
    stop("Error generating model")
  }

  # Build the problem (but don't solve)
  if (verbose) cat("Building problem...\n")
  glpkAPI::mplBuildProbGLPK(tran, lp)

  # Get problem statistics
  n_rows <- glpkAPI::getNumRowsGLPK(lp)
  n_cols <- glpkAPI::getNumColsGLPK(lp)
  n_nz <- glpkAPI::getNumNnzGLPK(lp)

  # Extract variable names and count by base name
  var_names <- sapply(1:n_cols, function(i) glpkAPI::getColNameGLPK(lp, i))
  var_base_names <- gsub("\\[.*\\]$", "", var_names)
  var_counts <- table(var_base_names)

  # Extract constraint names and count by base name
  con_names <- sapply(1:n_rows, function(i) glpkAPI::getRowNameGLPK(lp, i))
  con_base_names <- gsub("\\[.*\\]$", "", con_names)
  con_counts <- table(con_base_names)

  if (verbose) {
    cat("\n=== Model Statistics ===\n")
    cat("Total constraints (rows):", n_rows, "\n")
    cat("Total variables (columns):", n_cols, "\n")
    cat("Non-zero elements:", n_nz, "\n")
    cat("Density:", sprintf("%.2f%%", 100 * n_nz / (n_rows * n_cols)), "\n\n")

    cat("=== Variables by Type ===\n")
    var_df <- data.frame(
      Variable = names(var_counts),
      Count = as.integer(var_counts),
      stringsAsFactors = FALSE
    )
    var_df <- var_df[order(-var_df$Count), ]
    print(var_df, row.names = FALSE)

    cat("\n=== Constraints by Type ===\n")
    con_df <- data.frame(
      Constraint = names(con_counts),
      Count = as.integer(con_counts),
      stringsAsFactors = FALSE
    )
    con_df <- con_df[order(-con_df$Count), ]
    print(con_df, row.names = FALSE)
  }

  # Return statistics
  invisible(list(
    n_rows = n_rows,
    n_cols = n_cols,
    n_nonzeros = n_nz,
    density = n_nz / (n_rows * n_cols),
    variables = var_counts,
    constraints = con_counts
  ))
}


#' Export GLPK model to MPS or CPLEX LP format
#'
#' @param model_dir Path to directory containing .mod and .dat files
#' @param mod_file Name of .mod file (default: searches for *.mod)
#' @param dat_file Name of .dat file (default: searches for *.dat)
#' @param output_file Path to output file (extension determines format: .mps or .lp)
#' @param format Format to export: "MPS" or "CPLEX_LP" (default: auto-detect from output_file)
#' @param verbose Logical, print progress messages (default: TRUE)
#' @return Invisible path to created file
#' @export
export_glpk_model <- function(model_dir,
                               mod_file = NULL,
                               dat_file = NULL,
                               output_file,
                               format = NULL,
                               verbose = TRUE) {

  # Check if glpkAPI is available
  if (!requireNamespace("glpkAPI", quietly = TRUE)) {
    stop("Package 'glpkAPI' is required but not installed. Install it with: install.packages('glpkAPI')")
  }

  # Validate model directory
  if (!dir.exists(model_dir)) {
    stop("Model directory does not exist: ", model_dir)
  }

  # Find .mod file if not specified
  if (is.null(mod_file)) {
    mod_files <- list.files(model_dir, pattern = "\\.mod$", full.names = FALSE)
    if (length(mod_files) == 0) {
      stop("No .mod file found in ", model_dir)
    }
    if (length(mod_files) > 1) {
      warning("Multiple .mod files found, using: ", mod_files[1])
    }
    mod_file <- mod_files[1]
  }

  # Find .dat file if not specified
  if (is.null(dat_file)) {
    dat_files <- list.files(model_dir, pattern = "\\.dat$", full.names = FALSE)
    if (length(dat_files) == 0) {
      stop("No .dat file found in ", model_dir)
    }
    if (length(dat_files) > 1) {
      warning("Multiple .dat files found, using: ", dat_files[1])
    }
    dat_file <- dat_files[1]
  }

  # Auto-detect format from file extension if not specified
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(output_file))
    format <- switch(ext,
                     "mps" = "MPS",
                     "lp" = "CPLEX_LP",
                     stop("Cannot auto-detect format from extension '", ext, "'. Use .mps or .lp"))
  }

  # Validate format
  format <- match.arg(format, c("MPS", "CPLEX_LP"))

  # Full paths
  mod_path <- file.path(model_dir, mod_file)
  dat_path <- file.path(model_dir, dat_file)

  if (!file.exists(mod_path)) {
    stop("Model file not found: ", mod_path)
  }
  if (!file.exists(dat_path)) {
    stop("Data file not found: ", dat_path)
  }

  if (verbose) {
    cat("Exporting GLPK model to", format, "format\n")
    cat("Model directory:", model_dir, "\n")
    cat("Model file:", mod_file, "\n")
    cat("Data file:", dat_file, "\n")
    cat("Output file:", output_file, "\n\n")
  }

  # Save current directory and change to model directory
  old_dir <- getwd()
  on.exit(setwd(old_dir))
  setwd(model_dir)

  # Initialize GLPK problem
  lp <- glpkAPI::initProbGLPK()

  # Create MathProg translator workspace
  tran <- glpkAPI::mplAllocWkspGLPK()

  # Ensure cleanup on exit
  on.exit({
    glpkAPI::mplFreeWkspGLPK(tran)
    glpkAPI::delProbGLPK(lp)
    setwd(old_dir)
  }, add = TRUE)

  # Read model file
  if (verbose) cat("Reading model file...\n")
  ret <- glpkAPI::mplReadModelGLPK(tran, mod_file, skip = 0)
  if (!is.null(ret) && ret != 0) {
    stop("Error reading model file")
  }

  # Read data file
  if (verbose) cat("Reading data file...\n")
  ret <- glpkAPI::mplReadDataGLPK(tran, dat_file)
  if (!is.null(ret) && ret != 0) {
    stop("Error reading data file")
  }

  # Generate the model
  if (verbose) cat("Generating model...\n")
  ret <- glpkAPI::mplGenerateGLPK(tran, fname = NULL)
  if (!is.null(ret) && ret != 0) {
    stop("Error generating model")
  }

  # Build the problem
  if (verbose) cat("Building problem...\n")
  glpkAPI::mplBuildProbGLPK(tran, lp)

  # Get problem size
  n_rows <- glpkAPI::getNumRowsGLPK(lp)
  n_cols <- glpkAPI::getNumColsGLPK(lp)
  if (verbose) {
    cat("Problem size:", n_rows, "rows,", n_cols, "columns\n")
  }

  # Create output directory if needed
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) cat("Created output directory:", output_dir, "\n")
  }

  # Export to file
  if (verbose) cat("\nWriting", format, "file...\n")

  if (format == "MPS") {
    glpkAPI::writeMPSGLPK(lp, fmt = glpkAPI::GLP_MPS_FILE, fname = basename(output_file))
  } else {  # CPLEX_LP
    glpkAPI::writeLPGLPK(lp, fname = basename(output_file))
  }

  if (verbose) {
    cat("Model exported successfully to:", output_file, "\n")
  }

  invisible(output_file)
}


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

  # Set output directory
  if (is.null(output_dir)) {
    output_dir <- dirname(mod_file)
  }

  # Create output file path
  sol_file <- file.path(output_dir, "solution.sol")

  # Build glpsol command
  args <- c(
    "-m", mod_file,
    "-d", dat_file,
    "-o", sol_file
  )

  # Run glpsol
  message("Solving with GLPK...")
  result <- system2(glpsol_path, args = args, stdout = TRUE, stderr = TRUE)

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
#' Solves an optimization model using GLPK solver (glpkAPI or glpsol)
#'
#' @param model Optional model object (for loading results back)
#' @param model_dir Directory containing model files (model.mod, data.dat, etc.)
#' @param method Solver method: "glpkAPI" (default, uses execute_glpkAPI), "glpsol" (uses system2 with glpsol executable)
#' @param verbose Logical; print progress messages (default: TRUE)
#' @param load_results Logical; load solution CSV results back into model object (default: !is.null(model))
#' @param glpsol_path Path to glpsol executable (default: "glpsol", only used if method="glpsol")
#' @param timeout Numeric; timeout in seconds (default: NULL, no timeout)
#' @return List with: model (if load_results=TRUE), success, objective, solve_time, status
#' @export
solve_gmpl <- function(model = NULL,
                       model_dir,
                       method = "glpkAPI",
                       verbose = TRUE,
                       load_results = !is.null(model),
                       glpsol_path = "glpsol",
                       timeout = NULL) {

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
  
  # Choose solver method
  method <- tolower(method)
  if (!(method %in% c("glpkapi", "glpsol"))) {
    stop("Invalid method. Must be 'glpkAPI' or 'glpsol'")
  }
  
  start_time <- Sys.time()
  
  if (method == "glpkapi") {
    # Use glpkAPI (default and recommended)
    if (verbose) cat("Solving with glpkAPI...\n")
    
    glpk_result <- execute_glpkAPI(
      model_dir = gmpl_dir,
      mod_file = "model.mod",
      dat_file = "data.dat",
      output_dir = file.path(gmpl_dir, "solution"),
      verbose = verbose,
      save_solution = TRUE,
      export_vars = export_vars,
      export_mps = export_mps
    )
    
    # Standardize result format
    result <- list(
      success = (!is.null(glpk_result$status) && glpk_result$status == 5),  # 5 = optimal
      objective = glpk_result$objective,
      status = glpk_result$status_message,
      status_code = glpk_result$status,
      n_variables = glpk_result$n_variables,
      n_constraints = glpk_result$n_constraints,
      solution = glpk_result$solution,
      solution_nonzero = glpk_result$solution_nonzero
    )
    
  } else {
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
      
      if (verbose) cat(sprintf("  ✓ Loaded %s: %d rows\n", var_name, nrow(solution_data)))
      
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
