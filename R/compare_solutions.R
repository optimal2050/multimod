#' Compare solutions from different solver outputs
#'
#' Directly compares variable solution files without loading full model.
#' Handles different data formats (e.g., CSV from GMPL, Arrow from JuMP).
#'
#' @param path1 Path to first solver output directory (reference/expected)
#' @param path2 Path to second solver output directory (to compare)
#' @param solver1 Solver name for path1 if it's a model root (e.g., "gmpl", "jump"). NULL for direct solution paths.
#' @param solver2 Solver name for path2 if it's a model root (e.g., "gmpl", "jump"). NULL for direct solution paths.
#' @param variables Character vector of variable names to compare. If NULL, compares all common variables.
#' @param tol Numeric tolerance for comparing values (default 1e-5)
#' @param keep What to return: "all", "differences", "matches" (default "differences")
#' @param allowAll Passed to compare::compare() - allows all transformations (default TRUE)
#' @param verbose Print comparison details (default TRUE)
#'
#' @return A list with components:
#'   - summary: Overall comparison summary
#'   - variables: List of comparison results per variable
#'   - differences: Variables with differences (if any)
#'   - objective: Objective value comparison
#'   - paths: Paths to the compared models
#'
#' @export
compare_solution <- function(
    path1,
    path2,
    solver1 = NULL,
    solver2 = NULL,
    variables = NULL,
    tol = 1e-5,
    keep = c("differences", "all", "matches"),
    allowAll = TRUE,
    verbose = TRUE
) {
  keep <- match.arg(keep)
  
  if (!requireNamespace("compare", quietly = TRUE)) {
    stop("Package 'compare' is required. Install with: install.packages('compare')")
  }
  
  if (verbose) {
    cat("=== Comparing Solutions ===\n")
    cat("Path 1:", path1, if (!is.null(solver1)) paste0(" (solver: ", solver1, ")") else "", "\n")
    cat("Path 2:", path2, if (!is.null(solver2)) paste0(" (solver: ", solver2, ")") else "", "\n\n")
  }
  
  # Find variable data locations in both paths
  vars1 <- .find_solution_variables(path1, solver1)
  vars2 <- .find_solution_variables(path2, solver2)
  
  # Determine which variables to compare
  if (is.null(variables)) {
    variables <- intersect(names(vars1), names(vars2))
  } else {
    missing1 <- setdiff(variables, names(vars1))
    missing2 <- setdiff(variables, names(vars2))
    if (length(missing1) > 0) {
      warning("Variables not found in path1: ", paste(missing1, collapse = ", "))
    }
    if (length(missing2) > 0) {
      warning("Variables not found in path2: ", paste(missing2, collapse = ", "))
    }
    variables <- intersect(variables, intersect(names(vars1), names(vars2)))
  }
  
  if (length(variables) == 0) {
    stop("No common variables found to compare")
  }
  
  if (verbose) {
    cat("Found", length(variables), "variables to compare\n")
    cat("Tolerance:", tol, "\n\n")
  }
  
  # Storage for results
  results <- list()
  differences <- character(0)
  matches <- character(0)
  
  # Compare each variable
  for (var_name in variables) {
    if (verbose) cat("Comparing", var_name, "... ")
    
    # Load data from path1
    data1 <- .load_variable_data(vars1[[var_name]])
    
    # Load data from path2
    data2 <- .load_variable_data(vars2[[var_name]])
    
    # Handle missing data
    if (is.null(data1) && is.null(data2)) {
      if (verbose) cat("no data in either path\n")
      results[[var_name]] <- list(
        status = "no_data",
        message = "No solution data found"
      )
      next
    }
    
    if (is.null(data1)) {
      if (verbose) cat("missing in path1\n")
      results[[var_name]] <- list(
        status = "missing_path1",
        message = "No solution data in path1",
        rows_path2 = nrow(data2)
      )
      differences <- c(differences, var_name)
      next
    }
    
    if (is.null(data2)) {
      if (verbose) cat("missing in path2\n")
      results[[var_name]] <- list(
        status = "missing_path2",
        message = "No solution data in path2",
        rows_path1 = nrow(data1)
      )
      differences <- c(differences, var_name)
      next
    }
    
    # Normalize column names for comparison
    # Replace dimension columns (dim1, dim2, etc.) with standard names
    # and ensure value column is consistently named
    data1_normalized <- data1
    data2_normalized <- data2
    
    # Standardize column names: if one has dim1,dim2,... and other has actual names,
    # use positional matching
    if (ncol(data1) == ncol(data2)) {
      # Check if column names differ
      if (!identical(names(data1), names(data2))) {
        # Use generic column names for comparison
        std_names <- c(paste0("col", seq_len(ncol(data1) - 1)), "value")
        names(data1_normalized) <- std_names
        names(data2_normalized) <- std_names
      }
    }
    
    # Compare data frames using compare package
    comp <- compare::compare(data1_normalized, data2_normalized, allowAll = allowAll)
    
    # Check if identical (within tolerance for numeric columns)
    is_identical <- comp$result
    
    # For numeric differences, apply tolerance
    if (!is_identical && any(sapply(data1_normalized, is.numeric))) {
      # Manual tolerance check for numeric columns
      numeric_cols <- names(data1_normalized)[sapply(data1_normalized, is.numeric)]
      
      # Check dimensions first
      if (nrow(data1_normalized) == nrow(data2_normalized) && ncol(data1_normalized) == ncol(data2_normalized)) {
        # Check non-numeric columns are identical
        non_numeric_cols <- setdiff(names(data1_normalized), numeric_cols)
        non_numeric_match <- TRUE
        if (length(non_numeric_cols) > 0) {
          for (col in non_numeric_cols) {
            if (!identical(data1_normalized[[col]], data2_normalized[[col]])) {
              non_numeric_match <- FALSE
              break
            }
          }
        }
        
        # Check numeric columns within tolerance
        numeric_match <- TRUE
        max_diff <- 0
        for (col in numeric_cols) {
          diff <- abs(data1_normalized[[col]] - data2_normalized[[col]])
          max_col_diff <- max(diff, na.rm = TRUE)
          if (max_col_diff > max_diff) max_diff <- max_col_diff
          if (max_col_diff > tol) {
            numeric_match <- FALSE
          }
        }
        
        if (non_numeric_match && numeric_match) {
          is_identical <- TRUE
          if (verbose) cat("match (within tolerance, max diff:", signif(max_diff, 3), ")\n")
          results[[var_name]] <- list(
            status = "match",
            message = "Identical within tolerance",
            max_diff = max_diff,
            rows = nrow(data1_normalized)
          )
          matches <- c(matches, var_name)
          next
        }
      }
    }
    
    if (is_identical) {
      if (verbose) cat("exact match\n")
      results[[var_name]] <- list(
        status = "exact_match",
        message = "Exactly identical",
        rows = nrow(data1_normalized)
      )
      matches <- c(matches, var_name)
    } else {
      if (verbose) cat("DIFFERENT\n")
      results[[var_name]] <- list(
        status = "different",
        message = "Data differs between paths",
        comparison = comp,
        rows_path1 = nrow(data1_normalized),
        rows_path2 = nrow(data2_normalized)
      )
      differences <- c(differences, var_name)
    }
  }
  
  # Extract objective values
  obj1 <- NULL
  obj2 <- NULL
  if ("vObjective" %in% names(vars1)) {
    obj_data1 <- .load_variable_data(vars1[["vObjective"]])
    if (!is.null(obj_data1) && "value" %in% names(obj_data1) && nrow(obj_data1) > 0) {
      obj1 <- obj_data1$value[1]
    }
  }
  if ("vObjective" %in% names(vars2)) {
    obj_data2 <- .load_variable_data(vars2[["vObjective"]])
    if (!is.null(obj_data2) && "value" %in% names(obj_data2) && nrow(obj_data2) > 0) {
      obj2 <- obj_data2$value[1]
    }
  }
  
  objective_comp <- list(
    path1 = obj1,
    path2 = obj2,
    difference = if (!is.null(obj1) && !is.null(obj2)) abs(obj1 - obj2) else NA,
    relative_diff = if (!is.null(obj1) && !is.null(obj2) && obj1 != 0) {
      abs(obj1 - obj2) / abs(obj1)
    } else NA
  )
  
  # Summary
  summary_text <- sprintf(
    "Variables compared: %d\n  Matches: %d\n  Differences: %d\n  Missing/No data: %d",
    length(variables),
    length(matches),
    length(differences),
    length(variables) - length(matches) - length(differences)
  )
  
  if (verbose) {
    cat("\n=== Comparison Summary ===\n")
    cat(summary_text, "\n")
    if (!is.null(obj1) && !is.null(obj2)) {
      cat("\nObjective Values:\n")
      cat(sprintf("  Path 1: %.10e\n", obj1))
      cat(sprintf("  Path 2: %.10e\n", obj2))
      cat(sprintf("  Absolute difference: %.10e\n", objective_comp$difference))
      cat(sprintf("  Relative difference: %.6f%%\n", objective_comp$relative_diff * 100))
    }
    if (length(differences) > 0) {
      cat("\nVariables with differences:\n")
      cat("  ", paste(differences, collapse = ", "), "\n")
    }
  }
  
  # Filter results based on 'keep'
  filtered_results <- switch(keep,
    all = results,
    differences = results[differences],
    matches = results[matches]
  )
  
  output <- list(
    summary = summary_text,
    variables = filtered_results,
    differences = differences,
    matches = matches,
    objective = objective_comp,
    tolerance = tol,
    paths = list(
      path1 = path1,
      path2 = path2
    )
  )
  
  class(output) <- c("variable_comparison", "list")
  return(output)
}


#' Internal: Find solution variables in a path
#'
#' Detects solution files in various locations:
#' - Direct solution directory with CSV files
#' - Model root with solvers/gmpl/solution/
#' - Model root with variables/ directory
#'
#' @param path Path to model directory or solution directory
#' @param solver Solver name to use if path is a model root (e.g., "gmpl", "jump")
#' @return Named list of file paths by variable name
#' @keywords internal
.find_solution_variables <- function(path, solver = NULL) {
  variables <- list()
  
  if (!dir.exists(path)) {
    return(variables)
  }
  
  # Check if this is a direct solution directory (contains CSV files)
  csv_files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) > 0) {
    # Filter out log files and metadata files (exact matches only)
    var_csv_files <- csv_files[!grepl("^(log|raw_data_set|variable_list|table_output|solution|solution_nonzero)\\.csv$", basename(csv_files))]
    
    for (csv_file in var_csv_files) {
      var_name <- tools::file_path_sans_ext(basename(csv_file))
      variables[[var_name]] <- csv_file
    }
    
    # If we found CSVs directly, return (this is a solution directory)
    if (length(variables) > 0) {
      return(variables)
    }
  }
  
  # Check if this path contains variable subdirectories (JuMP-style output)
  # Look for subdirectories with data files
  subdirs <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  has_var_structure <- FALSE
  
  for (subdir in subdirs) {
    subdir_path <- file.path(path, subdir)
    if (file.exists(file.path(subdir_path, "data.arrow")) ||
        file.exists(file.path(subdir_path, "data.csv")) ||
        file.exists(file.path(subdir_path, "data.feather"))) {
      has_var_structure <- TRUE
      
      # Check for data files in order of preference
      if (file.exists(file.path(subdir_path, "data.arrow"))) {
        variables[[subdir]] <- file.path(subdir_path, "data.arrow")
      } else if (file.exists(file.path(subdir_path, "data.csv"))) {
        variables[[subdir]] <- file.path(subdir_path, "data.csv")
      } else if (file.exists(file.path(subdir_path, "data.feather"))) {
        variables[[subdir]] <- file.path(subdir_path, "data.feather")
      }
    }
  }
  
  # If we found variable structure, return
  if (has_var_structure) {
    return(variables)
  }
  
  # Otherwise, treat as model root directory
  # If solver is specified, only check that solver's output
  if (!is.null(solver)) {
    solver_solution_dir <- file.path(path, "solvers", solver, "solution")
    if (dir.exists(solver_solution_dir)) {
      # Check if it's CSV-based (GMPL style)
      csv_files <- list.files(solver_solution_dir, pattern = "\\.csv$", full.names = TRUE)
      if (length(csv_files) > 0) {
        var_csv_files <- csv_files[!grepl("^(log|raw_data_set|variable_list|table_output|solution|solution_nonzero)\\.csv$", basename(csv_files))]
        
        for (csv_file in var_csv_files) {
          var_name <- tools::file_path_sans_ext(basename(csv_file))
          variables[[var_name]] <- csv_file
        }
      } else {
        # Check for variable subdirectories (JuMP style)
        var_dirs <- list.dirs(solver_solution_dir, recursive = FALSE, full.names = FALSE)
        
        for (var_name in var_dirs) {
          var_path <- file.path(solver_solution_dir, var_name)
          
          if (file.exists(file.path(var_path, "data.arrow"))) {
            variables[[var_name]] <- file.path(var_path, "data.arrow")
          } else if (file.exists(file.path(var_path, "data.csv"))) {
            variables[[var_name]] <- file.path(var_path, "data.csv")
          } else if (file.exists(file.path(var_path, "data.feather"))) {
            variables[[var_name]] <- file.path(var_path, "data.feather")
          }
        }
      }
    }
    
    return(variables)
  }
  
  # No solver specified - check all standard locations
  # Check for GMPL solutions (CSV format in solvers/gmpl/solution/)
  gmpl_solution_dir <- file.path(path, "solvers", "gmpl", "solution")
  if (dir.exists(gmpl_solution_dir)) {
    csv_files <- list.files(gmpl_solution_dir, pattern = "\\.csv$", full.names = TRUE)
    # Filter out log files and metadata files (exact matches only)
    var_csv_files <- csv_files[!grepl("^(log|raw_data_set|variable_list|table_output|solution|solution_nonzero)\\.csv$", basename(csv_files))]
    
    for (csv_file in var_csv_files) {
      var_name <- tools::file_path_sans_ext(basename(csv_file))
      variables[[var_name]] <- csv_file
    }
  }
  
  # Check for JuMP solutions (solvers/jump/solution/)
  jump_solution_dir <- file.path(path, "solvers", "jump", "solution")
  if (dir.exists(jump_solution_dir)) {
    var_dirs <- list.dirs(jump_solution_dir, recursive = FALSE, full.names = FALSE)
    
    for (var_name in var_dirs) {
      var_path <- file.path(jump_solution_dir, var_name)
      
      # Check for data files in order of preference
      if (file.exists(file.path(var_path, "data.arrow"))) {
        variables[[var_name]] <- file.path(var_path, "data.arrow")
      } else if (file.exists(file.path(var_path, "data.csv"))) {
        variables[[var_name]] <- file.path(var_path, "data.csv")
      } else if (file.exists(file.path(var_path, "data.feather"))) {
        variables[[var_name]] <- file.path(var_path, "data.feather")
      }
    }
  }
  
  # Check for variables/ directory (at model root)
  variables_dir <- file.path(path, "variables")
  if (dir.exists(variables_dir)) {
    var_dirs <- list.dirs(variables_dir, recursive = FALSE, full.names = FALSE)
    
    for (var_name in var_dirs) {
      var_path <- file.path(variables_dir, var_name)
      
      # Check for data files in order of preference
      if (file.exists(file.path(var_path, "data.arrow"))) {
        variables[[var_name]] <- file.path(var_path, "data.arrow")
      } else if (file.exists(file.path(var_path, "data.csv"))) {
        variables[[var_name]] <- file.path(var_path, "data.csv")
      } else if (file.exists(file.path(var_path, "data.feather"))) {
        variables[[var_name]] <- file.path(var_path, "data.feather")
      }
    }
  }
  
  return(variables)
}


#' Internal: Load variable data from file
#'
#' Handles CSV, Arrow, and Feather formats
#'
#' @param file_path Path to data file
#' @return Data frame or NULL
#' @keywords internal
.load_variable_data <- function(file_path) {
  if (is.null(file_path) || !file.exists(file_path)) {
    return(NULL)
  }
  
  ext <- tools::file_ext(file_path)
  
  tryCatch({
    if (ext == "csv") {
      return(read.csv(file_path, stringsAsFactors = FALSE))
    } else if (ext == "arrow") {
      if (requireNamespace("arrow", quietly = TRUE)) {
        data <- arrow::read_ipc_file(file_path)
        return(as.data.frame(data))
      } else {
        warning("Package 'arrow' required to read .arrow files")
        return(NULL)
      }
    } else if (ext == "feather") {
      if (requireNamespace("arrow", quietly = TRUE)) {
        data <- arrow::read_feather(file_path)
        return(as.data.frame(data))
      } else {
        warning("Package 'arrow' required to read .feather files")
        return(NULL)
      }
    } else {
      warning("Unknown file format: ", ext)
      return(NULL)
    }
  }, error = function(e) {
    warning("Error loading ", file_path, ": ", e$message)
    return(NULL)
  })
}


#' Compare variable solutions between two models
#'
#' @param model1 First model object (reference/expected)
#' @param model2 Second model object (to compare against model1)
#' @param variables Character vector of variable names to compare. If NULL, compares all common variables.
#' @param tol Numeric tolerance for comparing values (default 1e-5)
#' @param keep What to return: "all", "differences", "matches" (default "differences")
#' @param allowAll Passed to compare::compare() - allows all transformations (default TRUE)
#' @param verbose Print comparison details (default TRUE)
#'
#' @return A list with components:
#'   - summary: Overall comparison summary
#'   - variables: List of comparison results per variable
#'   - differences: Variables with differences (if any)
#'   - objective: Objective value comparison
#'
#' @export
compare_variables <- function(
    model1, 
    model2, 
    variables = NULL,
    tol = 1e-5,
    keep = c("differences", "all", "matches"),
    allowAll = TRUE,
    verbose = TRUE
) {
  keep <- match.arg(keep)
  
  if (!requireNamespace("compare", quietly = TRUE)) {
    stop("Package 'compare' is required. Install with: install.packages('compare')")
  }
  
  # Get variable names to compare
  vars1 <- names(model1$variables)
  vars2 <- names(model2$variables)
  
  if (is.null(variables)) {
    # Compare all common variables
    variables <- intersect(vars1, vars2)
  } else {
    # Validate requested variables exist
    missing1 <- setdiff(variables, vars1)
    missing2 <- setdiff(variables, vars2)
    if (length(missing1) > 0) {
      warning("Variables not found in model1: ", paste(missing1, collapse = ", "))
    }
    if (length(missing2) > 0) {
      warning("Variables not found in model2: ", paste(missing2, collapse = ", "))
    }
    variables <- intersect(variables, intersect(vars1, vars2))
  }
  
  if (length(variables) == 0) {
    stop("No common variables to compare")
  }
  
  if (verbose) {
    cat("Comparing", length(variables), "variables between models\n")
    cat("Tolerance:", tol, "\n\n")
  }
  
  # Storage for results
  results <- list()
  differences <- character(0)
  matches <- character(0)
  
  # Compare each variable
  for (var_name in variables) {
    if (verbose) cat("Comparing", var_name, "... ")
    
    # Get data from model1 (use get_data with lazy loading if available)
    data1 <- NULL
    if (!is.null(model1$variables[[var_name]]$solution)) {
      data1 <- model1$variables[[var_name]]$solution
    } else if (!is.null(model1$storage$path) || !is.null(model1$base_path)) {
      # Try lazy loading
      tryCatch({
        data1 <- get_data(model1, var_name, type = "variable")
      }, error = function(e) NULL)
    }
    
    # Get data from model2
    data2 <- NULL
    if (!is.null(model2$variables[[var_name]]$solution)) {
      data2 <- model2$variables[[var_name]]$solution
    } else if (!is.null(model2$storage$path) || !is.null(model2$base_path)) {
      # Try lazy loading
      tryCatch({
        data2 <- get_data(model2, var_name, type = "variable")
      }, error = function(e) NULL)
    }
    
    # Handle missing data
    if (is.null(data1) && is.null(data2)) {
      if (verbose) cat("no data in either model\n")
      results[[var_name]] <- list(
        status = "no_data",
        message = "No solution data in either model"
      )
      next
    }
    
    if (is.null(data1)) {
      if (verbose) cat("missing in model1\n")
      results[[var_name]] <- list(
        status = "missing_model1",
        message = "No solution data in model1",
        rows_model2 = nrow(data2)
      )
      differences <- c(differences, var_name)
      next
    }
    
    if (is.null(data2)) {
      if (verbose) cat("missing in model2\n")
      results[[var_name]] <- list(
        status = "missing_model2",
        message = "No solution data in model2",
        rows_model1 = nrow(data1)
      )
      differences <- c(differences, var_name)
      next
    }
    
    # Compare data frames using compare package
    comp <- compare::compare(data1, data2, allowAll = allowAll)
    
    # Check if identical (within tolerance for numeric columns)
    is_identical <- comp$result
    
    # For numeric differences, apply tolerance
    if (!is_identical && any(sapply(data1, is.numeric))) {
      # Manual tolerance check for numeric columns
      numeric_cols <- names(data1)[sapply(data1, is.numeric)]
      
      # Check dimensions first
      if (nrow(data1) == nrow(data2) && ncol(data1) == ncol(data2)) {
        # Check non-numeric columns are identical
        non_numeric_cols <- setdiff(names(data1), numeric_cols)
        non_numeric_match <- TRUE
        if (length(non_numeric_cols) > 0) {
          for (col in non_numeric_cols) {
            if (!identical(data1[[col]], data2[[col]])) {
              non_numeric_match <- FALSE
              break
            }
          }
        }
        
        # Check numeric columns within tolerance
        numeric_match <- TRUE
        max_diff <- 0
        for (col in numeric_cols) {
          diff <- abs(data1[[col]] - data2[[col]])
          max_col_diff <- max(diff, na.rm = TRUE)
          if (max_col_diff > max_diff) max_diff <- max_col_diff
          if (max_col_diff > tol) {
            numeric_match <- FALSE
          }
        }
        
        if (non_numeric_match && numeric_match) {
          is_identical <- TRUE
          if (verbose) cat("match (within tolerance, max diff:", signif(max_diff, 3), ")\n")
          results[[var_name]] <- list(
            status = "match",
            message = "Identical within tolerance",
            max_diff = max_diff,
            rows = nrow(data1)
          )
          matches <- c(matches, var_name)
          next
        }
      }
    }
    
    if (is_identical) {
      if (verbose) cat("exact match\n")
      results[[var_name]] <- list(
        status = "exact_match",
        message = "Exactly identical",
        rows = nrow(data1)
      )
      matches <- c(matches, var_name)
    } else {
      if (verbose) cat("DIFFERENT\n")
      results[[var_name]] <- list(
        status = "different",
        message = "Data differs between models",
        comparison = comp,
        rows_model1 = nrow(data1),
        rows_model2 = nrow(data2)
      )
      differences <- c(differences, var_name)
    }
  }
  
  # Extract objective values
  obj1 <- NULL
  obj2 <- NULL
  if ("vObjective" %in% names(model1$variables)) {
    obj_data1 <- model1$variables$vObjective$solution
    if (!is.null(obj_data1) && "value" %in% names(obj_data1) && nrow(obj_data1) > 0) {
      obj1 <- obj_data1$value[1]
    }
  }
  if ("vObjective" %in% names(model2$variables)) {
    obj_data2 <- model2$variables$vObjective$solution
    if (!is.null(obj_data2) && "value" %in% names(obj_data2) && nrow(obj_data2) > 0) {
      obj2 <- obj_data2$value[1]
    }
  }
  
  objective_comp <- list(
    model1 = obj1,
    model2 = obj2,
    difference = if (!is.null(obj1) && !is.null(obj2)) abs(obj1 - obj2) else NA,
    relative_diff = if (!is.null(obj1) && !is.null(obj2) && obj1 != 0) {
      abs(obj1 - obj2) / abs(obj1)
    } else NA
  )
  
  # Summary
  summary_text <- sprintf(
    "Variables compared: %d\n  Matches: %d\n  Differences: %d\n  Missing/No data: %d",
    length(variables),
    length(matches),
    length(differences),
    length(variables) - length(matches) - length(differences)
  )
  
  if (verbose) {
    cat("\n=== Comparison Summary ===\n")
    cat(summary_text, "\n")
    if (!is.null(obj1) && !is.null(obj2)) {
      cat("\nObjective Values:\n")
      cat(sprintf("  Model 1: %.10e\n", obj1))
      cat(sprintf("  Model 2: %.10e\n", obj2))
      cat(sprintf("  Absolute difference: %.10e\n", objective_comp$difference))
      cat(sprintf("  Relative difference: %.6f%%\n", objective_comp$relative_diff * 100))
    }
    if (length(differences) > 0) {
      cat("\nVariables with differences:\n")
      cat("  ", paste(differences, collapse = ", "), "\n")
    }
  }
  
  # Filter results based on 'keep'
  filtered_results <- switch(keep,
    all = results,
    differences = results[differences],
    matches = results[matches]
  )
  
  output <- list(
    summary = summary_text,
    variables = filtered_results,
    differences = differences,
    matches = matches,
    objective = objective_comp,
    tolerance = tol
  )
  
  class(output) <- c("variable_comparison", "list")
  return(output)
}


#' Print method for variable comparison results
#'
#' @param x A variable_comparison object
#' @param ... Additional arguments (unused)
#'
#' @export
print.variable_comparison <- function(x, ...) {
  cat("=== Variable Comparison Results ===\n\n")
  cat(x$summary, "\n\n")
  
  if (!is.null(x$objective$model1) && !is.null(x$objective$model2)) {
    cat("Objective Values:\n")
    cat(sprintf("  Model 1: %.10e\n", x$objective$model1))
    cat(sprintf("  Model 2: %.10e\n", x$objective$model2))
    cat(sprintf("  Difference: %.10e (%.6f%%)\n", 
                x$objective$difference, 
                x$objective$relative_diff * 100))
    cat("\n")
  }
  
  if (length(x$differences) > 0) {
    cat("Variables with differences (", length(x$differences), "):\n", sep = "")
    cat("  ", paste(x$differences, collapse = ", "), "\n")
  }
  
  invisible(x)
}
