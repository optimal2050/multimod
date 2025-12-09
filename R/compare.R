#' Compare two AST objects and report differences
#' 
#' @description
#' Recursively compare two AST objects to identify structural and value differences.
#' Useful for debugging changes in model structure or equation transformations.
#' 
#' @param ast1 First AST object to compare
#' @param ast2 Second AST object to compare
#' @param path Character vector tracking the path to current node (for reporting)
#' @param max_diffs Maximum number of differences to report (default 50)
#' 
#' @return A list with:
#'   \item{identical}{Logical, TRUE if ASTs are identical}
#'   \item{differences}{data.frame with columns: path, field, value1, value2}
#'   \item{summary}{Character string summarizing key differences}
#'   
#' @examples
#' \dontrun{
#' # Compare two equation ASTs
#' eq1 <- model_old$equations$eqTechCap
#' eq2 <- model_new$equations$eqTechCap
#' result <- compare_ast(eq1, eq2)
#' print(result)
#' 
#' # Compare specific parts
#' result <- compare_ast(eq1$rhs, eq2$rhs, path = "eqTechCap$rhs")
#' }
#' 
#' @export
compare_ast <- function(ast1, ast2, path = "root", max_diffs = 50) {
  
  differences <- list()
  
  # Helper to add a difference
  add_diff <- function(path, field, val1, val2) {
    if (length(differences) < max_diffs) {
      differences[[length(differences) + 1]] <<- list(
        path = path,
        field = field,
        value1 = if (is.null(val1)) "NULL" else toString(val1),
        value2 = if (is.null(val2)) "NULL" else toString(val2)
      )
    }
  }
  
  # Recursive comparison function
  compare_nodes <- function(node1, node2, current_path) {
    if (length(differences) >= max_diffs) return()
    
    # Check if both are NULL
    if (is.null(node1) && is.null(node2)) return()
    
    # Check if one is NULL
    if (is.null(node1) || is.null(node2)) {
      add_diff(current_path, "existence", 
               if (is.null(node1)) "NULL" else class(node1)[1],
               if (is.null(node2)) "NULL" else class(node2)[1])
      return()
    }
    
    # Check class differences
    class1 <- class(node1)
    class2 <- class(node2)
    if (!identical(class1, class2)) {
      add_diff(current_path, "class", 
               paste(class1, collapse = ", "),
               paste(class2, collapse = ", "))
      return()  # Don't recurse if classes differ
    }
    
    # For atomic values, compare directly
    if (is.atomic(node1) && length(node1) == 1) {
      if (!identical(node1, node2)) {
        add_diff(current_path, "value", node1, node2)
      }
      return()
    }
    
    # For lists/objects, compare fields
    if (is.list(node1)) {
      names1 <- names(node1)
      names2 <- names(node2)
      
      # Check for missing/extra fields
      missing_in_2 <- setdiff(names1, names2)
      missing_in_1 <- setdiff(names2, names1)
      
      if (length(missing_in_2) > 0) {
        add_diff(current_path, "missing_fields_in_ast2", 
                 paste(missing_in_2, collapse = ", "), "")
      }
      if (length(missing_in_1) > 0) {
        add_diff(current_path, "extra_fields_in_ast2", 
                 "", paste(missing_in_1, collapse = ", "))
      }
      
      # Compare common fields
      common_names <- intersect(names1, names2)
      for (field_name in common_names) {
        new_path <- paste0(current_path, "$", field_name)
        compare_nodes(node1[[field_name]], node2[[field_name]], new_path)
      }
    }
  }
  
  # Start comparison
  compare_nodes(ast1, ast2, path)
  
  # Convert to data frame
  if (length(differences) > 0) {
    diff_df <- do.call(rbind, lapply(differences, function(d) {
      data.frame(
        path = d$path,
        field = d$field,
        value1 = d$value1,
        value2 = d$value2,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    diff_df <- data.frame(
      path = character(0),
      field = character(0),
      value1 = character(0),
      value2 = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Generate summary
  identical_flag <- nrow(diff_df) == 0
  summary_text <- if (identical_flag) {
    "ASTs are identical"
  } else {
    unique_paths <- unique(diff_df$path)
    paste0(
      "Found ", nrow(diff_df), " difference(s) across ", 
      length(unique_paths), " node(s)",
      if (length(differences) >= max_diffs) " (truncated)" else ""
    )
  }
  
  structure(
    list(
      identical = identical_flag,
      differences = diff_df,
      summary = summary_text
    ),
    class = "ast_comparison"
  )
}

#' Print comparison results in a readable format
#' 
#' @param x Result from compare_ast()
#' @param ... Additional arguments (not used)
#' 
#' @export
print.ast_comparison <- function(x, ...) {
  cat(x$summary, "\n\n")
  
  if (!x$identical) {
    cat("Differences:\n")
    for (i in seq_len(nrow(x$differences))) {
      row <- x$differences[i, ]
      cat(sprintf("  [%d] %s @ %s\n", i, row$field, row$path))
      cat(sprintf("      AST1: %s\n", row$value1))
      cat(sprintf("      AST2: %s\n", row$value2))
      cat("\n")
    }
  }
  
  invisible(x)
}

#' Compare AST objects using compare package with custom handling
#' 
#' @description
#' Alternative comparison using the compare package. Simpler but may be 
#' more verbose for complex AST structures.
#' 
#' @param ast1 First AST object
#' @param ast2 Second AST object
#' 
#' @return Result from compare::compare()
#' 
#' @export
compare_ast_simple <- function(ast1, ast2) {
  if (!requireNamespace("compare", quietly = TRUE)) {
    stop("Package 'compare' is required. Install with: install.packages('compare')")
  }
  
  result <- compare::compare(ast1, ast2, 
                             allowAll = TRUE,  # Allow different classes to be compared
                             ignoreEnvironments = TRUE)
  
  # Extract meaningful differences
  if (!result$result) {
    cat("ASTs differ:\n")
    print(result$tM)  # Transformation matrix showing differences
    cat("\nDetailed comparison:\n")
    print(result)
  } else {
    cat("ASTs are identical\n")
  }
  
}

#' Compare two LP files and report differences
#'
#' Reads two LP files using glpkAPI, extracts all constraint and variable
#' information into data frames, and performs a deep comparison using the compare
#' package. Constraints and variables are sorted by name before comparison,
#' making the comparison order-independent.
#'
#' @param file1 Path to first LP file (reference/expected)
#' @param file2 Path to second LP file (to compare)
#' @param ignoreOrder Ignore row order in comparison (default TRUE, passed to compare::compare)
#' @param ignoreNameCase Ignore case differences in names (default FALSE, passed to compare::compare)
#' @param verbose Logical; if TRUE, print detailed progress (default: FALSE)
#' @param compareMatrix Logical; if TRUE, compare every non-zero coefficient in
#'   the constraint matrix (default FALSE because it can be expensive)
#' @param matrixTolerance Absolute tolerance used when flagging coefficient
#'   differences (default 1e-9)
#' @param matrixVerbose Logical; emit progress while extracting matrices
#' @param ... Additional arguments passed to compare::compare()
#'
#' @return A list with:
#'   \item{identical}{Logical, TRUE if models are identical}
#'   \item{dimensions}{List with nrows, ncols for each model}
#'   \item{constraints}{List with comparison results for constraints}
#'   \item{variables}{List with comparison results for variables}
#'   \item{constraints1}{Constraint data frame for file1 when returnData = TRUE}
#'   \item{constraints2}{Constraint data frame for file2 when returnData = TRUE}
#'   \item{variables1}{Variable data frame for file1 when returnData = TRUE}
#'   \item{variables2}{Variable data frame for file2 when returnData = TRUE}
#'   \item{objective}{Comparison of objective direction}
#'   \item{summary}{Text summary of differences}
#'
#' @export
#'
#' @seealso \code{\link{compare_mps}}
#'
#' @examples
#' \dontrun{
#' result <- compare_lp("original.lp", "reproduced.lp")
#' if (!result$identical) {
#'   cat(result$summary, "\n")
#'   # Examine constraint differences
#'   print(result$constraints$comparison)
#' }
#' }
compare_lp <- function(file1, file2, ignoreOrder = TRUE, ignoreNameCase = FALSE, 
                      verbose = FALSE, ...) {
  
  if (!requireNamespace("glpkAPI", quietly = TRUE)) {
    stop("Package 'glpkAPI' is required. Install with: install.packages('glpkAPI')")
  }
  
  if (!requireNamespace("compare", quietly = TRUE)) {
    stop("Package 'compare' is required. Install with: install.packages('compare')")
  }
  
  if (verbose) {
    cat("=== Comparing LP Files ===\n")
    cat("File 1:", file1, "\n")
    cat("File 2:", file2, "\n\n")
  }
  
  # Read LP files
  if (verbose) cat("Reading files...\n")
  
  lp1 <- glpkAPI::initProbGLPK()
  ret1 <- glpkAPI::readLPGLPK(lp1, file1)
  if (ret1 != 0) {
    glpkAPI::delProbGLPK(lp1)
    stop("Failed to read file1: ", file1)
  }
  
  lp2 <- glpkAPI::initProbGLPK()
  ret2 <- glpkAPI::readLPGLPK(lp2, file2)
  if (ret2 != 0) {
    glpkAPI::delProbGLPK(lp1)
    glpkAPI::delProbGLPK(lp2)
    stop("Failed to read file2: ", file2)
  }
  
  # Get dimensions
  nrows1 <- glpkAPI::getNumRowsGLPK(lp1)
  ncols1 <- glpkAPI::getNumColsGLPK(lp1)
  nrows2 <- glpkAPI::getNumRowsGLPK(lp2)
  ncols2 <- glpkAPI::getNumColsGLPK(lp2)
  
  if (verbose) {
    cat("Model 1:", nrows1, "constraints,", ncols1, "variables\n")
    cat("Model 2:", nrows2, "constraints,", ncols2, "variables\n\n")
  }
  
  # Extract constraint information as data frames
  if (verbose) cat("Extracting", nrows1, "constraints from model 1...\n")
  constraints1 <- .extract_constraints_df(lp1, verbose = verbose)
  if (verbose) cat("Extracting", nrows2, "constraints from model 2...\n")
  constraints2 <- .extract_constraints_df(lp2, verbose = verbose)
  
  # Sort constraints by name for order-independent comparison
  if (verbose) cat("Sorting constraints by name...\n")
  constraints1 <- constraints1[order(constraints1$name), ]
  constraints2 <- constraints2[order(constraints2$name), ]
  rownames(constraints1) <- NULL
  rownames(constraints2) <- NULL
  constraints1$index <- NULL  # Drop index column
  constraints2$index <- NULL
  
  # Extract variable information as data frames
  if (verbose) cat("Extracting", ncols1, "variables from model 1...\n")
  variables1 <- .extract_variables_df(lp1, verbose = verbose)
  if (verbose) cat("Extracting", ncols2, "variables from model 2...\n")
  variables2 <- .extract_variables_df(lp2, verbose = verbose)
  
  # Sort variables by name for order-independent comparison
  if (verbose) cat("Sorting variables by name...\n")
  variables1 <- variables1[order(variables1$name), ]
  variables2 <- variables2[order(variables2$name), ]
  rownames(variables1) <- NULL
  rownames(variables2) <- NULL
  variables1$index <- NULL  # Drop index column
  variables2$index <- NULL
  
  # Extract objective direction
  if (verbose) cat("Extracting objective...\n")
  obj_dir1 <- glpkAPI::getObjDirGLPK(lp1)
  obj_dir2 <- glpkAPI::getObjDirGLPK(lp2)
  
  # Clean up
  glpkAPI::delProbGLPK(lp1)
  glpkAPI::delProbGLPK(lp2)
  
  # Compare constraints
  if (verbose) cat("\nComparing constraints...\n")
  constr_comp <- compare::compare(constraints1, constraints2, 
                                  ignoreOrder = ignoreOrder, 
                                  ignoreNameCase = ignoreNameCase,
                                  ...)
  
  # Compare variables
  if (verbose) cat("Comparing variables...\n")
  var_comp <- compare::compare(variables1, variables2, 
                               ignoreOrder = ignoreOrder, 
                               ignoreNameCase = ignoreNameCase,
                               ...)

  strip_dim_names <- function(x) {
    if (length(x) == 0) {
      return(character(0))
    }
    unique(sub("\\[.*$", "", x))
  }
  vars_only_in_1 <- setdiff(variables1$name, variables2$name)
  vars_only_in_2 <- setdiff(variables2$name, variables1$name)
  base_vars_only_in_1 <- strip_dim_names(vars_only_in_1)
  base_vars_only_in_2 <- strip_dim_names(vars_only_in_2)

  strip_dim_names <- function(x) {
    if (length(x) == 0) {
      return(character(0))
    }
    unique(sub("\\[.*$", "", x))
  }
  vars_only_in_1 <- setdiff(variables1$name, variables2$name)
  vars_only_in_2 <- setdiff(variables2$name, variables1$name)
  base_vars_only_in_1 <- strip_dim_names(vars_only_in_1)
  base_vars_only_in_2 <- strip_dim_names(vars_only_in_2)

  strip_dim_names <- function(x) {
    if (length(x) == 0) {
      return(character(0))
    }
    unique(sub("\\[.*$", "", x))
  }
  vars_only_in_1 <- setdiff(variables1$name, variables2$name)
  vars_only_in_2 <- setdiff(variables2$name, variables1$name)
  base_vars_only_in_1 <- strip_dim_names(vars_only_in_1)
  base_vars_only_in_2 <- strip_dim_names(vars_only_in_2)
  
  # Compare objective direction
  obj_identical <- obj_dir1 == obj_dir2
  
  # Determine overall result
  identical <- constr_comp$result && var_comp$result && obj_identical
  
  # Build summary
  summary_lines <- character(0)
  
  if (identical) {
    summary_lines <- c(summary_lines, "Models are IDENTICAL")
  } else {
    summary_lines <- c(summary_lines, "Models have DIFFERENCES:")
    
    if (!obj_identical) {
      obj_name1 <- if (obj_dir1 == glpkAPI::GLP_MIN) "minimize" else "maximize"
      obj_name2 <- if (obj_dir2 == glpkAPI::GLP_MIN) "minimize" else "maximize"
      summary_lines <- c(summary_lines, paste("  - Objective direction:", obj_name1, "vs", obj_name2))
    }
    
    if (!constr_comp$result) {
      summary_lines <- c(summary_lines, "  - Constraints differ")
      if (!is.null(constr_comp$message)) {
        summary_lines <- c(summary_lines, paste("    ", constr_comp$message))
      }
    }
    
    if (!var_comp$result) {
      summary_lines <- c(summary_lines, "  - Variables differ")
      if (length(base_vars_only_in_1) > 0) {
        summary_lines <- c(summary_lines, paste0("    Vars only in file1: ", length(base_vars_only_in_1)))
      }
      if (length(base_vars_only_in_2) > 0) {
        summary_lines <- c(summary_lines, paste0("    Vars only in file2: ", length(base_vars_only_in_2)))
      }
      if (!is.null(var_comp$message)) {
        summary_lines <- c(summary_lines, paste("    ", var_comp$message))
      }
    }
  }
  
  if (verbose) {
    cat("\n")
    cat(paste(summary_lines, collapse = "\n"), "\n")
  }
  
  return(list(
    identical = identical,
    dimensions = list(
      model1 = list(nrows = nrows1, ncols = ncols1),
      model2 = list(nrows = nrows2, ncols = ncols2)
    ),
    constraints = list(
      identical = constr_comp$result,
      comparison = constr_comp
    ),
    variables = list(
      identical = var_comp$result,
      comparison = var_comp
    ),
    vars_only_in_1 = base_vars_only_in_1,
    vars_only_in_2 = base_vars_only_in_2,
    objective = list(
      identical = obj_identical,
      dir1 = obj_dir1,
      dir2 = obj_dir2
    ),
    summary = paste(summary_lines, collapse = "\n")
  ))
}

#' Compare two MPS files and report differences
#'
#' Reads two MPS files using glpkAPI, extracts all constraint and variable
#' information into data frames, and performs a deep comparison using the compare
#' package. Constraints and variables are sorted by name before comparison,
#' making the comparison order-independent.
#'
#' @param file1 Path to first MPS file (reference/expected)
#' @param file2 Path to second MPS file (to compare)
#' @param ignoreOrder Ignore row order in comparison (default TRUE, passed to compare::compare)
#' @param ignoreNameCase Ignore case differences in names (default FALSE, passed to compare::compare)
#' @param verbose Logical; if TRUE, print detailed progress (default: FALSE)
#' @param returnData Logical; if TRUE, include the extracted constraint and
#'   variable data frames in the returned list (default: FALSE)
#' @param ... Additional arguments passed to compare::compare()
#'
#' @return A list with:
#'   \item{identical}{Logical, TRUE if models are identical}
#'   \item{dimensions}{List with nrows, ncols for each model}
#'   \item{constraints}{List with comparison results for constraints}
#'   \item{variables}{List with comparison results for variables}
#'   \item{objective}{Comparison of objective direction}
#'   \item{summary}{Text summary of differences}
#'
#' @export
#'
#' @seealso \code{\link{compare_lp}}
#'
#' @examples
#' \dontrun{
#' result <- compare_mps("original.mps", "reproduced.mps")
#' if (!result$identical) {
#'   cat(result$summary, "\n")
#'   # Examine constraint differences
#'   print(result$constraints$comparison)
#' }
#' }
compare_mps <- function(file1, file2, ignoreOrder = TRUE, ignoreNameCase = FALSE, 
                       verbose = FALSE, returnData = FALSE, compareMatrix = FALSE,
                       matrixTolerance = 1e-9, matrixVerbose = FALSE, ...) {
  
  if (!requireNamespace("glpkAPI", quietly = TRUE)) {
    stop("Package 'glpkAPI' is required. Install with: install.packages('glpkAPI')")
  }
  
  if (!requireNamespace("compare", quietly = TRUE)) {
    stop("Package 'compare' is required. Install with: install.packages('compare')")
  }
  
  if (verbose) {
    cat("=== Comparing MPS Files ===\n")
    cat("File 1:", file1, "\n")
    cat("File 2:", file2, "\n\n")
  }
  
  # Read MPS files
  if (verbose) cat("Reading files...\n")
  
  lp1 <- glpkAPI::initProbGLPK()
  ret1 <- glpkAPI::readMPSGLPK(lp1, glpkAPI::GLP_MPS_FILE, file1)
  if (ret1 != 0) {
    glpkAPI::delProbGLPK(lp1)
    stop("Failed to read file1: ", file1)
  }
  
  lp2 <- glpkAPI::initProbGLPK()
  ret2 <- glpkAPI::readMPSGLPK(lp2, glpkAPI::GLP_MPS_FILE, file2)
  if (ret2 != 0) {
    glpkAPI::delProbGLPK(lp1)
    glpkAPI::delProbGLPK(lp2)
    stop("Failed to read file2: ", file2)
  }
  
  # Get dimensions
  nrows1 <- glpkAPI::getNumRowsGLPK(lp1)
  ncols1 <- glpkAPI::getNumColsGLPK(lp1)
  nrows2 <- glpkAPI::getNumRowsGLPK(lp2)
  ncols2 <- glpkAPI::getNumColsGLPK(lp2)
  
  if (verbose) {
    cat("Model 1:", nrows1, "constraints,", ncols1, "variables\n")
    cat("Model 2:", nrows2, "constraints,", ncols2, "variables\n\n")
  }
  
  # Extract constraint information as data frames
  if (verbose) cat("Extracting", nrows1, "constraints from model 1...\n")
  constraints1 <- .extract_constraints_df(lp1, verbose = verbose)
  if (verbose) cat("Extracting", nrows2, "constraints from model 2...\n")
  constraints2 <- .extract_constraints_df(lp2, verbose = verbose)
  
  # Sort constraints by name for order-independent comparison
  if (verbose) cat("Sorting constraints by name...\n")
  constraints1 <- constraints1[order(constraints1$name), ]
  constraints2 <- constraints2[order(constraints2$name), ]
  rownames(constraints1) <- NULL
  rownames(constraints2) <- NULL
  constraints1$index <- NULL  # Drop index column
  constraints2$index <- NULL
  
  # Extract variable information as data frames
  if (verbose) cat("Extracting", ncols1, "variables from model 1...\n")
  variables1 <- .extract_variables_df(lp1, verbose = verbose)
  if (verbose) cat("Extracting", ncols2, "variables from model 2...\n")
  variables2 <- .extract_variables_df(lp2, verbose = verbose)
  
  # Sort variables by name for order-independent comparison
  if (verbose) cat("Sorting variables by name...\n")
  variables1 <- variables1[order(variables1$name), ]
  variables2 <- variables2[order(variables2$name), ]
  rownames(variables1) <- NULL
  rownames(variables2) <- NULL
  variables1$index <- NULL  # Drop index column
  variables2$index <- NULL
  
  # Extract objective direction
  if (verbose) cat("Extracting objective...\n")
  obj_dir1 <- glpkAPI::getObjDirGLPK(lp1)
  obj_dir2 <- glpkAPI::getObjDirGLPK(lp2)
  
  matrix_result <- NULL
  matrix_identical <- TRUE
  if (compareMatrix) {
    if (verbose) {
      cat("Extracting constraint matrices (this may take a while)...\n")
    }
    mat1 <- .extract_matrix_long(lp1, verbose = matrixVerbose)
    mat2 <- .extract_matrix_long(lp2, verbose = matrixVerbose)
    matrix_result <- .compare_matrix_entries(mat1, mat2, matrixTolerance)
    matrix_identical <- matrix_result$identical
  }

  # Clean up
  glpkAPI::delProbGLPK(lp1)
  glpkAPI::delProbGLPK(lp2)
  
  # Compare constraints
  if (verbose) cat("\nComparing constraints...\n")
  constr_comp <- compare::compare(constraints1, constraints2, 
                                  ignoreOrder = ignoreOrder, 
                                  ignoreNameCase = ignoreNameCase,
                                  ...)
  
  # Compare variables
  if (verbose) cat("Comparing variables...\n")
  var_comp <- compare::compare(variables1, variables2, 
                               ignoreOrder = ignoreOrder, 
                               ignoreNameCase = ignoreNameCase,
                               ...)

  strip_dim_names <- function(x) {
    if (length(x) == 0) {
      return(character(0))
    }
    unique(sub("\\[.*$", "", x))
  }
  vars_only_in_1 <- setdiff(variables1$name, variables2$name)
  vars_only_in_2 <- setdiff(variables2$name, variables1$name)
  base_vars_only_in_1 <- strip_dim_names(vars_only_in_1)
  base_vars_only_in_2 <- strip_dim_names(vars_only_in_2)
  
  # Compare objective direction
  obj_identical <- obj_dir1 == obj_dir2
  
  # Determine overall result
  identical <- constr_comp$result && var_comp$result && obj_identical && matrix_identical
  
  # Build summary
  summary_lines <- character(0)
  
  if (identical) {
    summary_lines <- c(summary_lines, "Models are IDENTICAL")
  } else {
    summary_lines <- c(summary_lines, "Models have DIFFERENCES:")
    
    if (!obj_identical) {
      obj_name1 <- if (obj_dir1 == glpkAPI::GLP_MIN) "minimize" else "maximize"
      obj_name2 <- if (obj_dir2 == glpkAPI::GLP_MIN) "minimize" else "maximize"
      summary_lines <- c(summary_lines, paste("  - Objective direction:", obj_name1, "vs", obj_name2))
    }
    
    if (!constr_comp$result) {
      summary_lines <- c(summary_lines, "  - Constraints differ")
      if (!is.null(constr_comp$message)) {
        summary_lines <- c(summary_lines, paste("    ", constr_comp$message))
      }
    }
    
    if (!var_comp$result) {
      summary_lines <- c(summary_lines, "  - Variables differ")
      if (length(base_vars_only_in_1) > 0) {
        summary_lines <- c(summary_lines, paste0("    Vars only in file1: ", length(base_vars_only_in_1)))
      }
      if (length(base_vars_only_in_2) > 0) {
        summary_lines <- c(summary_lines, paste0("    Vars only in file2: ", length(base_vars_only_in_2)))
      }
      if (!is.null(var_comp$message)) {
        summary_lines <- c(summary_lines, paste("    ", var_comp$message))
      }
    }

    if (compareMatrix && !matrix_identical) {
      summary_lines <- c(
        summary_lines,
        sprintf(
          "  - Constraint matrix differs: %d entries exceed %.2e (max |delta| %.3e)",
          matrix_result$count,
          matrixTolerance,
          matrix_result$max_abs_delta
        )
      )
    }
  }
  
  if (verbose) {
    cat("\n")
    cat(paste(summary_lines, collapse = "\n"), "\n")
  }
  
  constraint_data1 <- if (returnData) constraints1 else NULL
  constraint_data2 <- if (returnData) constraints2 else NULL
  variable_data1 <- if (returnData) variables1 else NULL
  variable_data2 <- if (returnData) variables2 else NULL

  return(list(
    identical = identical,
    dimensions = list(
      model1 = list(nrows = nrows1, ncols = ncols1),
      model2 = list(nrows = nrows2, ncols = ncols2)
    ),
    constraints = list(
      identical = constr_comp$result,
      comparison = constr_comp
    ),
    variables = list(
      identical = var_comp$result,
      comparison = var_comp
    ),
    constraints1 = constraint_data1,
    constraints2 = constraint_data2,
    variables1 = variable_data1,
    variables2 = variable_data2,
    vars_only_in_1 = base_vars_only_in_1,
    vars_only_in_2 = base_vars_only_in_2,
    objective = list(
      identical = obj_identical,
      dir1 = obj_dir1,
      dir2 = obj_dir2
    ),
    matrix = matrix_result,
    summary = paste(summary_lines, collapse = "\n")
  ))
}

# Internal helper functions

# Internal function to extract constraint information as data frame
.extract_constraints_df <- function(lp, verbose = FALSE) {
  nrows <- glpkAPI::getNumRowsGLPK(lp)
  
  if (nrows == 0) {
    return(data.frame(
      index = integer(0),
      name = character(0),
      type = integer(0),
      lb = numeric(0),
      ub = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract all row information with progress
  names <- character(nrows)
  types <- integer(nrows)
  lbs <- numeric(nrows)
  ubs <- numeric(nrows)
  
  progress_interval <- max(1, floor(nrows / 10))
  
  for (i in seq_len(nrows)) {
    if (verbose && i %% progress_interval == 0) {
      cat("  ", round(100 * i / nrows), "%\r", sep = "")
    }
    names[i] <- glpkAPI::getRowNameGLPK(lp, i)
    types[i] <- glpkAPI::getRowTypeGLPK(lp, i)
    lbs[i] <- glpkAPI::getRowLowBndGLPK(lp, i)
    ubs[i] <- glpkAPI::getRowUppBndGLPK(lp, i)
  }
  
  if (verbose) cat("  100%\n")
  
  data.frame(
    index = seq_len(nrows),
    name = names,
    type = types,
    lb = lbs,
    ub = ubs,
    stringsAsFactors = FALSE
  )
}

# Internal function to extract variable information as data frame
.extract_variables_df <- function(lp, verbose = FALSE) {
  ncols <- glpkAPI::getNumColsGLPK(lp)
  
  if (ncols == 0) {
    return(data.frame(
      index = integer(0),
      name = character(0),
      type = integer(0),
      lb = numeric(0),
      ub = numeric(0),
      obj_coef = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract all column information with progress
  names <- character(ncols)
  types <- integer(ncols)
  lbs <- numeric(ncols)
  ubs <- numeric(ncols)
  obj_coefs <- numeric(ncols)
  
  progress_interval <- max(1, floor(ncols / 10))
  
  for (i in seq_len(ncols)) {
    if (verbose && i %% progress_interval == 0) {
      cat("  ", round(100 * i / ncols), "%\r", sep = "")
    }
    names[i] <- glpkAPI::getColNameGLPK(lp, i)
    types[i] <- glpkAPI::getColKindGLPK(lp, i)
    lbs[i] <- glpkAPI::getColLowBndGLPK(lp, i)
    ubs[i] <- glpkAPI::getColUppBndGLPK(lp, i)
    obj_coefs[i] <- glpkAPI::getObjCoefGLPK(lp, i)
  }
  
  if (verbose) cat("  100%\n")
  
  data.frame(
    index = seq_len(ncols),
    name = names,
    type = types,
    lb = lbs,
    ub = ubs,
    obj_coef = obj_coefs,
    stringsAsFactors = FALSE
  )
}

#' Extract the sparse constraint matrix of an MPS file
#'
#' Reads an MPS file via glpkAPI and returns the constraint matrix in
#' long/tidy form so it can be grouped by constraint or variable. Optional
#' regular expressions allow filtering rows/columns before extraction and the
#' resulting data can be split per constraint or per variable.
#'
#' @param file Path to the MPS file
#' @param rows Optional character vector of regular expressions used to select
#'   constraint (row) names. Default NULL keeps every constraint.
#' @param cols Optional character vector of regular expressions used to select
#'   variable (column) names. Default NULL keeps every variable.
#' @param partition One of "none", "row", or "column" specifying whether the
#'   returned object should be a single data frame (none) or a list split by
#'   constraint/variable.
#' @param verbose Logical; print progress while extracting (default FALSE)
#'
#' @return Either a data frame with columns (row, col, coef) or a named list of
#'   such data frames if partitioning is requested.
#' @export
mps_matrix_long <- function(file, rows = NULL, cols = NULL,
                            partition = c("none", "row", "column"),
                            verbose = FALSE) {
  partition <- match.arg(partition)

  if (!requireNamespace("glpkAPI", quietly = TRUE)) {
    stop("Package 'glpkAPI' is required. Install with: install.packages('glpkAPI')")
  }

  lp <- glpkAPI::initProbGLPK()
  on.exit(glpkAPI::delProbGLPK(lp))

  ret <- glpkAPI::readMPSGLPK(lp, glpkAPI::GLP_MPS_FILE, file)
  if (ret != 0) {
    stop("Failed to read MPS file: ", file)
  }

  mat <- .extract_matrix_long(lp, row_filter = rows, col_filter = cols, verbose = verbose)

  if (partition == "none") {
    return(mat)
  }

  if (!nrow(mat)) {
    return(list())
  }

  if (partition == "row") {
    return(split(mat, f = mat$row))
  }

  split(mat, f = mat$col)
}

.extract_matrix_long <- function(lp, row_filter = NULL, col_filter = NULL,
                                 verbose = FALSE) {
  nrows <- glpkAPI::getNumRowsGLPK(lp)
  ncols <- glpkAPI::getNumColsGLPK(lp)
  row_names <- vapply(seq_len(nrows), function(i) glpkAPI::getRowNameGLPK(lp, i), character(1))
  col_names <- vapply(seq_len(ncols), function(i) glpkAPI::getColNameGLPK(lp, i), character(1))

  row_mask <- .build_name_mask(row_names, row_filter)
  col_mask <- .build_name_mask(col_names, col_filter)
  row_idx <- which(row_mask)

  if (!length(row_idx)) {
    return(data.frame(row = character(0), col = character(0), coef = numeric(0), stringsAsFactors = FALSE))
  }

  result <- vector("list", length(row_idx))
  progress_interval <- max(1, floor(length(row_idx) / 10))

  for (k in seq_along(row_idx)) {
    if (verbose && k %% progress_interval == 0) {
      cat(sprintf("  %3d%%\r", round(100 * k / length(row_idx))))
    }
    i <- row_idx[k]
    mat_row <- glpkAPI::getMatRowGLPK(lp, i)
    idx <- mat_row$index
    if (length(idx) <= 1) {
      next
    }
    idx <- idx[-1]
    vals <- mat_row$value[-1]
    keep <- col_mask[idx]
    if (!any(keep)) {
      next
    }
    result[[k]] <- data.frame(
      row = row_names[i],
      col = col_names[idx[keep]],
      coef = vals[keep],
      stringsAsFactors = FALSE
    )
  }

  if (verbose) {
    cat("  100%\n")
  }

  result <- result[lengths(result) > 0]
  if (!length(result)) {
    return(data.frame(row = character(0), col = character(0), coef = numeric(0), stringsAsFactors = FALSE))
  }

  do.call(rbind, result)
}

.build_name_mask <- function(names, filters) {
  if (length(names) == 0 || is.null(filters) || !length(filters)) {
    return(rep(TRUE, length(names)))
  }
  mask <- rep(FALSE, length(names))
  for (flt in filters) {
    if (!is.character(flt) || !nzchar(flt)) {
      next
    }
    mask <- mask | grepl(flt, names, perl = TRUE)
  }
  mask
}

.compare_matrix_entries <- function(mat1, mat2, tolerance = 0) {
  if (nrow(mat1) == 0 && nrow(mat2) == 0) {
    return(list(
      identical = TRUE,
      tolerance = tolerance,
      count = 0,
      max_abs_delta = 0,
      differences = data.frame(row = character(0), col = character(0), coef_ref = numeric(0), coef_tgt = numeric(0), delta = numeric(0), stringsAsFactors = FALSE)
    ))
  }

  names(mat1)[names(mat1) == "coef"] <- "coef_ref"
  names(mat2)[names(mat2) == "coef"] <- "coef_tgt"

  merged <- merge(mat1, mat2, by = c("row", "col"), all = TRUE)
  merged$coef_ref[is.na(merged$coef_ref)] <- 0
  merged$coef_tgt[is.na(merged$coef_tgt)] <- 0
  merged$delta <- merged$coef_tgt - merged$coef_ref

  diffs <- merged[abs(merged$delta) > tolerance, , drop = FALSE]
  list(
    identical = nrow(diffs) == 0,
    tolerance = tolerance,
    count = nrow(diffs),
    max_abs_delta = if (!nrow(diffs)) 0 else max(abs(diffs$delta)),
    differences = diffs
  )
}
