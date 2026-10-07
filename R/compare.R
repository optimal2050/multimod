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
#' m <- example_models$energyRt$multimod
#' eq1 <- m$equations$eqTechSng2Sng
#' eq2 <- m$equations$eqTechGrp2Sng
#' result <- compare_ast(eq1, eq2)
#' print(result)
#'
#' # Compare specific parts
#' compare_ast(eq1$rhs, eq2$rhs, path = "rhs")
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
#' @return Invisibly, `x`.
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

