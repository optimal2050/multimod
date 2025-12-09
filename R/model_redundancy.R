#' Analyze parameter redundancy in model
#'
#' Reports repeated values in parameters to identify redundancy before and after folding.
#' This helps assess folding effectiveness and identify remaining optimization potential.
#'
#' For each parameter with data, calculates:
#' - Original redundancy: ratio of repeated values to total rows
#' - Folded redundancy: ratio of repeated values in folded data (if folded)
#' - Compression achieved through folding
#'
#' @param model A multimod model object (folded or unfolded)
#' @param params Character vector of parameter names to analyze. If NULL (default),
#'   analyzes all parameters with data.
#' @param verbose Logical; print detailed output (default: TRUE)
#' @param return_details Logical; return detailed data frame instead of summary (default: FALSE)
#'
#' @return If return_details=FALSE, invisibly returns summary data frame.
#'   If return_details=TRUE, returns detailed data frame with per-parameter statistics.
#'
#' @details
#' Redundancy metrics:
#' - **Original redundancy**: percentage of rows that have duplicate values
#'   (within the full parameter dataset)
#' - **Folded redundancy**: percentage of rows with duplicate values after folding
#' - **Compression ratio**: original rows / folded rows
#' - **Remaining potential**: estimated further compression if all redundancy removed
#'
#' High original redundancy with no folding indicates missed optimization opportunity.
#' High folded redundancy suggests additional dimensions could be folded.
#'
#' @examples
#' \dontrun{
#' # Analyze all parameters
#' analyze_parameter_redundancy(model)
#'
#' # Analyze specific parameters
#' analyze_parameter_redundancy(model, params = c("pTechCinp2use", "pSupCost"))
#'
#' # Get detailed results
#' results <- analyze_parameter_redundancy(model, return_details = TRUE)
#' View(results)
#' }
#'
#' @export
analyze_parameter_redundancy <- function(model, 
                                         params = NULL,
                                         verbose = TRUE,
                                         return_details = FALSE) {
  
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  
  # Select parameters to analyze
  if (is.null(params)) {
    params <- names(model$parameters)
  }
  
  # Filter to parameters with data
  params <- params[sapply(params, function(pname) {
    if (!pname %in% names(model$parameters)) return(FALSE)
    param <- model$parameters[[pname]]
    !is.null(param$data) && nrow(param$data) > 0
  })]
  
  if (length(params) == 0) {
    if (verbose) cat("No parameters with data found\n")
    return(invisible(NULL))
  }
  
  results <- list()
  
  for (param_name in params) {
    param <- model$parameters[[param_name]]
    
    # Original data analysis
    orig_data <- param$data
    orig_rows <- nrow(orig_data)
    orig_unique_values <- length(unique(orig_data$value))
    orig_redundancy_pct <- (1 - orig_unique_values / orig_rows) * 100
    
    # Folded data analysis (if folded)
    is_folded <- !is.null(param$folded_data)
    if (is_folded) {
      fold_data <- param$folded_data
      fold_rows <- nrow(fold_data)
      fold_unique_values <- length(unique(fold_data$value))
      fold_redundancy_pct <- (1 - fold_unique_values / fold_rows) * 100
      compression_ratio <- orig_rows / fold_rows
      
      # Remaining potential if all redundancy removed
      remaining_potential <- fold_rows / fold_unique_values
    } else {
      fold_rows <- NA
      fold_unique_values <- NA
      fold_redundancy_pct <- NA
      compression_ratio <- NA
      remaining_potential <- orig_rows / orig_unique_values
    }
    
    # Dimensions
    dims <- get_dim_names(param$dims)
    if (is_folded) {
      active_dims <- get_dim_names(param$active_dims)
      removed_dims <- setdiff(dims, active_dims)
    } else {
      active_dims <- dims
      removed_dims <- character(0)
    }
    
    results[[param_name]] <- data.frame(
      param_name = param_name,
      n_dims = length(dims),
      dims = paste(dims, collapse = ", "),
      is_folded = is_folded,
      removed_dims = paste(removed_dims, collapse = ", "),
      orig_rows = orig_rows,
      orig_unique = orig_unique_values,
      orig_redundancy_pct = round(orig_redundancy_pct, 1),
      fold_rows = fold_rows,
      fold_unique = fold_unique_values,
      fold_redundancy_pct = round(fold_redundancy_pct, 1),
      compression_ratio = round(compression_ratio, 1),
      remaining_potential = round(remaining_potential, 1),
      stringsAsFactors = FALSE
    )
  }
  
  results_df <- do.call(rbind, results)
  rownames(results_df) <- NULL
  
  if (verbose) {
    cat("\n=== Parameter Redundancy Analysis ===\n")
    cat("Parameters analyzed:", nrow(results_df), "\n")
    cat("Folded parameters:", sum(results_df$is_folded, na.rm = TRUE), "\n\n")
    
    # Summary statistics
    cat("--- Summary Statistics ---\n")
    cat(sprintf("Original redundancy (avg): %.1f%%\n", 
                mean(results_df$orig_redundancy_pct, na.rm = TRUE)))
    
    folded_subset <- results_df[results_df$is_folded, ]
    if (nrow(folded_subset) > 0) {
      cat(sprintf("Folded redundancy (avg): %.1f%%\n",
                  mean(folded_subset$fold_redundancy_pct, na.rm = TRUE)))
      cat(sprintf("Average compression: %.1fx\n",
                  mean(folded_subset$compression_ratio, na.rm = TRUE)))
    }
    
    cat("\n--- High Redundancy Parameters (>50% repeated values) ---\n")
    high_redundancy <- results_df[results_df$orig_redundancy_pct > 50, ]
    if (nrow(high_redundancy) > 0) {
      high_redundancy <- high_redundancy[order(-high_redundancy$orig_redundancy_pct), ]
      print(high_redundancy[, c("param_name", "is_folded", "orig_redundancy_pct", 
                                "fold_redundancy_pct", "remaining_potential")],
            row.names = FALSE)
    } else {
      cat("None\n")
    }
    
    cat("\n--- Folded Parameters with Remaining Redundancy (>20%) ---\n")
    folded_high <- results_df[results_df$is_folded & 
                               !is.na(results_df$fold_redundancy_pct) & 
                               results_df$fold_redundancy_pct > 20, ]
    if (nrow(folded_high) > 0) {
      folded_high <- folded_high[order(-folded_high$fold_redundancy_pct), ]
      print(folded_high[, c("param_name", "removed_dims", "fold_redundancy_pct", 
                            "fold_rows", "fold_unique", "remaining_potential")],
            row.names = FALSE)
      cat("\nThese parameters may benefit from folding additional dimensions\n")
    } else {
      cat("None - excellent folding!\n")
    }
    
    cat("\n--- Unfolded Parameters with High Redundancy ---\n")
    unfolded_high <- results_df[!results_df$is_folded & 
                                 results_df$orig_redundancy_pct > 50, ]
    if (nrow(unfolded_high) > 0) {
      unfolded_high <- unfolded_high[order(-unfolded_high$orig_redundancy_pct), ]
      print(unfolded_high[, c("param_name", "orig_redundancy_pct", "orig_rows", 
                              "orig_unique", "remaining_potential")],
            row.names = FALSE)
      cat("\nThese parameters are good candidates for folding\n")
    } else {
      cat("None\n")
    }
  }
  
  if (return_details) {
    return(results_df)
  } else {
    invisible(results_df)
  }
}


#' Get dimension names from dims object (internal helper)
#'
#' @param dims dims object or character vector
#' @return Character vector of dimension names
#' @keywords internal
get_dim_names <- function(dims) {
  if (is.null(dims)) return(character(0))
  if (is.character(dims)) return(dims)
  
  if (inherits(dims, "dims")) {
    vapply(seq_along(dims), function(i) {
      d <- dims[[i]]
      if (inherits(d, "symbol")) {
        d$name
      } else if (inherits(d, "set")) {
        d$name
      } else {
        as.character(d)
      }
    }, character(1))
  } else {
    as.character(dims)
  }
}
