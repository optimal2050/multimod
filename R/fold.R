#' Create fold specification for energyRt models (convenience wrapper)
#'
#' Convenience wrapper around the generic \code{create_fold_spec()} function.
#' Uses the same underlying implementation but provides energyRt-specific defaults
#' for entity-to-mapping associations.
#'
#' @param model A multimod model object (from energyRt)
#' @param fold_dims Dimension configuration. Same format as \code{create_fold_spec()}.
#'   Can be:
#'   - Character vector: dimension names (e.g., \code{c("slice")})  
#'   - Named list (simple): dimension → mapping (e.g., \code{list(slice = "mCommSlice")})
#'   - Named list (advanced): dimension → entity-specific mappings
#'     (e.g., \code{list(slice = list(tech = "mTechSlice", comm = "mCommSlice"))})
#' @param tolerance Numeric tolerance for considering values equal (default: 1e-10)
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return Data frame with columns: param_name, dim_to_fold, validation_mapping,
#'   can_fold, reason
#'   
#' @details
#' **Note**: This function has the SAME functionality as \code{create_fold_spec()}.
#' The only difference is naming - this version emphasizes it's designed for energyRt
#' models, but both functions support all features (entity-specific mappings, chains, etc.).
#' 
#' Mapping priority (for entity-specific configuration):
#' - Uses FIRST matching entity dimension
#' - If parameter has "tech" → use tech mapping
#' - Else if has "sup" → use sup mapping  
#' - Else if has "comm" → use comm mapping
#' - Else if has "stg" → use stg mapping
#' - Else → NA (no validation)
#' 
#' Mapping chains join mappings by common columns:
#' Example: \code{stg = c("mStorageComm", "mCommSlice")}
#' - \code{mStorageComm[stg, comm]} ⨝ \code{mCommSlice[comm, slice]} → \code{[stg, slice]}
#' 
#' For derived mappings (chains), mappings are joined in sequence by common columns.
#' Example: \code{stg = c("mStorageComm", "mCommSlice")}
#' - \code{mStorageComm[stg, comm]} inner_join \code{mCommSlice[comm, slice]} → \code{[stg, slice]}
#'
#' @examples
#' \dontrun{
#' # Explicit configuration for energyRt models
#' fold_spec <- create_fold_spec_energyRt(model, 
#'   fold_dims = list(
#'     slice = list(
#'       tech = "mTechSlice",
#'       sup = "mSupSlice", 
#'       comm = "mCommSlice",
#'       stg = c("mStorageComm", "mCommSlice"),
#'       trade = c("mTradeComm", "mCommSlice"),
#'       imp = c("mImportComm", "mCommSlice")
#'     )
#'   ))
#' 
#' # Review and edit
#' View(fold_spec)
#' 
#' # Use edited specification
#' model <- fold_model(model, fold_spec = fold_spec)
#' }
#' @export
create_fold_spec_energyRt <- function(model,
                                       fold_dims,
                                       tolerance = 1e-10,
                                       verbose = TRUE) {
  create_fold_spec(model, fold_dims, tolerance, verbose)
}

#' Get active dimensions from a parameter
#'
#' Returns the dimensions that are actually present in a parameter's data.
#' For folded parameters, returns the reduced dimensions. For unfolded parameters,
#' returns the original dimensions.
#'
#' @param param A parameter object with \code{dims} and optionally \code{active_dims}
#' @param folded Logical; if TRUE, returns folded dimensions only (returns NULL for
#'   unfolded parameters). If FALSE (default), returns active dimensions regardless
#'   of folding state.
#'
#' @return A dims object (list of set objects), or NULL if folded=TRUE and parameter
#'   is not folded
#'
#' @details
#' During folding, parameters store \code{active_dims} which reflects the reduced
#' dimensionality. This function provides a consistent way to retrieve the appropriate
#' dimensions for code generation and validation.
#'
#' @examples
#' \dontrun{
#' # Get active dimensions (works for folded or unfolded)
#' dims <- get_active_dims(param)
#' 
#' # Check if parameter is folded
#' folded_dims <- get_active_dims(param, folded = TRUE)
#' is_folded <- !is.null(folded_dims)
#' }
#'
#' @export
get_active_dims <- function(param, folded = FALSE) {
  stopifnot(inherits(param, "parameter"))
  
  if (folded) {
    # Return folded dimensions only if parameter is folded
    if (!is.null(param$active_dims)) {
      return(param$active_dims)
    } else {
      return(NULL)
    }
  } else {
    # Return active dimensions (folded if available, otherwise original)
    if (!is.null(param$active_dims)) {
      return(param$active_dims)
    } else {
      return(param$dims)
    }
  }
}

#' Get folding statistics summary
#'
#' Retrieves comprehensive folding statistics including list of folded parameters,
#' dimension reductions, compression ratios, and removed dimensions.
#'
#' @param model A multimod model object
#' @param format Character string specifying output format: "list" (default) 
#'   returns structured list, "data.frame" returns tabular format, "text" prints
#'   formatted summary
#'
#' @return Depending on format:
#' \itemize{
#'   \item "list": List with fold_summary and statistics
#'   \item "data.frame": Data frame with one row per folded parameter
#'   \item "text": Prints formatted summary and returns invisibly
#' }
#'
#' @details
#' The function extracts folding statistics stored in model$misc$fold_summary
#' during fold_model() execution. Each folded parameter includes:
#' \itemize{
#'   \item Original and folded dimensions
#'   \item Removed dimension names
#'   \item Original and folded row counts
#'   \item Compression ratio
#' }
#'
#' @examples
#' \dontrun{
#' # Get structured list
#' stats <- get_fold_summary(model_folded)
#' 
#' # Get as data frame
#' df <- get_fold_summary(model_folded, format = "data.frame")
#' 
#' # Print formatted summary
#' get_fold_summary(model_folded, format = "text")
#' }
#'
#' @export
get_fold_summary <- function(model, format = c("list", "data.frame", "text"), verbose = FALSE) {
  format <- match.arg(format)
  
  # Check if model has fold summary
  if (is.null(model$misc$fold_summary) || length(model$misc$fold_summary) == 0) {
    if (format == "text" && verbose) {
      cat("No folding applied to this model.\n")
      return(invisible(NULL))
    } else {
      return(NULL)
    }
  }
  
  fold_summary <- model$misc$fold_summary
  
  # Calculate aggregate statistics
  n_folded <- length(fold_summary)
  n_total <- length(model$parameters)
  
  # Get detailed parameter information
  param_info <- lapply(names(fold_summary), function(param_name) {
    param <- model$parameters[[param_name]]
    summary <- fold_summary[[param_name]]
    
    list(
      parameter = param_name,
      original_dims = summary$original,
      folded_dims = summary$folded,
      removed_dims = paste(param$misc$fold_info$removed_dims, collapse = ", "),
      original_rows = summary$original_rows,
      folded_rows = summary$folded_rows,
      compression = summary$compression,
      compression_ratio = param$misc$fold_info$compression_ratio
    )
  })
  
  # Return based on format
  if (format == "list") {
    return(list(
      n_folded = n_folded,
      n_total = n_total,
      parameters = param_info,
      fold_summary = fold_summary
    ))
  } else if (format == "data.frame") {
    df <- do.call(rbind, lapply(param_info, as.data.frame))
    rownames(df) <- NULL
    return(df)
  } else if (format == "text") {
    if (!verbose) {
      # Silent mode - return data without printing
      return(invisible(list(
        n_folded = n_folded,
        n_total = n_total,
        parameters = param_info,
        fold_summary = fold_summary
      )))
    }
    
    cat(sprintf("\n=== Folding Statistics ===\n\n"))
    cat(sprintf("Parameters folded: %d / %d (%.1f%%)\n\n", 
                n_folded, n_total, 100 * n_folded / n_total))
    
    cat(sprintf("%-25s %20s %20s %15s %10s\n", 
                "Parameter", "Original Dims", "Folded Dims", "Rows", "Compression"))
    cat(strrep("-", 95), "\n")
    
    for (info in param_info) {
      cat(sprintf("%-25s %20s %20s %6d→%-6d %10s\n",
                  info$parameter,
                  info$original_dims,
                  info$folded_dims,
                  info$original_rows,
                  info$folded_rows,
                  info$compression))
    }
    
    cat("\nRemoved dimensions by parameter:\n")
    for (info in param_info) {
      cat(sprintf("  %-25s: %s\n", info$parameter, info$removed_dims))
    }
    
    return(invisible(param_info))
  }
}

#' Check if a parameter or model is folded
#'
#' Tests whether a parameter has been folded (reduced dimensions) or whether
#' a model contains any folded parameters.
#'
#' @param x A parameter object or multimod model object
#'
#' @return Logical value indicating if the parameter/model is folded
#'
#' @details
#' For parameters: Returns TRUE if the parameter has active_dims set (indicating
#' it has been folded), FALSE otherwise.
#'
#' For models: Returns TRUE if the model has any folded parameters, FALSE otherwise.
#' Also returns TRUE if the model has folded_equations.
#'
#' @examples
#' \dontrun{
#' # Check if a parameter is folded
#' is_folded(model$parameters$pTechCinp2use)
#' 
#' # Check if a model has any folded parameters
#' is_folded(model)
#' }
#'
#' @export
is_folded <- function(x) {
  UseMethod("is_folded")
}

#' @export
is_folded.parameter <- function(x) {
  # A parameter is considered folded if it has fold_info indicating folding occurred
  !is.null(x$misc$fold_info) && isTRUE(x$misc$fold_info$folded)
}

#' @export
is_folded.model <- function(x) {
  # Check if any parameters are folded
  if (!is.null(x$parameters) && length(x$parameters) > 0) {
    for (param in x$parameters) {
      if (is_folded(param)) {
        return(TRUE)
      }
    }
  }
  
  # Check if folded_equations exists
  if (!is.null(x$folded_equations) && length(x$folded_equations) > 0) {
    return(TRUE)
  }
  
  return(FALSE)
}

#' @export
is_folded.multimod <- function(x) {
  is_folded.model(x)
}

#' @export
is_folded.default <- function(x) {
  # Return FALSE for objects without folding support
  # This allows safe checking without errors
  if (is.null(x)) return(FALSE)
  
  # Check if it's a model-like object without proper class
  if (is.list(x)) {
    # Try to check for folded parameters
    if (!is.null(x$parameters) && length(x$parameters) > 0) {
      for (param in x$parameters) {
        if (!is.null(param$misc$fold_info) && isTRUE(param$misc$fold_info$folded)) {
          return(TRUE)
        }
      }
    }
    
    # Check for folded_equations
    if (!is.null(x$folded_equations) && length(x$folded_equations) > 0) {
      return(TRUE)
    }
    
    return(FALSE)
  }
  
  # For other types, return FALSE instead of error
  return(FALSE)
}

create_fold_spec_energyRt <- function(model,
                                       fold_dims,
                                       tolerance = 1e-10,
                                       verbose = TRUE) {
  
  `%>%` <- magrittr::`%>%`
  
  # Parse fold_dims configuration
  if (is.list(fold_dims)) {
    # Explicit configuration: list(slice = list(tech = "mTechSlice", stg = c("mStorageComm", "mCommSlice")))
    dim_entity_mapping_config <- fold_dims
    fold_dims_vec <- names(fold_dims)
  } else {
    # Simple vector: c("slice", "region") - but no default mappings, will return NA
    fold_dims_vec <- fold_dims
    dim_entity_mapping_config <- list()
  }
  
  # Validate fold_dims
  if (length(fold_dims_vec) == 0) {
    if (verbose) cat("No dimensions selected for folding specification\n")
    return(data.frame(
      param_name = character(0),
      dim_to_fold = character(0),
      validation_mapping = character(0),
      validation_mapping_raw = I(list()),
      can_fold = logical(0),
      reason = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  spec_list <- list()
  
  for (param_name in names(model$parameters)) {
    param <- model$parameters[[param_name]]
    
    # Skip if no data (check both data and folded_data)
    param_data <- param$data
    if (is.null(param_data) || nrow(param_data) == 0) {
      # If already folded, we can't fold again
      if (!is.null(param$folded_data)) {
        next  # Already folded, skip
      }
      next
    }
    
    dims <- get_dim_names(param$dims)
    foldable_dims <- intersect(dims, fold_dims_vec)
    
    if (length(foldable_dims) == 0) next
    
    # Skip if all dimensions are foldable (would become scalar)
    if (length(foldable_dims) == length(dims)) next
    
    # For each foldable dimension, create specification entry
    for (dim_to_fold in foldable_dims) {
      
      # Get validation mapping from configuration
      if (dim_to_fold %in% names(dim_entity_mapping_config)) {
        entity_mapping <- dim_entity_mapping_config[[dim_to_fold]]
        
        # Find which entity dimension matches (priority-based)
        mapping_spec <- NULL
        for (entity_dim in names(entity_mapping)) {
          if (entity_dim %in% dims) {
            mapping_spec <- entity_mapping[[entity_dim]]
            break
          }
        }
        
        if (is.null(mapping_spec)) {
          validation_mapping_raw <- NA_character_
          validation_mapping_display <- NA_character_
        } else if (length(mapping_spec) == 1) {
          # Direct mapping
          validation_mapping_raw <- mapping_spec
          validation_mapping_display <- mapping_spec
        } else {
          # Chain of mappings
          validation_mapping_raw <- mapping_spec  # Store as vector
          validation_mapping_display <- paste(mapping_spec, collapse = " → ")
        }
      } else {
        validation_mapping_raw <- NA_character_
        validation_mapping_display <- NA_character_
      }
      
      # Analyze if can fold with this mapping
      result <- analyze_dimension_redundancy(
        param = param,
        dim_to_test = dim_to_fold,
        model = model,
        tolerance = tolerance,
        validation_mapping = validation_mapping_raw
      )
      
      spec_list[[length(spec_list) + 1]] <- data.frame(
        param_name = param_name,
        dim_to_fold = dim_to_fold,
        validation_mapping = validation_mapping_display,
        validation_mapping_raw = I(list(validation_mapping_raw)),  # Wrap in list() here
        can_fold = result$can_fold,
        reason = result$reason,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(spec_list) == 0) {
    return(data.frame(
      param_name = character(0),
      dim_to_fold = character(0),
      validation_mapping = character(0),
      validation_mapping_raw = I(list()),
      can_fold = logical(0),
      reason = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  spec_df <- do.call(rbind, spec_list)
  
  if (verbose) {
    cat("\n=== Fold Specification Generated ===\n")
    cat("Total entries:", nrow(spec_df), "\n")
    cat("Can fold:", sum(spec_df$can_fold), "\n")
    cat("Cannot fold:", sum(!spec_df$can_fold), "\n")
    cat("\nREVIEW CAREFULLY: validation_mapping may be incorrect for non-standard models\n\n")
  }
  
  return(spec_df)
}


#' Guess validation mapping for fold specification (energyRt-specific)
#'
#' Uses parameter dimensions and entity-to-mapping configuration to select
#' the appropriate validation mapping. Uses PRIORITY-BASED selection: returns
#' the mapping for the FIRST matching entity dimension found.
#'
#' Supports mapping chains for derived mappings (e.g., stg → comm → slice).
#'
#' @param param_name Parameter name (for debugging)
#' @param dim_to_fold Dimension to fold
#' @param dims All dimensions in parameter
#' @param entity_mapping Named list mapping entity dimensions to validation mappings.
#'   Values can be single string or character vector for chains.
#'   Example: list(tech = "mTechSlice", 
#'                 stg = c("mStorageComm", "mCommSlice"))
#'
#' @return Mapping specification: single string or character vector for chains
#' @keywords internal
guess_validation_mapping_energyRt <- function(param_name, dim_to_fold, dims, entity_mapping) {
  
  if (dim_to_fold != "slice") {
    # Not implemented for region/year yet
    return(NA_character_)
  }
  
  # Priority-based selection: use FIRST matching entity dimension
  # For pTechCinp2use[tech, comm, ...]: has both tech and comm, returns mTechSlice (tech wins)
  # For pStorage...[stg, comm, ...]: has stg, returns c("mStorageComm", "mCommSlice")
  for (entity_dim in names(entity_mapping)) {
    if (entity_dim %in% dims) {
      mapping_spec <- entity_mapping[[entity_dim]]
      # Return as-is: can be single string or vector (chain)
      if (length(mapping_spec) == 1) {
        return(mapping_spec)
      } else {
        # Return chain as concatenated string with separator for storage
        return(paste(mapping_spec, collapse = " → "))
      }
    }
  }
  
  return(NA_character_)
}


#' Create fold specification for parameters (generic interface)
#'
#' Generic interface for creating fold specifications. Supports all the same 
#' features as \code{create_fold_spec_energyRt} but without energyRt-specific defaults.
#'
#' @param model A multimod model object
#' @param fold_dims Dimension configuration. Can be:
#'   - Character vector: dimension names (e.g., \code{c("slice")})
#'   - Named list (simple): dimension → mapping (e.g., \code{list(slice = "mCommSlice")})
#'   - Named list (advanced): dimension → entity-specific mappings 
#'     (e.g., \code{list(slice = list(tech = "mTechSlice", comm = "mCommSlice"))})
#' @param tolerance Numeric tolerance for detecting constant values (default: 1e-10)
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return Data frame with fold specification for each parameter
#' 
#' @examples
#' \dontrun{
#' # Simple: fold slice dimension
#' fold_spec <- create_fold_spec(model, fold_dims = "slice")
#' 
#' # With single validation mapping
#' fold_spec <- create_fold_spec(model, fold_dims = list(slice = "mCommSlice"))
#' 
#' # Entity-specific (advanced)
#' fold_spec <- create_fold_spec(
#'   model,
#'   fold_dims = list(
#'     slice = list(
#'       tech = "mTechSlice",
#'       comm = "mCommSlice",
#'       stg = c("mStorageComm", "mCommSlice")  # Chain
#'     )
#'   )
#' )
#' }
#' @export
create_fold_spec <- function(model,
                              fold_dims,
                              tolerance = 1e-10,
                              verbose = TRUE) {
  
  # Delegate to energyRt implementation which has full functionality
  # (entity-specific mappings, chains, etc.)
  create_fold_spec_energyRt(model, fold_dims, tolerance, verbose)
}


#' Fold model dimensions to reduce data size
#'
#' Reduces redundant dimensions in parameters where data is constant across
#' those dimensions. Only folds high-cardinality indexing dimensions (slice,
#' optionally region/year) while preserving structural dimensions (tech, comm, etc.).
#'
#' The algorithm uses per-group validation: for each combination of OTHER dimensions,
#' it checks if values are identical. This is an "all or nothing" approach - if ANY
#' group shows variation, the dimension is not folded.
#'
#' @param model A multimod model object
#' @param fold_spec Data frame specifying fold configuration. Must have columns:
#'   param_name, dim_to_fold, validation_mapping. Use `create_fold_spec()` to
#'   generate proper fold specification with entity-specific mappings.
#' @param tolerance Numeric tolerance for considering values equal (default: 1e-10)
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return Modified model with folded parameters
#' 
#' @details
#' Folding analyzes each parameter to identify dimensions that don't provide
#' variation in the data. For each parameter, it groups by all OTHER dimensions
#' and checks if values are constant within each group (within tolerance).
#' 
#' Example: For \code{pStorageInpEff[stg, comm, region, year, slice]} testing slice:
#' - Groups by \code{(stg, comm, region, year)}
#' - For each group, checks if ALL slice values are identical
#' - Only folds if EVERY group has constant values
#' 
#' This handles sparse data correctly - if a parameter only has values for
#' peak hours (1 slice out of 17), that's fine. It only checks if the values
#' that DO exist are constant within each group.
#' 
#' The fold creates three slots per parameter:
#' - `data`: Original complete data (preserved)
#' - `folded_data`: Collapsed data with reduced dimensions  
#' - `active_dims`: Dimensions remaining after fold
#' 
#' Safety measures:
#' - Only folds dimensions explicitly requested by user
#' - Never folds structural dimensions (tech, comm, sup, dem, stg, trade, etc.)
#' - Skips parameters where all dimensions would be folded (would become scalar)
#' - Validates consistency before folding
#' - Updates equation ASTs to match reduced dimensions
#' 
#' @examples
#' \dontrun{
#' # Specify fold dimensions with entity-specific mappings
#' fold_dims <- list(
#'   slice = c("mSupSlice", "mDemSlice", "mStorageSlice", "mTradeSlice"),
#'   region = c("mTechRegion", "mSupRegion", "mDemRegion", "mStorageRegion"),
#'   year = c("mTechYear", "mSupYear", "mDemYear", "mStorageYear")
#' )
#' fold_spec <- create_fold_spec(model, fold_dims = fold_dims)
#' # Review and edit fold_spec to ensure correct validation mappings
#' model <- fold_model(model, fold_spec = fold_spec)
#' }
#' 
#' @keywords internal
should_enable_progress_fold <- function(model, fold_spec, 
                                        param_threshold = 30,
                                        row_threshold = 50000) {
  if (!requireNamespace("progressr", quietly = TRUE)) {
    return(FALSE)
  }
  
  # Count parameters in spec that have data
  params_with_data <- sum(vapply(fold_spec$param_name, function(pname) {
    pname %in% names(model$parameters) && 
      !is.null(model$parameters[[pname]]$data) &&
      nrow(model$parameters[[pname]]$data) > 0
  }, logical(1)))
  
  if (params_with_data > param_threshold) return(TRUE)
  
  # Check total data size (proxy for processing time)
  total_rows <- sum(vapply(model$parameters, function(p) {
    if (!is.null(p$data)) nrow(p$data) else 0L
  }, integer(1)))
  
  return(total_rows > row_threshold)
}

#' @export
fold_model <- function(model, 
                       fold_spec,
                       tolerance = 1e-10,
                       verbose = TRUE,
                       .progress = "auto") {
  `%>%` <- magrittr::`%>%`
  
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  stopifnot("fold_spec parameter is required. Use create_fold_spec() to generate it." = !missing(fold_spec))
  
  # Determine if progress should be enabled
  use_progress <- FALSE
  if (.progress == "auto") {
    use_progress <- should_enable_progress_fold(model, fold_spec)
  } else if (isTRUE(.progress)) {
    use_progress <- requireNamespace("progressr", quietly = TRUE)
  }
  
  if (verbose) {
    cat("\n=== Folding Model ===\n")
    cat("Using provided fold specification\n")
    cat("Parameters specified:", nrow(fold_spec), "\n")
    
    # Check if multiple dimensions are being folded
    unique_dims <- unique(fold_spec$dim_to_fold)
    if (length(unique_dims) > 1) {
      cat("Multi-dimensional folding detected:", paste(unique_dims, collapse = ", "), "\n")
      cat("Applying folds sequentially by dimension\n")
    }
    cat("\n")
  }
  
  # Validate fold specification
  required_cols <- c("param_name", "dim_to_fold")
  if (!all(required_cols %in% names(fold_spec))) {
    stop("fold_spec must have columns: param_name, dim_to_fold, validation_mapping_raw")
  }
  
  # Add validation_mapping_raw if not present (backward compatibility)
  if (!"validation_mapping_raw" %in% names(fold_spec)) {
    # Try to use validation_mapping column
    if ("validation_mapping" %in% names(fold_spec)) {
      # Convert to raw format (assume no chains if coming from old code)
      fold_spec$validation_mapping_raw <- lapply(fold_spec$validation_mapping, function(x) {
        if (is.na(x)) {
          NA_character_
        } else {
          x
        }
      })
    } else {
      stop("fold_spec must have either validation_mapping_raw or validation_mapping column")
    }
  }
  
  # Check if multiple dimensions - apply sequentially if so
  unique_dims <- unique(fold_spec$dim_to_fold)
  if (length(unique_dims) > 1) {
    # Sequential multi-dimensional folding
    current_model <- model
    
    for (dim_name in unique_dims) {
      if (verbose) {
        cat(sprintf("=== Folding dimension: %s ===\n", dim_name))
      }
      
      # Extract fold spec for this dimension only
      dim_spec <- fold_spec[fold_spec$dim_to_fold == dim_name, ]
      
      # Recursively call fold_model with single-dimension spec
      current_model <- fold_model(
        current_model,
        fold_spec = dim_spec,
        tolerance = tolerance,
        verbose = verbose,
        .progress = .progress
      )
    }
    
    return(current_model)
  }
  
  # Wrap main loop with progress if enabled
  if (use_progress) {
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(fold_spec))
      fold_model_loop(model, fold_spec, tolerance, verbose, p)
    })
  } else {
    fold_model_loop(model, fold_spec, tolerance, verbose, NULL)
  }
}

#' Internal fold loop with optional progress reporting
#' @keywords internal
fold_model_loop <- function(model, fold_spec, tolerance, verbose, p = NULL) {
  `%>%` <- magrittr::`%>%`
  
  fold_summary <- list()
  n_folded <- 0
  n_skipped <- 0
  
  for (i in seq_len(nrow(fold_spec))) {
    if (!is.null(p)) {
      p(sprintf("Folding parameters (%d/%d)", i, nrow(fold_spec)))
    }
    param_name <- fold_spec$param_name[i]
    dim_to_fold <- fold_spec$dim_to_fold[i]
    
    # Get raw validation mapping (could be vector for chains)
    if ("validation_mapping_raw" %in% names(fold_spec)) {
      validation_mapping <- fold_spec$validation_mapping_raw[[i]]
    } else {
      # Fallback to validation_mapping column
      validation_mapping <- fold_spec$validation_mapping[i]
    }
    
    # For display purposes
    if ("validation_mapping" %in% names(fold_spec)) {
      validation_mapping_display <- fold_spec$validation_mapping[i]
    } else {
      if (length(validation_mapping) > 1) {
        validation_mapping_display <- paste(validation_mapping, collapse = " → ")
      } else {
        validation_mapping_display <- validation_mapping
      }
    }
    
    if (!param_name %in% names(model$parameters)) {
      if (verbose) cat("⊠ Skipping", param_name, "- not found in model\n")
      n_skipped <- n_skipped + 1
      next
    }
    
    param <- model$parameters[[param_name]]
    
    # Skip if no data
    if (is.null(param$data) || nrow(param$data) == 0) {
      n_skipped <- n_skipped + 1
      next
    }
    
    # Check if parameter already has folded_data from previous fold iteration
    # If so, work from the folded data and active_dims instead of original data
    if (!is.null(param$folded_data) && !is.null(param$active_dims)) {
      working_data <- param$folded_data
      working_dims <- param$active_dims
      dims <- get_dim_names(working_dims)
    } else {
      working_data <- param$data
      working_dims <- param$dims
      dims <- get_dim_names(param$dims)
    }
    
    # Check if dimension exists in parameter
    if (!dim_to_fold %in% dims) {
      if (verbose) cat("⊠ Skipping", param_name, "- doesn't have dimension", dim_to_fold, "\n")
      n_skipped <- n_skipped + 1
      next
    }
    
    # Analyze redundancy with specified validation mapping
    result <- analyze_dimension_redundancy(
      param = param,
      dim_to_test = dim_to_fold,
      model = model,
      tolerance = tolerance,
      validation_mapping = validation_mapping
    )
    
    if (!result$can_fold) {
      n_skipped <- n_skipped + 1
      next
    }
    
    # Apply fold
    original_dims <- dims
    # Use match-based removal to preserve duplicate dimension names
    # setdiff() removes duplicates, so we need to manually remove only the specific match
    dim_names <- get_dim_names(dims)
    dims_to_remove_match <- dim_names == dim_to_fold
    active_dims <- dims[!dims_to_remove_match]
    
    # For data collapsing, use actual data columns (excluding 'value', 'n_original', and the folded dimension)
    # This handles cases where dimensions use positional aliases (e.g., {region, region} → src, dst)
    data_cols <- setdiff(names(working_data), c("value", "n_original", dim_to_fold))
    active_cols_in_data <- data_cols
    
    # Collapse data using actual column names
    folded_data <- working_data %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(active_cols_in_data))) %>%
      dplyr::summarize(
        value = dplyr::first(value),
        n_original = dplyr::n(),
        .groups = "drop"
      )
    
    # Validate consistency
    consistency_check <- working_data %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(active_cols_in_data))) %>%
      dplyr::summarize(
        value_range = max(value) - min(value),
        .groups = "drop"
      )
    
    max_inconsistency <- max(consistency_check$value_range, na.rm = TRUE)
    if (max_inconsistency > tolerance) {
      warning(sprintf("Parameter %s: inconsistent values when folding (max range: %.2e)",
                     param_name, max_inconsistency))
    }
    
    # Update parameter
    param$active_dims <- ast_dims(active_dims)
    param$folded_data <- folded_data
    
    # Accumulate removed dimensions (for sequential multi-dimensional folding)
    existing_removed_dims <- if (!is.null(param$misc$fold_info$removed_dims)) {
      param$misc$fold_info$removed_dims
    } else {
      character(0)
    }
    all_removed_dims <- unique(c(existing_removed_dims, dim_to_fold))
    
    # Store metadata
    param$misc$fold_info <- list(
      folded = TRUE,
      original_dims = original_dims,
      removed_dims = all_removed_dims,  # Accumulated across sequential folds
      validation_mapping = validation_mapping,
      original_rows = nrow(param$data),
      folded_rows = nrow(folded_data),
      compression_ratio = nrow(param$data) / nrow(folded_data),
      fold_timestamp = Sys.time(),
      fold_details = list(result),
      max_inconsistency = max_inconsistency
    )
    
    model$parameters[[param_name]] <- param
    
    # Track for summary
    fold_summary[[param_name]] <- list(
      original = paste(original_dims, collapse=" × "),
      folded = paste(active_dims, collapse=" × "),
      removed = dim_to_fold,
      original_rows = nrow(param$data),
      folded_rows = nrow(folded_data),
      compression = sprintf("%.1fx", nrow(param$data) / nrow(folded_data))
    )
    
    n_folded <- n_folded + 1
    
    if (verbose) {
      cat(sprintf("✓ %s: %dD → %dD (%d → %d rows, %.1fx)\n",
                 param_name, 
                 length(original_dims),
                 length(active_dims),
                 nrow(param$data),
                 nrow(folded_data),
                 nrow(param$data) / nrow(folded_data)))
    }
  }
  
  # Store summary in model metadata
  if (is.null(model$misc)) model$misc <- list()
  model$misc$fold_summary <- fold_summary
  model$misc$folded <- (length(fold_summary) > 0)
  model$misc$fold_options <- list(
    tolerance = tolerance
  )
  
  if (verbose) {
    cat(sprintf("\n=== Fold Summary ===\n"))
    cat(sprintf("Parameters folded: %d\n", n_folded))
    cat(sprintf("Parameters skipped: %d\n", n_skipped))
    cat(sprintf("Total parameters: %d\n", length(model$parameters)))
  }
  
  # Fold equations if any parameters were folded
  if (n_folded > 0) {
    model <- fold_equations(model, verbose = verbose)
  }
  
  return(model)
}


#' Analyze if a dimension can be safely folded
#'
#' Checks if a parameter's dimension is redundant (values constant across it).
#' Uses per-group validation: for each combination of OTHER dimensions,
#' checks if values are identical within that group.
#'
#' This is an "all or nothing" approach - if ANY group shows variation,
#' the dimension cannot be folded, as we don't support partial folding.
#'
#' @param param Parameter object
#' @param dim_to_test Dimension name to analyze
#' @param model Model object (for set membership and alias resolution)
#' @param tolerance Numeric tolerance for value equality (default 1e-10)
#' @param validation_mapping Optional. Name of mapping to use for coverage validation
#'   (e.g., "mTechSlice"). If NULL, auto-detects based on parameter dimensions.
#'
#' @return List with can_fold (logical) and reason (character)
#' @keywords internal
analyze_dimension_redundancy <- function(param, dim_to_test, model, 
                                         tolerance = 1e-10,
                                         validation_mapping = NULL) {
  
  # Ensure pipe operator is available
  `%>%` <- magrittr::`%>%`
  
  dims <- get_dim_names(param$dims)
  data <- param$data
  
  # Step 1: Check if this dimension exists in model sets
  if (!dim_to_test %in% names(model$sets)) {
    return(list(can_fold = FALSE, reason = "Dimension not found in model sets"))
  }
  
  # Step 2: Check if dimension is in data
  if (!dim_to_test %in% names(data)) {
    return(list(can_fold = FALSE, reason = "Dimension not in data"))
  }
  
  # Step 3: For slice dimension, get timeframe mapping for coverage validation
  # This is CRITICAL: we need to verify that data covers ALL applicable slices
  # for each entity. Otherwise folding would broadcast values to slices that
  # should use the default value.
  #
  # The validation_mapping parameter can be:
  # - Single string: direct mapping name (e.g., "mTechSlice")
  # - Character vector: chain of mappings to join (e.g., c("mStorageComm", "mCommSlice"))
  timeframe_mapping <- NULL
  entity_dim <- NULL
  
  if (dim_to_test == "slice") {
    if (!is.null(validation_mapping) && length(validation_mapping) > 0 && !is.na(validation_mapping[1])) {
      
      if (length(validation_mapping) == 1) {
        # Direct mapping
        mapping_name <- validation_mapping
        
        if (mapping_name %in% names(model$mappings)) {
          timeframe_mapping <- model$mappings[[mapping_name]]$data
          
          # Extract entity dimension from mapping name
          entity_mapping <- c(
            "mTechSlice" = "tech",
            "mCommSlice" = "comm",
            "mSupSlice" = "sup"
          )
          entity_dim <- entity_mapping[mapping_name]
          
          # Verify this entity dimension is actually in the parameter
          if (!is.na(entity_dim) && !(entity_dim %in% dims)) {
            return(list(
              can_fold = FALSE,
              reason = sprintf("Validation mapping %s requires dimension %s, but parameter doesn't have it",
                             mapping_name, entity_dim)
            ))
          }
        } else {
          return(list(
            can_fold = FALSE,
            reason = sprintf("Validation mapping %s not found in model", mapping_name)
          ))
        }
        
      } else {
        # Chain of mappings - join them in sequence
        # Example: c("mStorageComm", "mCommSlice") 
        # mStorageComm[stg, comm] join mCommSlice[comm, slice] → [stg, slice]
        
        tryCatch({
          result_mapping <- model$mappings[[validation_mapping[1]]]$data
          entity_dim <- names(result_mapping)[1]  # First column is entity dimension
          
          # Join subsequent mappings
          for (i in 2:length(validation_mapping)) {
            next_mapping <- model$mappings[[validation_mapping[i]]]$data
            
            # Find common column(s) for join
            common_cols <- intersect(names(result_mapping), names(next_mapping))
            if (length(common_cols) == 0) {
              return(list(
                can_fold = FALSE,
                reason = sprintf("Cannot join %s and %s: no common columns",
                               validation_mapping[i-1], validation_mapping[i])
              ))
            }
            
            # Inner join
            result_mapping <- dplyr::inner_join(result_mapping, next_mapping, by = common_cols)
          }
          
          timeframe_mapping <- result_mapping
          
          # Verify entity dimension is in parameter
          if (!(entity_dim %in% dims)) {
            return(list(
              can_fold = FALSE,
              reason = sprintf("Derived mapping chain requires dimension %s, but parameter doesn't have it",
                             entity_dim)
            ))
          }
          
        }, error = function(e) {
          return(list(
            can_fold = FALSE,
            reason = sprintf("Error deriving mapping from chain: %s", e$message)
          ))
        })
      }
    }
  }
  
  # Step 4: For scalar case - would become scalar parameter
  other_dims <- setdiff(dims, dim_to_test)
  
  if (length(other_dims) == 0) {
    # Would become scalar - check if all values identical
    n_unique <- length(unique(data$value))
    return(list(
      can_fold = (n_unique == 1),
      reason = if (n_unique == 1) {
        "All values identical - can fold to scalar"
      } else {
        sprintf("Only dimension with variation (%d unique values) - cannot fold", n_unique)
      }
    ))
  }
  
  # Get dimension aliases if available
  # Convert model$aliases (list of pairs) to a lookup structure
  alias_map <- list()
  if (!is.null(model$aliases)) {
    for (alias_pair in model$aliases) {
      if (length(alias_pair) == 2) {
        primary <- alias_pair[1]
        alias <- alias_pair[2]
        if (!primary %in% names(alias_map)) {
          alias_map[[primary]] <- character(0)
        }
        alias_map[[primary]] <- c(alias_map[[primary]], alias)
      }
    }
  }
  
  # Build list of all possible column names (primary + aliases)
  other_dims_with_aliases <- other_dims
  for (dim_name in other_dims) {
    if (dim_name %in% names(alias_map)) {
      other_dims_with_aliases <- c(other_dims_with_aliases, alias_map[[dim_name]])
    }
  }
  
  # Find which dimension names actually exist in data
  dims_in_data <- intersect(other_dims_with_aliases, names(data))
  
  if (length(dims_in_data) < length(other_dims)) {
    # Some dimensions missing even with aliases
    missing_dims <- setdiff(other_dims, names(data))
    # Check if all have aliases in data
    all_covered <- all(sapply(other_dims, function(d) {
      d %in% names(data) || (d %in% names(alias_map) && any(alias_map[[d]] %in% names(data)))
    }))
    
    if (!all_covered) {
      return(list(
        can_fold = FALSE,
        reason = sprintf("Missing dimension columns in data: %s (no aliases found)", 
                        paste(missing_dims, collapse=", "))
      ))
    }
  }
  
  # Group by all OTHER dimensions, check variance within groups
  # For each group, we check if values are constant (all identical)
  # We do NOT require full coverage - sparse data is fine as long as
  # the values that DO exist are constant within each group
  # Use any_of to handle both primary dimension names and aliases
  variance_check <- data %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(other_dims_with_aliases))) %>%
    dplyr::summarize(
      n_unique = dplyr::n_distinct(value),      # How many distinct values
      value_range = max(value) - min(value),    # Range of values
      .groups = "drop"
    )
  
  # Check if ALL groups have constant values (within tolerance)
  # Handle NA values explicitly
  # NOTE: This is an "all or nothing" approach - if ANY group has variation,
  # the entire dimension is rejected from folding. This is conservative but safe,
  # as we cannot do partial folding (some groups folded, others not) with the
  # current parameter structure.
  is_redundant <- all(variance_check$n_unique == 1, na.rm = TRUE) && 
                  all(variance_check$value_range <= tolerance, na.rm = TRUE)
  
  if (is.na(is_redundant) || !is_redundant) {
    max_range <- max(variance_check$value_range, na.rm = TRUE)
    n_varying_groups <- sum(variance_check$value_range > tolerance, na.rm = TRUE)
    n_total_groups <- nrow(variance_check)
    return(list(
      can_fold = FALSE,
      reason = sprintf("Dimension provides variation in %d/%d groups (range > %.0e, max: %.2e)",
                      n_varying_groups, n_total_groups, tolerance, max_range)
    ))
  }
  
  # Step 5: CRITICAL CHECK for slice dimension with timeframe mappings
  # Even if values are constant, we can only fold if data covers ALL applicable
  # slices for each entity. Otherwise, folding would incorrectly broadcast the
  # value to slices that should use the default.
  #
  # Example: pPeakCost[COAL,R1,2025,PEAK]=100 where COAL uses [PEAK,MID,BASE]
  # - Has data only for PEAK (1/3 slices)
  # - If we fold: pPeakCost[COAL,R1,2025]=100 broadcasts to ALL 3 slices
  # - But MID and BASE should use default (0), not 100!
  # - Therefore: CANNOT FOLD
  if (dim_to_test == "slice" && !is.null(timeframe_mapping) && !is.null(entity_dim)) {
    
    # Build map of dimension names to actual column names in data (handling aliases)
    dim_to_col <- setNames(other_dims, other_dims)
    for (dim_name in other_dims) {
      if (!dim_name %in% names(data)) {
        # Try to find alias
        if (dim_name %in% names(alias_map)) {
          aliases <- alias_map[[dim_name]]
          actual_col <- aliases[aliases %in% names(data)][1]
          if (!is.na(actual_col)) {
            dim_to_col[dim_name] <- actual_col
          }
        }
      }
    }
    
    for (i in seq_len(nrow(variance_check))) {
      # Get entity value - use mapped column name
      entity_col <- dim_to_col[entity_dim]
      entity_value <- variance_check[[entity_col]][i]
      
      # Get expected slices for this entity from the mapping
      expected_slices <- timeframe_mapping[[dim_to_test]][
        timeframe_mapping[[entity_dim]] == entity_value
      ]
      
      if (length(expected_slices) > 0) {
        # Get actual slices in data for this entity combination
        # Use actual column names from data
        filter_expr <- TRUE
        for (dim_name in other_dims) {
          actual_col <- dim_to_col[dim_name]
          if (dim_name == entity_dim) {
            filter_expr <- filter_expr & (data[[actual_col]] == entity_value)
          } else {
            filter_expr <- filter_expr & (data[[actual_col]] == variance_check[[actual_col]][i])
          }
        }
        
        actual_slices <- unique(data[[dim_to_test]][filter_expr])
        
        # MUST have ALL expected slices, not just some
        # If missing any, folding would broadcast to slices that should use default
        missing_slices <- setdiff(expected_slices, actual_slices)
        if (length(missing_slices) > 0) {
          return(list(
            can_fold = FALSE,
            reason = sprintf("Incomplete slice coverage for %s=%s: has %d/%d slices (missing: %s). Folding would incorrectly broadcast to missing slices.",
                            entity_dim, entity_value, 
                            length(actual_slices), length(expected_slices),
                            paste(head(missing_slices, 3), collapse=", "))
          ))
        }
      }
    }
  }
  
  # Dimension is redundant - values are constant within EVERY group
  # AND (for slice) data covers ALL applicable slices for each entity
  # This means the parameter value doesn't actually depend on this dimension,
  # even though it's indexed by it. Safe to fold.
  return(list(
    can_fold = TRUE,
    reason = sprintf("Values constant across dimension in all %d groups (tolerance: %.0e)",
                    nrow(variance_check), tolerance)
  ))
}


#' Get dimension names from dims object
#' Get dimension names from dims object
#'
#' @param dims dims object or character vector
#' @return Character vector of dimension names
#' @keywords internal
get_dim_names <- function(dims) {
  if (is.null(dims)) return(character(0))
  if (is.character(dims)) return(dims)
  
  if (inherits(dims, "dims")) {
    # Use seq_along to avoid "subscript out of bounds" errors with unnamed dims
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


#' Unfold model to restore original dimensions
#'
#' Removes all fold information and restores parameters to their original
#' full-dimensional state.
#'
#' @param model A multimod model object
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return Modified model with fold removed
#' @export
unfold_model <- function(model, verbose = TRUE) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  
  n_unfolded <- 0
  
  for (param_name in names(model$parameters)) {
    param <- model$parameters[[param_name]]
    
    if (!is.null(param$folded_data)) {
      # Clear folded state
      param$active_dims <- param$dims
      param$folded_data <- NULL
      param$misc$fold_info <- NULL
      
      model$parameters[[param_name]] <- param
      n_unfolded <- n_unfolded + 1
      
      if (verbose) {
        cat("✓ Unfolded", param_name, "\n")
      }
    }
  }
  
  # Clear model metadata
  if (!is.null(model$misc)) {
    model$misc$fold_summary <- NULL
    model$misc$folded <- FALSE
    model$misc$fold_options <- NULL
  }
  
  if (verbose) {
    cat(sprintf("\nUnfolded %d parameters\n", n_unfolded))
  }
  
  return(model)
}


#' Validate fold integrity
#'
#' Checks that folded data is consistent with original data and that
#' no information was lost during folding.
#'
#' @param model A multimod model object
#' @param verbose Logical; print validation results (default: TRUE)
#'
#' @return List of validation results, or NULL if all valid
#' @export
validate_fold <- function(model, verbose = TRUE) {
  # Ensure pipe operator is available
  `%>%` <- magrittr::`%>%`
  
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  
  issues <- list()
  n_validated <- 0
  n_valid <- 0
  
  for (param_name in names(model$parameters)) {
    param <- model$parameters[[param_name]]
    
    if (is.null(param$folded_data)) next
    
    n_validated <- n_validated + 1
    
    active_dims <- get_dim_names(param$active_dims)
    
    # Check 1: Consistency within folded groups
    consistency <- param$data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(active_dims))) %>%
      dplyr::summarize(
        n = dplyr::n(),
        n_unique = dplyr::n_distinct(value),
        min_val = min(value),
        max_val = max(value),
        range = max_val - min_val,
        .groups = "drop"
      )
    
    inconsistent <- consistency %>% dplyr::filter(range > 1e-10)
    
    if (nrow(inconsistent) > 0) {
      issues[[param_name]] <- list(
        type = "inconsistent_values",
        message = sprintf("%d groups have inconsistent values", nrow(inconsistent)),
        details = inconsistent
      )
      
      if (verbose) {
        cat("✗", param_name, "- inconsistent values in", nrow(inconsistent), "groups\n")
      }
      next
    }
    
    # Check 2: folded_data matches collapsed original
    recomputed <- param$data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(active_dims))) %>%
      dplyr::summarize(value = dplyr::first(value), .groups = "drop") %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(active_dims)))
    
    folded_sorted <- param$folded_data %>%
      dplyr::select(dplyr::all_of(c(active_dims, "value"))) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(active_dims)))
    
    if (!isTRUE(all.equal(recomputed$value, folded_sorted$value, tolerance = 1e-10))) {
      issues[[param_name]] <- list(
        type = "mismatch",
        message = "folded_data doesn't match collapsed original"
      )
      
      if (verbose) {
        cat("✗", param_name, "- folded_data mismatch\n")
      }
      next
    }
    
    # Check 3: Timestamp freshness (if data was modified)
    # (Could add data modification tracking in future)
    
    n_valid <- n_valid + 1
    if (verbose) {
      cat("✓", param_name, "- valid\n")
    }
  }
  
  if (verbose) {
    cat(sprintf("\n=== Validation Summary ===\n"))
    cat(sprintf("Parameters validated: %d\n", n_validated))
    cat(sprintf("Valid: %d\n", n_valid))
    cat(sprintf("Issues: %d\n", length(issues)))
  }
  
  return(if (length(issues) == 0) NULL else issues)
}


#' Analyze fold opportunities without applying
#'
#' Reports which parameters could be folded and the potential compression.
#'
#' @param model A multimod model object
#' @param fold_slice Logical; consider slice dimension
#' @param fold_slice Logical; consider slice dimension
#' @param fold_region Logical; consider region dimension
#' @param fold_year Logical; consider year dimension
#' @param tolerance Numeric tolerance for value equality (default: 1e-10)
#'
#' @return Data frame with fold analysis
#' @export
analyze_fold_opportunities <- function(model,
                                      fold_slice = TRUE,
                                      fold_region = FALSE,
                                      fold_year = FALSE,
                                      tolerance = 1e-10) {
  
  # Ensure pipe operator is available
  `%>%` <- magrittr::`%>%`
  
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  
  fold_candidates <- c()
  if (fold_slice) fold_candidates <- c(fold_candidates, "slice")
  if (fold_region) fold_candidates <- c(fold_candidates, "region")
  if (fold_year) fold_candidates <- c(fold_candidates, "year")
  
  results <- list()
  
  for (param_name in names(model$parameters)) {
    param <- model$parameters[[param_name]]
    
    if (is.null(param$data) || nrow(param$data) == 0) next
    
    dims <- get_dim_names(param$dims)
    foldable_dims <- intersect(dims, fold_candidates)
    
    if (length(foldable_dims) == 0) next
    if (length(foldable_dims) == length(dims)) next  # Skip all-foldable
    
    dims_to_remove <- c()
    
    for (dim_to_test in foldable_dims) {
      result <- analyze_dimension_redundancy(
        param = param,
        dim_to_test = dim_to_test,
        model = model,
        tolerance = tolerance
      )
      
      if (result$can_fold) {
        dims_to_remove <- c(dims_to_remove, dim_to_test)
      }
    }
    
    if (length(dims_to_remove) > 0) {
      # Use match-based removal to preserve duplicate dimension names
      dim_names <- get_dim_names(dims)
      dims_to_remove_match <- dim_names %in% dims_to_remove
      active_dims <- dims[!dims_to_remove_match]
      
      # Estimate folded size
      folded_size <- param$data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(active_dims))) %>%
        dplyr::summarize(n = dplyr::n(), .groups = "drop") %>%
        nrow()
      
      results[[param_name]] <- data.frame(
        parameter = param_name,
        original_dims = paste(dims, collapse=" × "),
        folded_dims = paste(active_dims, collapse=" × "),
        removed_dims = paste(dims_to_remove, collapse=", "),
        original_rows = nrow(param$data),
        folded_rows = folded_size,
        compression = nrow(param$data) / folded_size,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(results) == 0) {
    return(data.frame(
      parameter = character(0),
      original_dims = character(0),
      folded_dims = character(0),
      removed_dims = character(0),
      original_rows = numeric(0),
      folded_rows = numeric(0),
      compression = numeric(0)
    ))
  }
  
  do.call(rbind, results)
}


#' Fold equations to match folded parameters
#'
#' Scans all equation ASTs and reduces dimensions of folded parameters.
#' Creates model$folded_equations with updated parameter references.
#' Parameter declarations will use active_dims (4D instead of 5D).
#'
#' @param model Model object with folded parameters
#' @param verbose Logical; print progress
#'
#' @return Model with folded_equations slot
#' @keywords internal
fold_equations <- function(model, verbose = TRUE) {
  
  if (verbose) {
    cat("\n=== Folding Equations ===\n")
  }
  
  # Get list of folded parameters and their removed dimensions
  folded_params <- list()
  for (param_name in names(model$parameters)) {
    param <- model$parameters[[param_name]]
    if (!is.null(param$misc$fold_info) && param$misc$fold_info$folded) {
      folded_params[[param_name]] <- param$misc$fold_info$removed_dims
    }
  }
  
  if (length(folded_params) == 0) {
    if (verbose) cat("No folded parameters - skipping equation folding\n")
    return(model)
  }
  
  if (verbose) {
    cat(sprintf("Folded parameters: %d\n", length(folded_params)))
    for (pname in names(folded_params)) {
      cat(sprintf("  %s: removed %s\n", pname, paste(folded_params[[pname]], collapse=", ")))
    }
    cat("\n")
  }
  
  # Clone equations to create folded versions
  # Note: This preserves any $trimmed flags that may exist from prior trim_model() call
  model$folded_equations <- model$equations
  n_updated <- 0
  
  # Process each equation
  for (eq_name in names(model$folded_equations)) {
    eq <- model$folded_equations[[eq_name]]
    
    # Equations have lhs, rhs, not expr
    updated_lhs <- if (!is.null(eq$lhs)) fold_ast_node(eq$lhs, folded_params, model) else NULL
    updated_rhs <- if (!is.null(eq$rhs)) fold_ast_node(eq$rhs, folded_params, model) else NULL
    
    # Check if anything changed
    changed <- FALSE
    if (!identical(updated_lhs, eq$lhs)) {
      eq$lhs <- updated_lhs
      changed <- TRUE
    }
    if (!identical(updated_rhs, eq$rhs)) {
      eq$rhs <- updated_rhs
      changed <- TRUE
    }
    
    if (changed) {
      model$folded_equations[[eq_name]] <- eq
      n_updated <- n_updated + 1
    }
  }
  
  if (verbose) {
    cat(sprintf("Equations scanned: %d\n", length(model$equations)))
    cat(sprintf("Equations with folded params: %d\n", n_updated))
  }
  
  return(model)
}


#' Recursively fold AST nodes
#'
#' Walks AST tree and removes folded dimensions from parameter indexing
#'
#' @param node AST node
#' @param folded_params Named list of parameter names -> removed dimensions
#' @param model Model object (for alias resolution)
#'
#' @return Updated AST node
#' @keywords internal
fold_ast_node <- function(node, folded_params, model = NULL, debug = FALSE) {
  
  if (is.null(node) || !is.list(node)) return(node)
  
  # Handle parameter references - have $name and $dims
  if (inherits(node, "parameter") && !is.null(node$name)) {
    param_name <- node$name
    
    # Check if this parameter was folded
    if (param_name %in% names(folded_params)) {
      removed_dims <- folded_params[[param_name]]
      
      if (debug) {
        cat(sprintf("  Processing %s: removing %s from %d dims\n", 
                    param_name, paste(removed_dims, collapse=","), length(node$dims)))
      }
      
      # Remove folded dimensions from dims list
      if (!is.null(node$dims) && length(node$dims) > 0) {
        
        # Build list of dims to keep
        keep_dims <- list()
        
        for (i in seq_along(node$dims)) {
          dim <- node$dims[[i]]
          # Get dim name
          dim_name <- if (inherits(dim, "set") && !is.null(dim$name)) {
            dim$name
          } else if (inherits(dim, "symbol") && !is.null(dim$name)) {
            dim$name
          } else if (is.list(dim) && !is.null(dim$name)) {
            dim$name
          } else {
            NA_character_
          }
          
          # Resolve aliases if model is available
          resolved_name <- dim_name
          if (!is.na(dim_name) && !is.null(model)) {
            # Try model$aliases (list of character vectors)
            if (!is.null(model$aliases)) {
              for (alias_group in model$aliases) {
                if (dim_name %in% alias_group) {
                  # Use the first name in the group as the canonical name
                  resolved_name <- alias_group[1]
                  break
                }
              }
            }
            # Also try index_aliases (named character vector) as fallback
            else if (!is.null(model$index_aliases) && dim_name %in% names(model$index_aliases)) {
              resolved_name <- model$index_aliases[[dim_name]]
            }
          }
          
          # Keep dim if resolved name is NOT in removed_dims
          if (!is.na(resolved_name) && !resolved_name %in% removed_dims) {
            keep_dims <- c(keep_dims, list(dim))
          }
          
          if (debug && !is.na(dim_name)) {
            if (dim_name != resolved_name) {
              cat(sprintf("    Dim %s -> %s: %s\n", 
                          dim_name, resolved_name, 
                          if (resolved_name %in% removed_dims) "REMOVED" else "kept"))
            }
          }
        }
        
        # Update dims and preserve class
        node$dims <- structure(keep_dims, class = class(node$dims))
      }
    }
    
    return(node)
  }
  
  # Recursively process children for expression nodes
  if (inherits(node, "expression")) {
    if (!is.null(node$lhs)) {
      node$lhs <- fold_ast_node(node$lhs, folded_params, model, debug)
    }
    if (!is.null(node$rhs)) {
      node$rhs <- fold_ast_node(node$rhs, folded_params, model, debug)
    }
    if (!is.null(node$left)) {
      node$left <- fold_ast_node(node$left, folded_params, model, debug)
    }
    if (!is.null(node$right)) {
      node$right <- fold_ast_node(node$right, folded_params, model, debug)
    }
    if (!is.null(node$args)) {
      node$args <- lapply(node$args, function(arg) fold_ast_node(arg, folded_params, model, debug))
    }
  }
  
  # Handle function nodes (like prod, sum)
  if (inherits(node, "func")) {
    if (!is.null(node$value)) {
      node$value <- fold_ast_node(node$value, folded_params, model, debug)
    }
    if (!is.null(node$body)) {
      node$body <- fold_ast_node(node$body, folded_params, model, debug)
    }
    # Handle index field (contains when nodes with conditions for sum/prod)
    if (!is.null(node$index)) {
      node$index <- fold_ast_node(node$index, folded_params, model, debug)
    }
  }
  
  # Handle summation/product nodes
  if (!is.null(node$body) && !inherits(node, "func")) {
    node$body <- fold_ast_node(node$body, folded_params, model, debug)
  }
  
  # Handle conditional expressions  
  if (!is.null(node$condition)) {
    node$condition <- fold_ast_node(node$condition, folded_params, model, debug)
  }
  if (!is.null(node$true_expr)) {
    node$true_expr <- fold_ast_node(node$true_expr, folded_params, model, debug)
  }
  if (!is.null(node$false_expr)) {
    node$false_expr <- fold_ast_node(node$false_expr, folded_params, model, debug)
  }
  
  # Handle 'when' nodes (conditional expressions)
  if (inherits(node, "when")) {
    if (!is.null(node$then)) {
      node$then <- fold_ast_node(node$then, folded_params, model, debug)
    }
    if (!is.null(node$otherwise)) {
      node$otherwise <- fold_ast_node(node$otherwise, folded_params, model, debug)
    }
  }
  
  return(node)
}
