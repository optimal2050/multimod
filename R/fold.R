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
#' m <- example_models$energyRt$multimod
#' param <- m$parameters$pTechCinp2use
#'
#' # Active dimensions: the folded ones if folded, else the declared ones
#' get_active_dims(param)
#'
#' # Folded dimensions only
#' get_active_dims(param, folded = TRUE)
#'
#' @export
get_active_dims <- function(param, folded = FALSE) {
  stopifnot(inherits(param, "mm_parameter"))
  
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
#' @param verbose Logical; if `TRUE` and `format = "text"`, print a note when
#'   the model has no folding applied (default: FALSE)
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
#' m <- example_models$energyRt$multimod
#' spec <- create_fold_spec(m, fold_dims = "timeslice", verbose = FALSE)
#' m_folded <- fold_model(m, fold_spec = spec, verbose = FALSE)
#'
#' # Structured list
#' stats <- get_fold_summary(m_folded)
#'
#' # As a data frame
#' df <- get_fold_summary(m_folded, format = "data.frame")
#'
#' # Formatted text
#' get_fold_summary(m_folded, format = "text")
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
      cat(sprintf("%-25s %20s %20s %6d\u2192%-6d %10s\n",
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
#' m <- example_models$energyRt$multimod
#' is_folded(m)
#' is_folded(m$parameters$pTechCinp2use)
#'
#' spec <- create_fold_spec(m, fold_dims = "timeslice", verbose = FALSE)
#' m_folded <- fold_model(m, fold_spec = spec, verbose = FALSE)
#' is_folded(m_folded)
#'
#' @export
is_folded <- function(x) {
  UseMethod("is_folded")
}

#' @export
is_folded.mm_parameter <- function(x) {
  # A parameter is considered folded if it has fold_info indicating folding occurred
  !is.null(x$misc$fold_info) && isTRUE(x$misc$fold_info$folded)
}

#' @export
is_folded.mm_model <- function(x) {
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
  is_folded.mm_model(x)
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

#' Create a fold specification for a model's parameters
#'
#' Scans a model's parameters and reports, for each, whether a dimension can be
#' folded away - that is, whether the parameter's values are constant along it
#' within tolerance - together with the mapping that should be used to validate
#' the fold.
#'
#' Nothing here is energyRt-specific: the function walks `model$parameters` and
#' the `fold_dims` configuration it is given. Only the *default* mapping names
#' (`mTechTimeslice` and friends) follow energyRt's conventions, and those are
#' supplied by the caller.
#'
#' @param model A multimod model object.
#' @param fold_dims Dimension configuration. Either:
#'   - a character vector of dimension names (e.g. `c("timeslice")`);
#'   - a named list mapping dimension to validation mapping
#'     (e.g. `list(timeslice = "mCommTimeslice")`);
#'   - a named list mapping dimension to entity-specific mappings
#'     (e.g. `list(timeslice = list(tech = "mTechTimeslice",
#'     stg = c("mStorageComm", "mCommTimeslice")))`), where a character vector
#'     is a chain.
#' @param tolerance Numeric tolerance for detecting constant values.
#' @param verbose Logical; print progress.
#'
#' @return A data frame with one row per parameter/dimension pair: `param_name`,
#'   `dim_to_fold`, `validation_mapping`, `validation_mapping_raw`, `can_fold`,
#'   `unproven` and `reason`. `unproven` marks a dimension refused for want of
#'   a validation mapping rather than because folding was shown to be wrong:
#'   constancy holds over the data present, but nothing proves the data covers
#'   every applicable member, so folding could broadcast a value where the
#'   default applies. Supplying the mapping settles it either way.
#'
#' @details
#' With an entity-specific configuration, the **first** matching entity
#' dimension wins: `tech`, then `sup`, then `comm`, then `stg`; a parameter
#' matching none gets `NA` and is not validated.
#'
#' A character vector is a **mapping chain**, joined in sequence on the columns
#' the mappings share. For `stg = c("mStorageComm", "mCommTimeslice")`:
#' `mStorageComm[stg, comm]` joined with `mCommTimeslice[comm, timeslice]`
#' yields `[stg, timeslice]`.
#'
#' @seealso [fold_model()]
#' @export
#' @examples
#' m <- example_models$energyRt$multimod
#' fold_spec <- create_fold_spec(m, fold_dims = "timeslice")
#' head(fold_spec)
#'
#' # Entity-specific validation mappings (a character vector is a chain)
#' fold_spec <- create_fold_spec(
#'   m,
#'   fold_dims = list(
#'     timeslice = list(comm = "mCommTimeslice")
#'   ),
#'   verbose = FALSE
#' )
create_fold_spec <- function(model,
                             fold_dims,
                             tolerance = 1e-10,
                             verbose = TRUE) {

  `%>%` <- magrittr::`%>%`
  
  # Parse fold_dims configuration
  if (is.list(fold_dims)) {
    # Explicit configuration: list(timeslice = list(tech = "mTechTimeslice", stg = c("mStorageComm", "mCommTimeslice")))
    dim_entity_mapping_config <- fold_dims
    fold_dims_vec <- names(fold_dims)
  } else {
    # Simple vector: c("timeslice", "region") - but no default mappings, will return NA
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
          validation_mapping_display <- paste(mapping_spec, collapse = " \u2192 ")
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
        unproven = isTRUE(result$unproven),
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
    nu <- sum(spec_df$unproven %in% TRUE)
    if (nu > 0) {
      cat("  of which unproven -- a validation mapping would allow them:", nu, "
")
    }
    cat("\nREVIEW CAREFULLY: validation_mapping may be incorrect for non-standard models\n\n")
  }
  
  return(spec_df)
}


#' Decide whether to show a progress bar while folding
#'
#' @param model A multimod model object.
#' @param fold_spec The fold specification.
#' @param param_threshold Parameter count above which to enable progress.
#' @param row_threshold Total data rows above which to enable progress.
#' @return Logical.
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

#' Fold model dimensions to reduce data size
#'
#' Reduces redundant dimensions in parameters where data is constant across
#' those dimensions. Only folds high-cardinality indexing dimensions (timeslice,
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
#' @param .progress Progress bar control: `"auto"` (default) shows a
#'   `progressr` bar for large models, `TRUE` forces it on (requires
#'   `progressr`), `FALSE` disables it.
#'
#' @return Modified model with folded parameters
#' 
#' @details
#' Folding analyzes each parameter to identify dimensions that don't provide
#' variation in the data. For each parameter, it groups by all OTHER dimensions
#' and checks if values are constant within each group (within tolerance).
#' 
#' Example: For \code{pStorageInpEff[stg, comm, region, year, timeslice]} testing timeslice:
#' - Groups by \code{(stg, comm, region, year)}
#' - For each group, checks if ALL timeslice values are identical
#' - Only folds if EVERY group has constant values
#' 
#' This handles sparse data correctly - if a parameter only has values for
#' peak hours (1 timeslice out of 17), that's fine. It only checks if the values
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
#' m <- example_models$energyRt$multimod
#' fold_spec <- create_fold_spec(m, fold_dims = "timeslice", verbose = FALSE)
#' # Review and edit fold_spec to ensure correct validation mappings
#' m_folded <- fold_model(m, fold_spec = fold_spec, verbose = FALSE)
#' get_fold_summary(m_folded, format = "text")
#' 
#' @export
fold_model <- function(model, 
                       fold_spec,
                       tolerance = 1e-10,
                       verbose = TRUE,
                       .progress = "auto") {
  `%>%` <- magrittr::`%>%`
  
  stopifnot(inherits(model, "multimod") || inherits(model, "mm_model"))
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
#' @return The folded `mm_model`.
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
        validation_mapping_display <- paste(validation_mapping, collapse = " \u2192 ")
      } else {
        validation_mapping_display <- validation_mapping
      }
    }
    
    if (!param_name %in% names(model$parameters)) {
      if (verbose) cat("\u22a0 Skipping", param_name, "- not found in model\n")
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
      if (verbose) cat("\u22a0 Skipping", param_name, "- doesn't have dimension", dim_to_fold, "\n")
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
      # Which ENCODING of the fold this is. energyRt records the same field and
      # can write either "wildcard" (the dimension column is kept and the
      # folded rows carry NA, exported later as ANY* tokens) or "drop". Both
      # packages compute the same whole-column fold; they serialise it
      # differently, and a wildcard has to be expanded before it reaches a
      # matrix while a dropped column does not. multimod only ever drops, but
      # it has to SAY so: without the field, absence is ambiguous between
      # "multimod wrote this" and "an energyRt predating the field wrote it",
      # and a reader cannot tell whether expansion is required.
      mode = "drop",
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
      original = paste(original_dims, collapse=" \u00d7 "),
      folded = paste(active_dims, collapse=" \u00d7 "),
      removed = dim_to_fold,
      original_rows = nrow(param$data),
      folded_rows = nrow(folded_data),
      compression = sprintf("%.1fx", nrow(param$data) / nrow(folded_data))
    )
    
    n_folded <- n_folded + 1
    
    if (verbose) {
      cat(sprintf("\u2713 %s: %dD \u2192 %dD (%d \u2192 %d rows, %.1fx)\n",
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
#'   (e.g., "mTechTimeslice"). If NULL, auto-detects based on parameter dimensions.
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
  
  # Step 2b: refuse a dimension the parameter carries more than once.
  # `fold_ast_node()` and the declaration rewrite both drop EVERY dim whose
  # resolved name matches, so folding `region` on pTradeIrEff(trade, region,
  # region, year, timeslice) would collapse source and destination into one
  # subscript - a model that still assembles, with the wrong contents. 18 of
  # the energyRt parameters carry such a repeat (comm,comm / region,region /
  # timeslice,timeslice). Folding one position only is possible but needs the
  # analysis to prove constancy per position, which it does not do.
  resolved_dims <- vapply(dims, resolve_alias_to_set, character(1), model = model)
  n_occur <- sum(resolved_dims == resolve_alias_to_set(dim_to_test, model))
  if (n_occur > 1L) {
    return(list(
      can_fold = FALSE,
      reason = sprintf(
        "Dimension appears %d times in the parameter (%s); folding would remove all of them",
        n_occur, paste(dims, collapse = ", "))
    ))
  }
  
  # Step 3: For timeslice dimension, get timeframe mapping for coverage validation
  # This is CRITICAL: we need to verify that data covers ALL applicable slices
  # for each entity. Otherwise folding would broadcast values to slices that
  # should use the default value.
  #
  # The validation_mapping parameter can be:
  # - Single string: direct mapping name (e.g., "mTechTimeslice")
  # - Character vector: chain of mappings to join (e.g., c("mStorageComm", "mCommTimeslice"))
  timeframe_mapping <- NULL
  entity_dim <- NULL
  
  if (dim_to_test == "timeslice") {
    if (!is.null(validation_mapping) && length(validation_mapping) > 0 && !is.na(validation_mapping[1])) {
      
      if (length(validation_mapping) == 1) {
        # Direct mapping
        mapping_name <- validation_mapping
        
        if (mapping_name %in% names(model$mappings)) {
          timeframe_mapping <- model$mappings[[mapping_name]]$data
          
          # The entity is whichever column of the mapping is not the folded
          # dimension. Derived rather than looked up in a table, so mImpTimeslice,
          # mExpTimeslice, mTradeTimeslice and the region/year mappings work too;
          # a name absent from the table used to yield NA and crash the coverage
          # check below.
          entity_dim <- setdiff(names(timeframe_mapping), dim_to_test)[1]
          
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
        # Example: c("mStorageComm", "mCommTimeslice") 
        # mStorageComm[stg, comm] join mCommTimeslice[comm, timeslice] → [stg, timeslice]
        
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
  
  # Step 5: CRITICAL CHECK for timeslice dimension with timeframe mappings
  # Even if values are constant, we can only fold if data covers ALL applicable
  # slices for each entity. Otherwise, folding would incorrectly broadcast the
  # value to slices that should use the default.
  #
  # Example: pPeakCost[COAL,R1,2025,PEAK]=100 where COAL uses [PEAK,MID,BASE]
  # - Has data only for PEAK (1/3 slices)
  # - If we fold: pPeakCost[COAL,R1,2025]=100 broadcasts to ALL 3 slices
  # - But MID and BASE should use default (0), not 100!
  # - Therefore: CANNOT FOLD
  if (!is.null(timeframe_mapping) && !is.null(entity_dim) &&
      !is.na(entity_dim) && entity_dim %in% other_dims) {
    
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
            reason = sprintf("Incomplete timeslice coverage for %s=%s: has %d/%d slices (missing: %s). Folding would incorrectly broadcast to missing slices.",
                            entity_dim, entity_value, 
                            length(actual_slices), length(expected_slices),
                            paste(head(missing_slices, 3), collapse=", "))
          ))
        }
      }
    }
  }
  
  # Step 5b: the broadcast hazard above is not specific to timeslice, but
  # without a mapping there is nothing to say which members a group SHOULD
  # carry, so per-group gaps cannot be judged - a supply process legitimately
  # priced in three of seven regions is not a defect. What is always wrong is
  # a parameter that never indexes some member of the folded dimension at
  # all: those lookups resolve to the default today, and folding would
  # replace it with the stored value.
  #
  # pTechStockNew (exogenous capacity commissioned at a milestone) holds one
  # row per tech-region, in 2020 only, with defVal 0. Its values are
  # trivially constant along year, so Step 4 passes - but folding made the
  # capacity arrive in all four years and scaled the whole solution by 2.8.
  if (is.null(timeframe_mapping)) {
    dim_col <- intersect(c(dim_to_test, alias_map[[dim_to_test]]), names(data))
    members <- model$sets[[dim_to_test]]$data
    if (length(dim_col) > 0 && !is.null(members) && length(members) > 1) {
      present <- unique(as.character(data[[dim_col[1]]]))
      absent <- setdiff(as.character(members), present)
      if (length(absent) > 0) {
        # The values do not vary, so this is a fold CANDIDATE; what is missing
        # is evidence that the uncovered members are never queried. Reported
        # apart from a genuine conflict so an analysis can list what a mapping
        # would unlock, while fold_model() still declines to apply it.
        return(list(
          can_fold = FALSE,
          unproven = TRUE,
          reason = sprintf(paste0("Values are constant, but the parameter never ",
                                  "indexes %d of %d %s members (%s), which resolve ",
                                  "to the default. Supply a validation mapping for ",
                                  "this dimension to confirm they are unreachable."),
                           length(absent), length(members), dim_to_test,
                           paste(utils::head(absent, 3), collapse = ", "))
        ))
      }
    }
  }

  # Dimension is redundant - values are constant within EVERY group, and the
  # data covers every member of it, so no default is being overwritten.
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
      } else if (inherits(d, "mm_set")) {
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
  stopifnot(inherits(model, "multimod") || inherits(model, "mm_model"))
  
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
        cat("\u2713 Unfolded", param_name, "\n")
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
  
  stopifnot(inherits(model, "multimod") || inherits(model, "mm_model"))
  
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
        cat("\u2717", param_name, "- inconsistent values in", nrow(inconsistent), "groups\n")
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
        cat("\u2717", param_name, "- folded_data mismatch\n")
      }
      next
    }
    
    # Check 3: Timestamp freshness (if data was modified)
    # (Could add data modification tracking in future)
    
    n_valid <- n_valid + 1
    if (verbose) {
      cat("\u2713", param_name, "- valid\n")
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
#' @param fold_timeslice Logical; consider the timeslice dimension
#' @param fold_region Logical; consider region dimension
#' @param fold_year Logical; consider year dimension
#' @param tolerance Numeric tolerance for value equality (default: 1e-10)
#'
#' @return Data frame with fold analysis
#' @export
analyze_fold_opportunities <- function(model,
                                      fold_timeslice = TRUE,
                                      fold_region = FALSE,
                                      fold_year = FALSE,
                                      tolerance = 1e-10) {
  
  # Ensure pipe operator is available
  `%>%` <- magrittr::`%>%`
  
  stopifnot(inherits(model, "multimod") || inherits(model, "mm_model"))
  
  fold_candidates <- c()
  if (fold_timeslice) fold_candidates <- c(fold_candidates, "timeslice")
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
        original_dims = paste(dims, collapse=" \u00d7 "),
        folded_dims = paste(active_dims, collapse=" \u00d7 "),
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
  if (inherits(node, "mm_parameter") && !is.null(node$name)) {
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
          dim_name <- if (inherits(dim, "mm_set") && !is.null(dim$name)) {
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
  
  # A unary node (-x, not x) holds its operand in `rhs` and its class is
  # c("unary", "ast") - it does NOT inherit "expression", so the branch above
  # does not reach it. Measured on the energyRt model: the subtrees under the
  # 2 unary nodes held 4 parameter references the rewriter never visited, so a
  # folded parameter kept its original arity there - a GMPL error, and a silent
  # default substitution on JuMP and Pyomo.
  if (inherits(node, "unary") && !is.null(node$rhs)) {
    node$rhs <- fold_ast_node(node$rhs, folded_params, model, debug)
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
