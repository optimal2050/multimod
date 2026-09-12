#' Extract energyRt domain mappings from GAMS file comments
#'
#' Reads *@ domain mapping hints from the GAMS source file and applies them
#' to variables in a model_structure object. This enables sparse indexing
#' in Julia/JuMP export.
#'
#' @param model A model_structure object (from read_gams())
#' @param verbose Print progress messages
#' @return Modified model_structure with domain field populated for variables
#' @export
#'
#' @examples
#' \dontrun{
#' model <- read_gams("energyRt.gms")
#' model <- en_extract_domains_from_comments(model)
#' }
en_extract_domains_from_comments <- function(model, verbose = FALSE) {
  if (!inherits(model, "model_structure")) {
    stop("model must be a model_structure object from read_gams()")
  }

  # Get source file path
  if (is.null(model$source) || !file.exists(model$source)) {
    warning("Cannot extract domain hints: source file not found")
    return(model)
  }

  if (verbose) {
    cat("Extracting domain hints from:", model$source, "\n")
  }

  # Extract hints from source file
  domain_hints <- en_extract_gams_domain_hints(model$source)

  if (verbose) {
    cat(sprintf("Found %d domain hints\n", length(domain_hints)))
  }

  # Apply hints to model
  model <- en_apply_domain_hints(model, domain_hints, verbose = verbose)

  return(model)
}


#' Extract domain mapping hints from GAMS file comments
#'
#' Reads a GAMS file and extracts *@ domain mapping comments that appear
#' before variable declarations. Does NOT modify the model structure.
#'
#' @param file_or_text Path to GAMS file or text content
#' @return Named list mapping variable names to domain mapping comments
#' @keywords internal
en_extract_gams_domain_hints <- function(file_or_text) {
  # Read the file
  if (length(file_or_text) == 1 && file.exists(file_or_text)) {
    lines <- readLines(file_or_text, warn = FALSE)
  } else {
    lines <- file_or_text
  }

  # Remove $ontext/$offtext blocks
  lines <- remove_ontext_offtext(lines)

  domain_hints <- list()
  pending_hint <- NULL
  in_variables <- FALSE

  for (i in seq_along(lines)) {
    line <- trimws(lines[i])

    # Skip blank lines
    if (line == "") next

    # Detect variable declaration block
    if (grepl("^(positive\\s+)?variable(s)?\\b", line, ignore.case = TRUE)) {
      in_variables <- TRUE
      next
    }

    # End of variable block
    if (in_variables && grepl("^;", line)) {
      in_variables <- FALSE
      pending_hint <- NULL
      next
    }

    # In variable block: capture *@ comments
    if (in_variables && grepl("^\\*@", line)) {
      pending_hint <- line
      next
    }

    # In variable block: parse variable declaration
    if (in_variables && grepl("^[a-zA-Z]", line)) {
      # Extract variable name
      var_match <- regexec("^([a-zA-Z0-9_]+)\\s*\\(", line)
      var_result <- regmatches(line, var_match)[[1]]

      if (length(var_result) >= 2) {
        var_name <- var_result[2]

        # Store the hint if we have one
        if (!is.null(pending_hint)) {
          domain_hints[[var_name]] <- pending_hint
        }
      }

      # Clear the pending hint after processing
      pending_hint <- NULL
    }
  }

  return(domain_hints)
}


#' Apply domain hints to a model_structure object
#'
#' Takes domain hints extracted from GAMS comments and applies them
#' to variables in a model_structure object.
#'
#' @param model A model_structure object from read_gams()
#' @param domain_hints Named list from en_extract_gams_domain_hints()
#' @param verbose Print progress messages
#' @return Modified model_structure with domain field populated
#' @keywords internal
en_apply_domain_hints <- function(model, domain_hints, verbose = FALSE) {
  if (!inherits(model, "model_structure")) {
    stop("model must be a model_structure object from read_gams()")
  }

  vars_updated <- 0

  for (var_name in names(model$variables)) {
    if (var_name %in% names(domain_hints)) {
      hint <- domain_hints[[var_name]]

      # Parse the hint using existing function
      parsed <- en_parse_domain_hint(hint, model$variables[[var_name]]$dims)

      if (!is.null(parsed)) {
        model$variables[[var_name]]$domain <- parsed
        model$variables[[var_name]]$comment <- hint
        vars_updated <- vars_updated + 1

        if (verbose) {
          cat(sprintf("  %s -> %s\n", var_name, parsed))
        }
      }
    }
  }

  if (verbose) {
    cat(sprintf("\nApplied domain hints to %d / %d variables\n",
                vars_updated, length(model$variables)))
  }

  return(model)
}


#' Parse a single *@ domain hint comment
#'
#' @param hint Comment line like "*@ mTechNew(tech,region,year)"
#' @param dims Variable dimensions from declaration
#' @return Domain mapping name or NULL
#' @keywords internal
en_parse_domain_hint <- function(hint, dims) {
  # Remove *@ prefix and whitespace
  hint <- sub("^\\*@\\s*", "", hint)
  hint <- trimws(hint)

  # Empty hint means unused variable
  if (hint == "") {
    return(character(0))
  }

  # Extract mapping name
  if (grepl("^([a-zA-Z0-9_]+)\\s*\\(", hint)) {
    mapping_match <- regexec("^([a-zA-Z0-9_]+)\\s*\\(([^)]*)\\)", hint)
    result <- regmatches(hint, mapping_match)[[1]]

    if (length(result) >= 3) {
      mapping_name <- result[2]
      hint_dims <- trimws(unlist(strsplit(result[3], ",")))

      # Validate dimensions match
      if (length(dims) != length(hint_dims)) {
        warning(sprintf("Dimension mismatch for mapping %s: expected %d, got %d",
                        mapping_name, length(dims), length(hint_dims)))
      }

      return(mapping_name)
    }
  }

  return(NULL)
}

#' Get default values from energyRt modInp
#'
#' @return Named list of parameter default values
#' @export
get_energyrt_defvals <- function() {
  # Access internal .modInp object from energyRt package
  modinp <- energyRt:::.modInp

  # Extract defVal for each parameter
  defvals <- list()
  for (param_name in names(modinp)) {
    param <- modinp[[param_name]]
    if (!is.null(param$defVal)) {
      defvals[[param_name]] <- param$defVal
    }
  }

  defvals
}

#' Update model parameters with default values from energyRt
#'
#' @param model multimod model object
#' @return Updated model with defVal populated
#' @export
populate_defvals_from_energyrt <- function(model) {
  defvals <- get_energyrt_defvals()
  defvals <- defvals[grepl("^p", names(defvals))] # parameters only

  if (length(defvals) == 0) {
    warning("No default values found in modInp")
    return(model)
  }

  # Update parameters
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    updated_count <- 0

    for (param_name in names(defvals)) {
      dv <- defvals[[param_name]]

      # Handle parameters with two values (lower and upper bounds)
      if (length(dv) == 2) {
        # Assign first value to *Lo parameter
        lo_name <- paste0(param_name, "Lo")
        if (lo_name %in% names(model$parameters)) {
          model$parameters[[lo_name]]$defVal <- dv[1]
          updated_count <- updated_count + 1
        }

        # Assign second value to *Up parameter
        up_name <- paste0(param_name, "Up")
        if (up_name %in% names(model$parameters)) {
          model$parameters[[up_name]]$defVal <- dv[2]
          updated_count <- updated_count + 1
        }
      } else {
        # Single value - assign directly if parameter exists
        if (param_name %in% names(model$parameters)) {
          model$parameters[[param_name]]$defVal <- dv
          updated_count <- updated_count + 1
        }
      }
    }

    message("Populated default values for ", updated_count,
            " parameters from energyRt modInp")
  }

  model
}

#' Link energyRt scenario data to model
#'
#' Links data from an energyRt scenario to a multimod model structure.
#' The data remains on disk (lazy loading) - only references are stored.
#'
#' @param model A multimod model object
#' @param scenario An energyRt scenario object with modInp@@parameters
#' @param inMemory Logical. Load parameter data into memory instead of
#'   referencing on-disk Arrow files?
#' @return The model with linked data references
#' @export
link_scenario_data <- function(model, scenario, inMemory = FALSE) {
  param_names <- names(scenario@modInp@parameters)
  n_params <- 0
  n_mappings <- 0

  for (pname in param_names) {
    ert_param <- scenario@modInp@parameters[[pname]]

    # Use @type to distinguish mappings from parameters
    param_type <- as.character(ert_param@type)

    if (param_type == "map") {
      # Mapping (including subsets)
      if (pname %in% names(model$mappings)) {
        model$mappings[[pname]] <- convert_energyrt_parameter(
          ert_param,
          model$mappings[[pname]],
          inMemory = inMemory
        )
        n_mappings <- n_mappings + 1
      }
    } else if (param_type == "numpar") {
      # Numeric parameter
      if (pname %in% names(model$parameters)) {
        model$parameters[[pname]] <- convert_energyrt_parameter(
          ert_param,
          model$parameters[[pname]],
          inMemory = inMemory
        )
        n_params <- n_params + 1
      }
    }
  }

  cat("  Mappings linked: ", n_mappings, "\n")
  cat("  Parameters linked:", n_params, "\n")

  model
}

#' Convert energyRt parameter to multimod format
#'
#' @param ert_param An energyRt parameter object
#' @param orig_param Original multimod parameter structure (optional)
#' @param inMemory Logical. Embed data in-memory?
#' @return Updated parameter with data reference
#' @keywords internal
convert_energyrt_parameter <- function(ert_param, orig_param = NULL, inMemory = FALSE,
                                       scenario = NULL) {
  # Start with original parameter structure if available
  result <- if (!is.null(orig_param)) {
    orig_param
  } else {
    list(
      name = ert_param@name,
      dims = ert_param@dimSets
    )
  }

  # Add default value if available
  if (!is.null(ert_param@defVal)) {
    result$defVal <- ert_param@defVal
  }

  param_misc <- ert_param@misc
  source_path <- if (!is.null(param_misc$path)) param_misc$path else NULL
  source_on_disk <- if (!is.null(param_misc$onDisk)) param_misc$onDisk else NULL
  source_in_memory <- if (!is.null(param_misc$inMemory)) param_misc$inMemory else FALSE
  result$data <- data.frame()
  result$misc <- list(
    inMemory = source_in_memory,
    path = source_path,
    onDisk = source_on_disk
  )

  if (isTRUE(inMemory)) {
    loaded <- collect_scenario_parameter_data(ert_param, scenario = scenario)
    result$data <- loaded
    result$misc$inMemory <- TRUE
    result$misc$path <- NULL
  }

  # Preserve class if original had one
  if (!is.null(orig_param) && !is.null(class(orig_param))) {
    class(result) <- class(orig_param)
  }

  result
}

