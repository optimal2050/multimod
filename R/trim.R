#' Trim unused elements from a model
#'
#' Removes "dead branches" from a model by marking unused sets, parameters,
#' mappings, variables, and equations with `$trimmed = TRUE`. Code generators
#' (e.g., write_jump, write_gmpl) will skip elements marked as trimmed.
#'
#' @param model A multimod model object
#' @param strategy Character. Trimming strategy:
#'   - "unused": Remove empty data and dependent variables/equations (default)
#'   - "aggressive": Also perform graph-based analysis (future)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return Model with `$trimmed = TRUE` flag on unused elements
#'
#' @details
#' Trimming process:
#' 1. Mark sets, parameters, and mappings with empty data
#' 2. Mark variables with empty domains
#' 3. Mark equations with empty domains or all variables trimmed
#' 4. Untrim any elements still needed by non-trimmed equations
#' 5. Final pass: mark variables not appearing in any non-trimmed equation
#'
#' Unlike folding (which reduces dimensions), trimming removes entire unused
#' elements. Original data is preserved on disk but marked for exclusion from
#' solver code generation.
#'
#' @examples
#' \dontrun{
#' # Typical workflow
#' model <- fold_model(model)  # Reduce dimensions first
#' model <- trim_model(model)  # Then remove dead branches
#' write_jump(model, "model.jl")  # Trimmed elements excluded
#' 
#' # Check what was trimmed
#' summary <- get_trim_summary(model)
#' print(summary, format = "text")
#' }
#'
#' @keywords internal
should_enable_progress_trim <- function(model, 
                                        element_threshold = 100,
                                        row_threshold = 50000) {
  if (!requireNamespace("progressr", quietly = TRUE)) {
    return(FALSE)
  }
  
  # Count total model elements
  n_elements <- length(model$sets) + length(model$parameters) + 
                length(model$mappings) + length(model$variables) + 
                length(model$equations)
  
  if (n_elements > element_threshold) return(TRUE)
  
  # Check total data size
  total_rows <- sum(vapply(model$parameters, function(p) {
    if (!is.null(p$data)) nrow(p$data) else 0L
  }, integer(1)))
  
  return(total_rows > row_threshold)
}

#' @export
trim_model <- function(model, strategy = "unused", verbose = TRUE, .progress = "auto") {
  stopifnot(inherits(model, c("model", "multimod", "model_structure")))
  stopifnot(strategy %in% c("unused", "aggressive"))
  
  # Determine if progress should be enabled
  use_progress <- FALSE
  if (.progress == "auto") {
    use_progress <- should_enable_progress_trim(model)
  } else if (isTRUE(.progress)) {
    use_progress <- requireNamespace("progressr", quietly = TRUE)
  }
  
  if (use_progress) {
    progressr::with_progress({
      # 4 main phases
      p <- progressr::progressor(steps = 4)
      
      if (verbose) message("Starting model trimming...")
      
      # Phase 1: Mark empty data
      p("Phase 1: Identifying empty data")
      if (verbose) message("  Phase 1: Identifying empty sets, parameters, and mappings...")
      model <- mark_empty_data(model, verbose = verbose)
      
      # Phase 2: Mark variables with empty domains
      p("Phase 2: Identifying empty variables")
      if (verbose) message("  Phase 2: Identifying variables with empty domains...")
      model <- mark_empty_variables(model, verbose = verbose)
      
      # Phase 3: Mark equations with empty domains or variables
      p("Phase 3: Identifying empty equations")
      if (verbose) message("  Phase 3: Identifying equations with empty domains...")
      model <- mark_empty_equations(model, verbose = verbose)
      
      # Phase 4: Untrim required elements
      p("Phase 4: Verifying required elements")
      if (verbose) message("  Phase 4: Verifying required elements...")
      model <- untrim_required_elements(model, verbose = verbose)
      
      model
    })
  } else {
    if (verbose) message("Starting model trimming...")
    
    # Phase 1: Mark empty data
    if (verbose) message("  Phase 1: Identifying empty sets, parameters, and mappings...")
    model <- mark_empty_data(model, verbose = verbose)
    
    # Phase 2: Mark variables with empty domains
    if (verbose) message("  Phase 2: Identifying variables with empty domains...")
    model <- mark_empty_variables(model, verbose = verbose)
    
    # Phase 3: Mark equations with empty domains or variables
    if (verbose) message("  Phase 3: Identifying equations with empty domains...")
    model <- mark_empty_equations(model, verbose = verbose)
    
    # Phase 4: Untrim required elements
    if (verbose) message("  Phase 4: Verifying required elements...")
    model <- untrim_required_elements(model, verbose = verbose)
  }
  
  # Sync trimmed flags to folded structures if they exist
  # This ensures trim_model() works correctly regardless of whether
  # it's called before or after fold_model()
  if (!is.null(model$folded_equations) && length(model$folded_equations) > 0) {
    for (eq_name in names(model$equations)) {
      if (!is.null(model$folded_equations[[eq_name]])) {
        # Copy trim flag from original to folded equation
        model$folded_equations[[eq_name]]$trimmed <- 
          isTRUE(model$equations[[eq_name]]$trimmed)
      }
    }
    if (verbose) {
      message("  Synced trim flags to folded equations")
    }
  }
  
  # Note: Variables and parameters don't need syncing because folding
  # doesn't create separate folded_variables or folded_parameters lists
  
  # Collect NET trim statistics (after Phase 4 untrimming)
  model$misc$trim_summary <- collect_trim_stats(model)
  
  # Calculate total NET trimmed elements
  net_trimmed_counts <- c(
    sets = if (!is.null(model$misc$trim_summary$sets)) model$misc$trim_summary$sets$trimmed else 0,
    parameters = if (!is.null(model$misc$trim_summary$parameters)) model$misc$trim_summary$parameters$trimmed else 0,
    mappings = if (!is.null(model$misc$trim_summary$mappings)) model$misc$trim_summary$mappings$trimmed else 0,
    variables = if (!is.null(model$misc$trim_summary$variables)) model$misc$trim_summary$variables$trimmed else 0,
    equations = if (!is.null(model$misc$trim_summary$equations)) model$misc$trim_summary$equations$trimmed else 0
  )
  
  total_net_trimmed <- sum(net_trimmed_counts)
  
  # Set trimmed flag based on whether ANY elements remain trimmed
  model$misc$trimmed <- total_net_trimmed > 0
  model$misc$trim_strategy <- strategy
  
  if (verbose) {
    # Report NET trim counts
    message("\n=== NET Trim Results (after Phase 4) ===")
    message(sprintf("  Sets trimmed:       %d", net_trimmed_counts["sets"]))
    message(sprintf("  Parameters trimmed: %d", net_trimmed_counts["parameters"]))
    message(sprintf("  Mappings trimmed:   %d", net_trimmed_counts["mappings"]))
    message(sprintf("  Variables trimmed:  %d", net_trimmed_counts["variables"]))
    message(sprintf("  Equations trimmed:  %d", net_trimmed_counts["equations"]))
    message(sprintf("  TOTAL trimmed:      %d", total_net_trimmed))
    
    if (total_net_trimmed == 0) {
      message("\nModel has not been trimmed (all elements are required).")
    } else {
      summary <- get_trim_summary(model, format = "text")
      message("\n", summary)
    }
  }
  
  return(model)
}

#' Mark sets, parameters, and mappings with empty data
#'
#' @param model A multimod model object
#' @param verbose Logical. Print progress messages
#' @return Model with `$trimmed = TRUE` on empty elements
#' @keywords internal
mark_empty_data <- function(model, verbose = FALSE) {
  n_trimmed <- 0
  
  # Check sets
  if (!is.null(model$sets) && length(model$sets) > 0) {
    for (set_name in names(model$sets)) {
      set_obj <- model$sets[[set_name]]
      if (is.null(set_obj$data) || length(set_obj$data) == 0) {
        model$sets[[set_name]]$trimmed <- TRUE
        n_trimmed <- n_trimmed + 1
        if (verbose) message("    Trimmed set: ", set_name)
      }
    }
  }
  
  # Check parameters
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    for (param_name in names(model$parameters)) {
      param_obj <- model$parameters[[param_name]]
      # Don't trim parameters with default values (they may be used even with no explicit data)
      has_default <- !is.null(param_obj$defVal)
      if ((is.null(param_obj$data) || nrow(param_obj$data) == 0) && !has_default) {
        model$parameters[[param_name]]$trimmed <- TRUE
        n_trimmed <- n_trimmed + 1
        if (verbose) message("    Trimmed parameter: ", param_name)
      }
    }
  }
  
  # Check mappings
  if (!is.null(model$mappings) && length(model$mappings) > 0) {
    for (mapping_name in names(model$mappings)) {
      mapping_obj <- model$mappings[[mapping_name]]
      if (is.null(mapping_obj$data) || nrow(mapping_obj$data) == 0) {
        model$mappings[[mapping_name]]$trimmed <- TRUE
        n_trimmed <- n_trimmed + 1
        if (verbose) message("    Trimmed mapping: ", mapping_name)
      }
    }
  }
  
  if (verbose && n_trimmed > 0) {
    message("    Total empty data elements: ", n_trimmed)
  }
  
  return(model)
}

#' Mark variables with empty domains
#'
#' Checks if any dimension of a variable references an empty or trimmed set.
#'
#' @param model A multimod model object
#' @param verbose Logical. Print progress messages
#' @return Model with `$trimmed = TRUE` on variables with empty domains
#' @keywords internal
mark_empty_variables <- function(model, verbose = FALSE) {
  if (is.null(model$variables) || length(model$variables) == 0) {
    return(model)
  }
  
  n_trimmed <- 0
  
  for (var_name in names(model$variables)) {
    var_obj <- model$variables[[var_name]]
    
    # Skip if already trimmed
    if (isTRUE(var_obj$trimmed)) next
    
    # Check if variable has a domain mapping
    if (!is.null(var_obj$domain) && length(var_obj$domain) > 0) {
      # Domain is a mapping name - extract it
      domain_name <- var_obj$domain
      if (is.list(domain_name) && !is.null(domain_name$name)) {
        domain_name <- domain_name$name
      } else if (!is.character(domain_name)) {
        domain_name <- as.character(domain_name)
      }
      
      if (is.character(domain_name) && length(domain_name) == 1 && 
          domain_name %in% names(model$mappings) && !is.null(model$mappings[[domain_name]])) {
        mapping_obj <- model$mappings[[domain_name]]
        if (isTRUE(mapping_obj$trimmed)) {
          model$variables[[var_name]]$trimmed <- TRUE
          n_trimmed <- n_trimmed + 1
          if (verbose) message("    Trimmed variable (empty domain mapping): ", var_name)
          next
        }
      }
    }
    
    # Check each dimension (set) of the variable
    if (!is.null(var_obj$dims)) {
      dims <- get_dim_names(var_obj$dims)
      
      for (dim_name in dims) {
        if (!is.null(model$sets[[dim_name]])) {
          set_obj <- model$sets[[dim_name]]
          if (isTRUE(set_obj$trimmed) || is.null(set_obj$data) || length(set_obj$data) == 0) {
            model$variables[[var_name]]$trimmed <- TRUE
            n_trimmed <- n_trimmed + 1
            if (verbose) message("    Trimmed variable (empty set '", dim_name, "'): ", var_name)
            break
          }
        }
      }
    }
  }
  
  if (verbose && n_trimmed > 0) {
    message("    Total variables with empty domains: ", n_trimmed)
  }
  
  return(model)
}

#' Mark equations with empty domains or all variables trimmed
#'
#' @param model A multimod model object
#' @param verbose Logical. Print progress messages
#' @return Model with `$trimmed = TRUE` on equations with empty domains
#' @keywords internal
mark_empty_equations <- function(model, verbose = FALSE) {
  if (is.null(model$equations) || length(model$equations) == 0) {
    return(model)
  }
  
  n_trimmed <- 0
  
  for (eq_name in names(model$equations)) {
    eq_obj <- model$equations[[eq_name]]
    
    # Skip if already trimmed
    if (isTRUE(eq_obj$trimmed)) next
    
    # Check if equation has a domain mapping
    if (!is.null(eq_obj$domain) && length(eq_obj$domain) > 0) {
      # Extract domain name - handle different formats
      domain_name <- eq_obj$domain
      if (is.list(domain_name) && !is.null(domain_name$name)) {
        domain_name <- domain_name$name
      } else if (!is.character(domain_name)) {
        domain_name <- as.character(domain_name)
      }
      
      if (is.character(domain_name) && length(domain_name) == 1 && 
          domain_name %in% names(model$mappings) && !is.null(model$mappings[[domain_name]])) {
        mapping_obj <- model$mappings[[domain_name]]
        if (isTRUE(mapping_obj$trimmed)) {
          model$equations[[eq_name]]$trimmed <- TRUE
          n_trimmed <- n_trimmed + 1
          if (verbose) message("    Trimmed equation (empty domain mapping): ", eq_name)
          next
        }
      }
    }
    
    # Check each dimension (set) of the equation
    if (!is.null(eq_obj$dims)) {
      dims <- get_dim_names(eq_obj$dims)
      
      for (dim_name in dims) {
        if (!is.null(model$sets[[dim_name]])) {
          set_obj <- model$sets[[dim_name]]
          if (isTRUE(set_obj$trimmed) || is.null(set_obj$data) || length(set_obj$data) == 0) {
            model$equations[[eq_name]]$trimmed <- TRUE
            n_trimmed <- n_trimmed + 1
            if (verbose) message("    Trimmed equation (empty set '", dim_name, "'): ", eq_name)
            break
          }
        }
      }
    }
    
    # Check if all variables in equation are trimmed
    vars_in_eq <- extract_variables_from_equation(eq_obj)
    if (length(vars_in_eq) > 0) {
      all_vars_trimmed <- all(sapply(vars_in_eq, function(v) {
        if (!is.null(model$variables[[v]])) {
          return(isTRUE(model$variables[[v]]$trimmed))
        }
        return(FALSE)
      }))
      
      if (all_vars_trimmed) {
        model$equations[[eq_name]]$trimmed <- TRUE
        n_trimmed <- n_trimmed + 1
        if (verbose) message("    Trimmed equation (all variables trimmed): ", eq_name)
      }
    }
  }
  
  if (verbose && n_trimmed > 0) {
    message("    Total equations with empty domains: ", n_trimmed)
  }
  
  return(model)
}

#' Extract variable names from equation AST
#'
#' @param eq_obj Equation object
#' @return Character vector of variable names
#' @keywords internal
extract_variables_from_equation <- function(eq_obj) {
  vars <- c(
    extract_ast_names(eq_obj$lhs, types = "variable"),
    extract_ast_names(eq_obj$rhs, types = "variable")
  )
  return(unique(vars))
}

#' Extract parameter names from equation AST
#'
#' @param eq_obj Equation object
#' @return Character vector of parameter names
#' @keywords internal
extract_parameters_from_equation <- function(eq_obj) {
  params <- c(
    extract_ast_names(eq_obj$lhs, types = "parameter"),
    extract_ast_names(eq_obj$rhs, types = "parameter")
  )
  return(unique(params))
}

#' Extract mapping names from equation AST
#'
#' @param eq_obj Equation object
#' @return Character vector of mapping names
#' @keywords internal
extract_mappings_from_equation <- function(eq_obj) {
  mappings <- c(
    extract_ast_names(eq_obj$lhs, types = "mapping"),
    extract_ast_names(eq_obj$rhs, types = "mapping")
  )
  return(unique(mappings))
}

#' Untrim elements still needed by non-trimmed equations
#'
#' Safety check: ensures variables and parameters used in active equations
#' are not marked as trimmed.
#'
#' @param model A multimod model object
#' @param verbose Logical. Print progress messages
#' @return Model with required elements untrimmed
#' @keywords internal
untrim_required_elements <- function(model, verbose = FALSE) {
  if (is.null(model$equations) || length(model$equations) == 0) {
    return(model)
  }
  
  n_untrimmed <- 0
  
  # Collect all variables, parameters, mappings, and sets from non-trimmed equations
  required_vars <- character(0)
  required_params <- character(0)
  required_mappings <- character(0)
  required_sets <- character(0)
  
  for (eq_name in names(model$equations)) {
    eq_obj <- model$equations[[eq_name]]
    
    # Skip trimmed equations
    if (isTRUE(eq_obj$trimmed)) next
    
    # Collect variables and parameters from equation expression
    required_vars <- c(required_vars, extract_variables_from_equation(eq_obj))
    required_params <- c(required_params, extract_parameters_from_equation(eq_obj))
    
    # Collect mappings from conditional expressions (if ... in mapping)
    required_mappings <- c(required_mappings, extract_mappings_from_equation(eq_obj))
    
    # Collect mappings from equation domain
    if (!is.null(eq_obj$domain)) {
      domain_names <- if (is.list(eq_obj$domain)) {
        sapply(eq_obj$domain, function(d) if (is.list(d) && !is.null(d$name)) d$name else as.character(d))
      } else {
        as.character(eq_obj$domain)
      }
      required_mappings <- c(required_mappings, domain_names)
    }
    
    # Collect mappings from dims (dimension restrictions)
    if (!is.null(eq_obj$dims) && length(eq_obj$dims) > 0) {
      for (dim in eq_obj$dims) {
        if (is.list(dim) && !is.null(dim$set)) {
          required_mappings <- c(required_mappings, dim$set)
        }
      }
    }
  }
  
  required_vars <- unique(required_vars)
  required_params <- unique(required_params)
  # Remove empty strings and NAs
  required_vars <- required_vars[!is.na(required_vars) & nzchar(required_vars)]
  required_params <- required_params[!is.na(required_params) & nzchar(required_params)]
  required_mappings <- unique(required_mappings)
  # Remove empty strings and NAs
  required_mappings <- required_mappings[!is.na(required_mappings) & nzchar(required_mappings)]
  
  # Collect sets used by non-trimmed variables
  if (!is.null(model$variables)) {
    for (var_name in names(model$variables)) {
      var_obj <- model$variables[[var_name]]
      if (!isTRUE(var_obj$trimmed) && !is.null(var_obj$domain)) {
        required_mappings <- c(required_mappings, var_obj$domain)
      }
    }
  }
  
  # Collect sets used by non-trimmed parameters OR parameters about to be untrimmed
  if (!is.null(model$parameters)) {
    for (param_name in names(model$parameters)) {
      param_obj <- model$parameters[[param_name]]
      # Include both non-trimmed params AND params in required_params list
      is_required <- param_name %in% required_params
      if ((!isTRUE(param_obj$trimmed) || is_required) && !is.null(param_obj$dims)) {
        # Extract set names from dims AST by converting to string
        if (inherits(param_obj$dims, "dims")) {
          # Use as.character to get comma-separated list: "[set1,set2,...]"
          dims_str <- as.character(param_obj$dims)
          # Remove brackets and split by comma
          dims_str <- gsub("^\\[|\\]$", "", dims_str)
          dim_sets <- strsplit(dims_str, ",")[[1]]
          dim_sets <- trimws(dim_sets)
          required_sets <- c(required_sets, dim_sets)
        } else {
          # Fallback for other formats
          required_sets <- c(required_sets, unlist(param_obj$dims))
        }
      }
    }
  }
  
  # Collect sets used by non-trimmed mappings OR mappings about to be untrimmed
  if (!is.null(model$mappings) && length(model$mappings) > 0) {
    for (mapping_name in names(model$mappings)) {
      mapping_obj <- model$mappings[[mapping_name]]
      # Include both non-trimmed mappings AND mappings in required_mappings list
      is_required <- mapping_name %in% required_mappings
      if ((!isTRUE(mapping_obj$trimmed) || is_required) && !is.null(mapping_obj$dims)) {
        # Extract set names from dims AST by converting to string
        if (inherits(mapping_obj$dims, "dims")) {
          # Use as.character to get comma-separated list: "[set1,set2,...]"
          dims_str <- as.character(mapping_obj$dims)
          # Remove brackets and split by comma
          dims_str <- gsub("^\\[|\\]$", "", dims_str)
          dim_sets <- strsplit(dims_str, ",")[[1]]
          required_sets <- c(required_sets, trimws(dim_sets))
        } else {
          required_sets <- c(required_sets, unlist(mapping_obj$dims))
        }
      }
    }
  }
  
  required_sets <- unique(required_sets)
  # Remove empty strings and NAs
  required_sets <- required_sets[!is.na(required_sets) & nzchar(required_sets)]
  
  # Untrim required variables
  if (!is.null(model$variables) && length(required_vars) > 0) {
    for (var_name in required_vars) {
      if (!is.null(model$variables[[var_name]]) && 
          isTRUE(model$variables[[var_name]]$trimmed)) {
        model$variables[[var_name]]$trimmed <- FALSE
        n_untrimmed <- n_untrimmed + 1
        if (verbose) message("    Untrimmed variable (used in equation): ", var_name)
      }
    }
  }
  
  # Untrim required parameters
  if (!is.null(model$parameters) && length(required_params) > 0) {
    for (param_name in required_params) {
      if (!is.null(model$parameters[[param_name]]) && 
          isTRUE(model$parameters[[param_name]]$trimmed)) {
        model$parameters[[param_name]]$trimmed <- FALSE
        n_untrimmed <- n_untrimmed + 1
        if (verbose) message("    Untrimmed parameter (used in equation): ", param_name)
      }
    }
  }
  
  # Untrim required mappings
  if (!is.null(model$mappings) && length(model$mappings) > 0 && length(required_mappings) > 0) {
    for (mapping_name in required_mappings) {
      if (mapping_name %in% names(model$mappings) && 
          !is.null(model$mappings[[mapping_name]]) && 
          isTRUE(model$mappings[[mapping_name]]$trimmed)) {
        model$mappings[[mapping_name]]$trimmed <- FALSE
        n_untrimmed <- n_untrimmed + 1
        if (verbose) message("    Untrimmed mapping (used in equation domain): ", mapping_name)
      }
    }
  }
  
  # Untrim required sets
  if (!is.null(model$sets) && length(required_sets) > 0) {
    for (set_name in required_sets) {
      if (!is.null(model$sets[[set_name]]) && 
          isTRUE(model$sets[[set_name]]$trimmed)) {
        model$sets[[set_name]]$trimmed <- FALSE
        n_untrimmed <- n_untrimmed + 1
        if (verbose) message("    Untrimmed set (used in dimension): ", set_name)
      }
    }
  }
  
  if (verbose && n_untrimmed > 0) {
    message("    Total elements untrimmed: ", n_untrimmed)
  }
  
  return(model)
}

#' Remove variables not appearing in any non-trimmed equation
#'
#' Final pass to mark variables that don't appear in any active equation.
#'
#' @param model A multimod model object
#' @param verbose Logical. Print progress messages
#' @return Model with unused variables trimmed
#' @keywords internal
remove_unused_variables <- function(model, verbose = FALSE) {
  if (is.null(model$variables) || length(model$variables) == 0) {
    return(model)
  }
  
  if (is.null(model$equations) || length(model$equations) == 0) {
    # No equations - trim all variables
    for (var_name in names(model$variables)) {
      if (!isTRUE(model$variables[[var_name]]$trimmed)) {
        model$variables[[var_name]]$trimmed <- TRUE
        if (verbose) message("    Trimmed unused variable: ", var_name)
      }
    }
    return(model)
  }
  
  # Collect all variables appearing in non-trimmed equations
  used_vars <- character(0)
  for (eq_name in names(model$equations)) {
    eq_obj <- model$equations[[eq_name]]
    if (!isTRUE(eq_obj$trimmed)) {
      used_vars <- c(used_vars, extract_variables_from_equation(eq_obj))
    }
  }
  used_vars <- unique(used_vars)
  
  # Mark unused variables
  n_trimmed <- 0
  for (var_name in names(model$variables)) {
    if (!var_name %in% used_vars && !isTRUE(model$variables[[var_name]]$trimmed)) {
      model$variables[[var_name]]$trimmed <- TRUE
      n_trimmed <- n_trimmed + 1
      if (verbose) message("    Trimmed unused variable: ", var_name)
    }
  }
  
  if (verbose && n_trimmed > 0) {
    message("    Total unused variables: ", n_trimmed)
  }
  
  return(model)
}

#' Collect trimming statistics
#'
#' @param model A multimod model object
#' @return List with trim statistics
#' @keywords internal
collect_trim_stats <- function(model) {
  stats <- list(
    sets = list(total = 0, trimmed = 0, names = character(0)),
    parameters = list(total = 0, trimmed = 0, names = character(0)),
    mappings = list(total = 0, trimmed = 0, names = character(0)),
    variables = list(total = 0, trimmed = 0, names = character(0)),
    equations = list(total = 0, trimmed = 0, names = character(0))
  )
  
  # Count sets
  if (!is.null(model$sets) && length(model$sets) > 0) {
    stats$sets$total <- length(model$sets)
    trimmed <- sapply(model$sets, function(x) isTRUE(x$trimmed))
    stats$sets$trimmed <- sum(trimmed)
    stats$sets$names <- names(model$sets)[trimmed]
  }
  
  # Count parameters
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    stats$parameters$total <- length(model$parameters)
    trimmed <- sapply(model$parameters, function(x) isTRUE(x$trimmed))
    stats$parameters$trimmed <- sum(trimmed)
    stats$parameters$names <- names(model$parameters)[trimmed]
  }
  
  # Count mappings
  if (!is.null(model$mappings) && length(model$mappings) > 0) {
    stats$mappings$total <- length(model$mappings)
    trimmed <- sapply(model$mappings, function(x) isTRUE(x$trimmed))
    stats$mappings$trimmed <- sum(trimmed)
    stats$mappings$names <- names(model$mappings)[trimmed]
  }
  
  # Count variables
  if (!is.null(model$variables) && length(model$variables) > 0) {
    stats$variables$total <- length(model$variables)
    trimmed <- sapply(model$variables, function(x) isTRUE(x$trimmed))
    stats$variables$trimmed <- sum(trimmed)
    stats$variables$names <- names(model$variables)[trimmed]
  }
  
  # Count equations
  if (!is.null(model$equations) && length(model$equations) > 0) {
    stats$equations$total <- length(model$equations)
    trimmed <- sapply(model$equations, function(x) isTRUE(x$trimmed))
    stats$equations$trimmed <- sum(trimmed)
    stats$equations$names <- names(model$equations)[trimmed]
  }
  
  return(stats)
}

#' Check if model or object is trimmed
#'
#' @param x Model object, or individual set/parameter/mapping/variable/equation
#' @return Logical indicating if trimmed
#'
#' @examples
#' \dontrun{
#' is_trimmed(model)
#' is_trimmed(model$parameters$pDemand)
#' }
#'
#' @export
is_trimmed <- function(x) {
  if (inherits(x, "model_structure") || inherits(x, "model")) {
    return(isTRUE(x$misc$trimmed))
  } else {
    return(isTRUE(x$trimmed))
  }
}

#' Check if any elements are actually trimmed
#'
#' Walks through model structure to check if any sets, parameters, mappings,
#' variables, or equations have `$trimmed = TRUE`. More robust than checking
#' only the `model$misc$trimmed` flag.
#'
#' @param model A multimod model object
#' @param include_equations Logical. If TRUE (default), include equations in check.
#'   If FALSE, only check data elements (sets, parameters, mappings, variables).
#' @return Logical indicating if any elements are trimmed
#'
#' @examples
#' \dontrun{
#' # Check if model has any trimmed elements
#' if (is_trimmed_any(model)) {
#'   cat("Model has trimmed elements\n")
#' }
#' # Check only data elements (excluding equations)
#' if (is_trimmed_any(model, include_equations = FALSE)) {
#'   cat("Model has trimmed data elements\n")
#' }
#' }
#'
#' @export
is_trimmed_any <- function(model, include_equations = TRUE) {
  stopifnot(inherits(model, "model_structure") || inherits(model, "model"))
  
  # Check sets
  if (!is.null(model$sets) && length(model$sets) > 0) {
    for (set_obj in model$sets) {
      if (isTRUE(set_obj$trimmed)) return(TRUE)
    }
  }
  
  # Check parameters
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    for (param_obj in model$parameters) {
      if (isTRUE(param_obj$trimmed)) return(TRUE)
    }
  }
  
  # Check mappings
  if (!is.null(model$mappings) && length(model$mappings) > 0) {
    for (map_obj in model$mappings) {
      if (isTRUE(map_obj$trimmed)) return(TRUE)
    }
  }
  
  # Check variables
  if (!is.null(model$variables) && length(model$variables) > 0) {
    for (var_obj in model$variables) {
      if (isTRUE(var_obj$trimmed)) return(TRUE)
    }
  }
  
  # Check equations (optional)
  if (include_equations) {
    if (!is.null(model$equations) && length(model$equations) > 0) {
      for (eq_obj in model$equations) {
        if (isTRUE(eq_obj$trimmed)) return(TRUE)
      }
    }
    
    # Check folded_equations if present
    if (!is.null(model$folded_equations) && length(model$folded_equations) > 0) {
      for (eq_obj in model$folded_equations) {
        if (isTRUE(eq_obj$trimmed)) return(TRUE)
      }
    }
  }
  
  return(FALSE)
}

#' Get trim summary statistics
#'
#' @param model A multimod model object
#' @param format Output format: "list" (default), "data.frame", or "text"
#' @return Trim statistics in requested format
#'
#' @examples
#' \dontrun{
#' # After trimming
#' model <- trim_model(model)
#' 
#' # Get summary
#' summary <- get_trim_summary(model)
#' summary_df <- get_trim_summary(model, format = "data.frame")
#' cat(get_trim_summary(model, format = "text"))
#' }
#'
#' @export
get_trim_summary <- function(model, format = c("list", "data.frame", "text")) {
  format <- match.arg(format)
  
  if (!is_trimmed(model)) {
    if (format == "text") {
      return("Model has not been trimmed (all elements are required).")
    } else {
      return(NULL)
    }
  }
  
  stats <- model$misc$trim_summary
  
  # Check which element types are trimmed
  has_data_trimmed <- is_trimmed_any(model, include_equations = FALSE)
  eq_trimmed <- if (!is.null(stats$equations)) stats$equations$trimmed else 0
  
  # Provide context-specific message for text format
  if (format == "text") {
    if (!has_data_trimmed && eq_trimmed > 0) {
      # Special case: Only equations trimmed (all data untrimmed)
      return(sprintf(
        "Model trimming partial: All data elements untrimmed (required by active equations).\n%d equations remain trimmed (empty domains - no instances to generate).",
        eq_trimmed
      ))
    }
  }
  
  if (format == "list") {
    return(stats)
  }
  
  if (format == "data.frame") {
    df <- data.frame(
      element_type = character(0),
      total = integer(0),
      trimmed = integer(0),
      remaining = integer(0),
      percent_trimmed = numeric(0),
      stringsAsFactors = FALSE
    )
    
    for (type in c("sets", "parameters", "mappings", "variables", "equations")) {
      if (!is.null(stats[[type]])) {
        total <- stats[[type]]$total
        trimmed <- stats[[type]]$trimmed
        remaining <- total - trimmed
        pct <- if (total > 0) round(100 * trimmed / total, 1) else 0
        
        df <- rbind(df, data.frame(
          element_type = type,
          total = total,
          trimmed = trimmed,
          remaining = remaining,
          percent_trimmed = pct,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(df)
  }
  
  if (format == "text") {
    lines <- c(
      "Model Trimming Summary",
      "======================"
    )
    
    for (type in c("sets", "parameters", "mappings", "variables", "equations")) {
      if (!is.null(stats[[type]])) {
        total <- stats[[type]]$total
        trimmed <- stats[[type]]$trimmed
        remaining <- total - trimmed
        pct <- if (total > 0) round(100 * trimmed / total, 1) else 0
        
        lines <- c(lines, sprintf(
          "%s: %d/%d trimmed (%.1f%%), %d remaining",
          type, trimmed, total, pct, remaining
        ))
      }
    }
    
    return(paste(lines, collapse = "\n"))
  }
}

#' Untrim model or specific elements
#'
#' Removes `$trimmed` flags from model or specific elements.
#'
#' @param model A multimod model object
#' @param elements Character vector of element types to untrim:
#'   "all" (default), "sets", "parameters", "mappings", "variables", "equations"
#'
#' @return Model with trimmed flags removed
#'
#' @examples
#' \dontrun{
#' # Untrim everything
#' model <- untrim_model(model)
#' 
#' # Untrim only variables and equations
#' model <- untrim_model(model, elements = c("variables", "equations"))
#' }
#'
#' @export
untrim_model <- function(model, elements = "all") {
  stopifnot(inherits(model, "model_structure"))
  
  if ("all" %in% elements) {
    elements <- c("sets", "parameters", "mappings", "variables", "equations")
  }
  
  # Untrim sets
  if ("sets" %in% elements && !is.null(model$sets)) {
    for (set_name in names(model$sets)) {
      model$sets[[set_name]]$trimmed <- NULL
    }
  }
  
  # Untrim parameters
  if ("parameters" %in% elements && !is.null(model$parameters)) {
    for (param_name in names(model$parameters)) {
      model$parameters[[param_name]]$trimmed <- NULL
    }
  }
  
  # Untrim mappings
  if ("mappings" %in% elements && !is.null(model$mappings)) {
    for (mapping_name in names(model$mappings)) {
      model$mappings[[mapping_name]]$trimmed <- NULL
    }
  }
  
  # Untrim variables
  if ("variables" %in% elements && !is.null(model$variables)) {
    for (var_name in names(model$variables)) {
      model$variables[[var_name]]$trimmed <- NULL
    }
  }
  
  # Untrim equations
  if ("equations" %in% elements && !is.null(model$equations)) {
    for (eq_name in names(model$equations)) {
      model$equations[[eq_name]]$trimmed <- NULL
    }
  }
  
  # Clear model-level trim metadata
  if ("all" %in% elements || length(elements) == 5) {
    model$misc$trimmed <- NULL
    model$misc$trim_summary <- NULL
    model$misc$trim_strategy <- NULL
  }
  
  return(model)
}
