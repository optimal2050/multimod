#' Evaluate Parameter Formulas and Defaults
#'
#' Convert AST formulas and default expressions to computed numeric values.
#' Handles dependency resolution through topological sorting.
#'
#' @param model Model object with parameters
#' @param data Named list of input data (sets and parameter values)
#' @param preserve_ast If TRUE, store original AST in param$misc$ast_*
#' @param verbose Print progress messages
#' @param strategy Evaluation strategy: "dag" for topological order, "iterative" for multi-pass
#'   retries, or "auto" (default) to try DAG first and fall back to iterative on failure.
#'
#' @return Updated model with computed parameter values
#' @export
#'
#' @examples
#' \dontrun{
#' gmpl <- read_gmpl("model.mod")
#' data <- list(
#'   REGION = c("R1", "R2"),
#'   DiscountRate = c(R1=0.05, R2=0.07)
#' )
#' model <- evaluate_parameters(gmpl, data)
#' }
evaluate_parameters <- function(
    model,
    data = list(),
    preserve_ast = TRUE,
    verbose = FALSE,
    strategy = c("auto", "dag", "iterative")
) {
  strategy <- match.arg(strategy)
  
  if (verbose) message("Evaluating parameter formulas and defaults...")
  
  # Materialize sets as their member values so formulas can iterate over indices
  set_data <- list()
  if (!is.null(model$sets) && length(model$sets) > 0) {
    for (sname in names(model$sets)) {
      members <- extract_set_members(model$sets[[sname]])
      if (!is.null(members)) {
        set_data[[sname]] <- members
      }
    }
  }

  # Combine model sets, existing parameter data, and user-provided data for lookup
  all_data <- c(set_data, data)

  # Ensure parameters that already have data/values are available for dependency resolution
  for (pname in names(model$parameters)) {
    param <- model$parameters[[pname]]
    param_data <- tryCatch(get_data(model, pname, type = "parameter"), error = function(e) NULL)
    materialized <- materialize_parameter_values(param, param_data, set_data)
    if (!is.null(materialized)) {
      all_data[[pname]] <- materialized
    } else if (!is.null(param$defVal) && !is_ast_node(param$defVal)) {
      all_data[[pname]] <- param$defVal
    }
  }
  
  # Track which parameters need evaluation
  needs_eval <- list()
  computed_params <- character(0)
  
  # Find parameters with AST formulas or defaults
  for (pname in names(model$parameters)) {
    param <- model$parameters[[pname]]
    has_formula <- !is.null(param$formula) && is_ast_node(param$formula)
    has_defval_ast <- !is.null(param$defVal) && is_ast_node(param$defVal)
    
    if (has_formula || has_defval_ast) {
      computed_params <- c(computed_params, pname)
      needs_eval[[pname]] <- list(
        formula = has_formula,
        defval = has_defval_ast,
        deps = character(0)
      )
    }
  }
  
  if (length(needs_eval) == 0) {
    if (verbose) message("  No parameters need evaluation")
    return(model)
  }
  
  # Extract dependencies for each parameter
  for (pname in names(needs_eval)) {
    param <- model$parameters[[pname]]
    deps <- character(0)
    
    if (needs_eval[[pname]]$formula) {
      deps <- c(deps, get_dependencies(param$formula, model))
    }
    if (needs_eval[[pname]]$defval) {
      deps <- c(deps, get_dependencies(param$defVal, model))
    }
    
    needs_eval[[pname]]$deps <- unique(deps)
  }
  
  evaluate_single_param <- function(pname, stop_on_error = TRUE) {
    result <- list(success = TRUE, error = NULL)
    info <- needs_eval[[pname]]
    if (is.null(info)) {
      return(result)
    }
    param <- model$parameters[[pname]]

    run_eval <- function() {
      if (verbose) message("  Evaluating: ", pname)

      # Evaluate formula
      if (info$formula) {
        if (verbose) message("    - Computing formula")

        if (preserve_ast) {
          if (is.null(param$misc)) param$misc <- list()
          param$misc$ast_formula <- param$formula
        }

        result_data <- evaluate_formula(
          param$formula,
          param$dims,
          param$dims_index_aliases,
          all_data,
          model,
          verbose
        )
        param$data <- parameter_result_to_dataframe(result_data, param$dims)
        param["formula"] <- list(NULL)
        all_data[[pname]] <<- result_data
      }

      # Evaluate default expression
      if (info$defval) {
        if (verbose) message("    - Computing default")

        if (preserve_ast) {
          if (is.null(param$misc)) param$misc <- list()
          param$misc$ast_defVal <- param$defVal
        }

        defval_args <- extract_args(param$defVal)
        param_dims_aliases <- if (!is.null(param$dims_index_aliases)) {
          param$dims_index_aliases
        } else {
          character(0)
        }
        has_index_vars <- any(defval_args %in% param_dims_aliases)

        if (has_index_vars) {
          result_data <- evaluate_formula(
            param$defVal,
            param$dims,
            param$dims_index_aliases,
            all_data,
            model,
            verbose
          )
          param$data <- parameter_result_to_dataframe(result_data, param$dims)
          all_data[[pname]] <<- result_data
        } else {
          result_scalar <- evaluate_default_expr(param$defVal, all_data, model, verbose)
          param$defVal <- result_scalar
        }
      }

      param <- ensure_parameter_defval(param)
      model$parameters[[pname]] <<- param
      needs_eval[[pname]] <<- NULL
    }

    if (stop_on_error) {
      run_eval()
    } else {
      tryCatch(run_eval(), error = function(e) {
        result$success <<- FALSE
        result$error <<- e$message
      })
    }

    result
  }

  run_iterative <- function(max_passes = max(length(needs_eval), 1) * 2) {
    pass <- 0
    errors <- list()
    while (length(needs_eval) > 0 && pass < max_passes) {
      pass <- pass + 1
      progressed <- FALSE
      pending <- names(needs_eval)
      for (pname in pending) {
        res <- evaluate_single_param(pname, stop_on_error = FALSE)
        if (res$success) {
          progressed <- TRUE
          errors[[pname]] <- NULL
        } else {
          errors[[pname]] <- res$error
        }
      }
      if (!progressed) break
    }

    if (length(needs_eval) > 0) {
      remaining <- names(needs_eval)
      detail <- vapply(
        remaining,
        function(p) {
          err <- errors[[p]]
          if (is.null(err)) "" else err
        },
        character(1),
        USE.NAMES = TRUE
      )
      detail <- detail[nzchar(detail)]
      error_msg <- paste0(
        "Unable to evaluate parameters: ",
        paste(remaining, collapse = ", ")
      )
      if (length(detail) > 0) {
        error_msg <- paste0(
          error_msg,
          ". Last errors: ",
          paste(paste(names(detail), detail, sep = ": "), collapse = "; ")
        )
      }
      stop(error_msg, call. = FALSE)
    }
  }

  dag_attempted <- FALSE
  dag_failed <- FALSE
  dag_error <- NULL

  if (strategy %in% c("auto", "dag")) {
    dag_attempted <- TRUE
    eval_order <- topological_sort(needs_eval, verbose)
    if (verbose) message("  Evaluation order: ", paste(eval_order, collapse = " -> "))

    dag_status <- tryCatch({
      for (pname in eval_order) {
        evaluate_single_param(pname, stop_on_error = TRUE)
      }
      TRUE
    }, error = function(e) {
      if (strategy == "dag") {
        stop(e)
      }
      dag_failed <<- TRUE
      dag_error <<- e
      FALSE
    })

    if (dag_status && strategy != "auto") {
      if (verbose) message("  Evaluation complete!")
      return(model)
    }
  }

  if (strategy == "iterative" || (strategy == "auto" && (dag_failed || length(needs_eval) > 0))) {
    if (strategy == "auto" && dag_failed && verbose) {
      message("  DAG evaluation failed, falling back to iterative: ", dag_error$message)
    }
    run_iterative()
  }

  if (verbose) message("  Evaluation complete!")
  model
}


#' Check if object is an AST node
#' @keywords internal
is_ast_node <- function(x) {
  is.list(x) && inherits(x, "ast")
}


#' Extract the member values of a set
#' @keywords internal
extract_set_members <- function(set_obj) {
  if (is.null(set_obj)) {
    return(NULL)
  }
  values <- set_obj$data
  if (is.null(values)) {
    return(NULL)
  }
  if (is.data.frame(values)) {
    if (ncol(values) == 0) {
      return(character())
    }
    values <- values[[1]]
  }
  if (is.list(values) && !is.data.frame(values) && !is.atomic(values)) {
    values <- unlist(values, use.names = FALSE)
  }
  # Attempt numeric coercion when all members are numeric-like strings
  if (is.character(values)) {
    numeric_values <- suppressWarnings(as.numeric(values))
    if (!any(is.na(numeric_values))) {
      values <- numeric_values
    }
  }
  values
}


#' Convert parameter data frames into arrays or vectors for indexing
#' @keywords internal
materialize_parameter_values <- function(param, data = NULL, dim_members = list()) {
  if (is.null(data)) {
    data <- param$data
  }
  if (is.null(data)) {
    return(NULL)
  }

  dim_names <- normalize_dim_names(param$dims)

  if (length(dim_names) == 0) {
    if (is.data.frame(data)) {
      if (!"value" %in% names(data) || nrow(data) == 0) {
        return(NULL)
      }
      return(data$value)
    }
    return(data)
  }

  scalar_fill <- NULL
  if (!is.data.frame(data)) {
    scalar_fill <- data
    data <- NULL
  }

  if (!is.null(data) && (!"value" %in% names(data) || nrow(data) == 0)) {
    data <- NULL
  }

  # Collect dimension members from sets (preferred) or data as fallback
  dim_levels <- lapply(dim_names, function(dname) {
    if (!is.null(dim_members[[dname]])) {
      return(dim_members[[dname]])
    }
    if (!is.null(data[[dname]])) {
      return(unique(data[[dname]]))
    }
    NULL
  })
  names(dim_levels) <- dim_names

  # Extend Missing dimension members with observed data values
  if (!is.null(data)) {
    for (dname in dim_names) {
      observed <- data[[dname]]
      if (is.null(observed)) next
      if (is.factor(observed)) observed <- as.character(observed)
      dim_levels[[dname]] <- unique(c(dim_levels[[dname]], observed))
    }
  }

  # If any dimension has no members, bail out
  if (any(vapply(dim_levels, function(x) is.null(x) || length(x) == 0, logical(1)))) {
    return(NULL)
  }

  dim_sizes <- vapply(dim_levels, length, integer(1))
  default_fill <- if (!is.null(scalar_fill)) {
    scalar_fill
  } else if (!is.null(param$defVal) && !is_ast_node(param$defVal)) {
    param$defVal
  } else {
    NA_real_
  }
  result_array <- array(default_fill, dim = dim_sizes, dimnames = dim_levels)

  if (!is.null(data) && nrow(data) > 0) {
    data_subset <- data[, c(dim_names, "value"), drop = FALSE]
    index_matrix <- matrix(NA_integer_, nrow = nrow(data_subset), ncol = length(dim_names))
    for (j in seq_along(dim_names)) {
      column <- data_subset[[dim_names[j]]]
      if (is.factor(column)) {
        column <- as.character(column)
      }
      if (is.character(column) && is.numeric(dim_levels[[dim_names[j]]])) {
        column <- suppressWarnings(as.numeric(column))
      }
      index_matrix[, j] <- match(column, dim_levels[[dim_names[j]]])
    }
    valid_rows <- apply(!is.na(index_matrix), 1, all)
    if (any(valid_rows)) {
      result_array[index_matrix[valid_rows, , drop = FALSE]] <- data_subset$value[valid_rows]
    }
  }

  result_array
}


#' Convert evaluated parameter arrays into long-form data frames
#' @keywords internal
parameter_result_to_dataframe <- function(result, dims) {
  if (is.null(result)) {
    return(NULL)
  }

  if (is.data.frame(result)) {
    return(result)
  }

  dim_names <- normalize_dim_names(dims)
  base_df <- NULL

  if (length(dim_names) == 0) {
    base_df <- data.frame(value = as.numeric(result), stringsAsFactors = FALSE)
  } else if (!is.null(dim(result))) {
    tbl <- as.data.frame(as.table(result), stringsAsFactors = FALSE)
    if (ncol(tbl) == length(dim_names) + 1) {
      names(tbl) <- c(dim_names, "value")
    } else {
      names(tbl)[ncol(tbl)] <- "value"
    }
    base_df <- tbl
  } else if (length(dim_names) == 1) {
    idx_vals <- names(result)
    if (is.null(idx_vals) || any(idx_vals == "")) {
      idx_vals <- seq_along(result)
    }
    base_df <- data.frame(
      ..dim = idx_vals,
      value = as.numeric(result),
      stringsAsFactors = FALSE
    )
    names(base_df)[1] <- dim_names
  } else {
    # Fallback: flatten vector into data frame with synthetic indices
    base_df <- data.frame(
      value = as.numeric(result),
      stringsAsFactors = FALSE
    )
  }

  base_df
}


#' Ensure parameters retain a scalar default after evaluation
#' @keywords internal
ensure_parameter_defval <- function(param) {
  if (!is.null(param$defVal) && !is_ast_node(param$defVal)) {
    return(param)
  }

  data_obj <- param$data
  fallback <- NULL
  method <- NULL

  if (is.data.frame(data_obj) && "value" %in% names(data_obj) && nrow(data_obj) > 0) {
    numeric_vals <- suppressWarnings(as.numeric(data_obj$value))
    if (!all(is.na(numeric_vals))) {
      fallback <- mean(numeric_vals, na.rm = TRUE)
      method <- "mean"
    }
  } else if (is.atomic(data_obj) && length(data_obj) > 0) {
    numeric_vals <- suppressWarnings(as.numeric(data_obj))
    if (!all(is.na(numeric_vals))) {
      fallback <- mean(numeric_vals, na.rm = TRUE)
      method <- "mean"
    }
  }

  if (is.null(fallback) || is.na(fallback) || is.nan(fallback)) {
    fallback <- 0
    method <- if (is.null(method)) "zero" else method
  }

  param$defVal <- fallback
  if (is.null(param$misc)) param$misc <- list()
  param$misc$defVal_fallback <- list(
    method = method,
    note = "auto-generated fallback default after parameter evaluation",
    value = fallback
  )

  param
}


#' Get dependencies from AST
#' @keywords internal
get_dependencies <- function(ast, model) {
  # Extract all symbols from AST
  symbols <- extract_args(ast)
  
  # Filter to only parameters (exclude sets and index variables)
  param_names <- names(model$parameters)
  deps <- symbols[symbols %in% param_names]
  
  # Remove single lowercase letters (likely index variables like r, t, y)
  deps <- deps[!grepl("^[a-z]$", deps)]
  
  return(unique(deps))
}


#' Topological sort of parameters by dependencies
#' @keywords internal
topological_sort <- function(needs_eval, verbose = FALSE) {
  # Simple topological sort using Kahn's algorithm
  
  # Filter dependencies to only those that need evaluation
  # (input parameters like DiscountRate are not in needs_eval)
  for (pname in names(needs_eval)) {
    needs_eval[[pname]]$deps <- needs_eval[[pname]]$deps[needs_eval[[pname]]$deps %in% names(needs_eval)]
  }
  
  # Calculate in-degree (number of dependencies)
  in_degree <- sapply(needs_eval, function(x) length(x$deps))
  
  # Initialize result and queue
  result <- character(0)
  queue <- names(in_degree)[in_degree == 0]
  
  # Track processed parameters
  processed <- character(0)
  
  while (length(queue) > 0) {
    # Process parameter with no dependencies
    current <- queue[1]
    queue <- queue[-1]
    result <- c(result, current)
    processed <- c(processed, current)
    
    # Update in-degrees of dependent parameters
    for (pname in names(needs_eval)) {
      if (pname %in% processed) next
      
      # Remove processed parameter from dependencies
      if (current %in% needs_eval[[pname]]$deps) {
        in_degree[pname] <- in_degree[pname] - 1
        
        # If no more dependencies, add to queue
        if (in_degree[pname] == 0) {
          queue <- c(queue, pname)
        }
      }
    }
  }
  
  # Check for circular dependencies
  if (length(result) != length(needs_eval)) {
    remaining <- setdiff(names(needs_eval), result)
    stop("Circular dependencies detected among parameters: ", 
         paste(remaining, collapse=", "))
  }
  
  return(result)
}


#' Evaluate formula AST over all dimension combinations
#' @keywords internal
evaluate_formula <- function(formula_ast, dims, dims_index_aliases, all_data, model, verbose = FALSE) {
  
  # Create R function from AST
  fn <- as_rfunction(formula_ast)
  fn_args <- names(formals(fn))

  dim_names <- normalize_dim_names(dims)
  
  if (verbose) message("      Formula args: ", paste(fn_args, collapse=", "))
  
  # Handle scalar (no dims)
  if (length(dim_names) == 0) {
    # Build arguments
    args_list <- list()
    for (arg in fn_args) {
      if (arg %in% names(all_data)) {
        args_list[[arg]] <- all_data[[arg]]
      } else {
        stop("Missing data for argument: ", arg)
      }
    }
    
    # Evaluate
    result <- do.call(fn, args_list)
    return(result)
  }
  
  # Get dimension sets
  dim_sets <- lapply(dim_names, function(dname) {
    if (dname %in% names(all_data)) {
      all_data[[dname]]
    } else {
      stop("Missing set: ", dname)
    }
  })
  names(dim_sets) <- dim_names
  
  # Get index variable names (e.g., r, t, y instead of REGION, TECHNOLOGY, YEAR)
  index_vars <- resolve_index_vars(dims_index_aliases, dim_names)
  
  # Create all combinations of indices
  grid <- expand.grid(dim_sets, stringsAsFactors = FALSE)
  colnames(grid) <- dim_names
  
  # Evaluate for each combination
  results <- numeric(nrow(grid))
  
  for (i in seq_len(nrow(grid))) {
    # Build arguments for this index combination
    args_list <- list()
    
    # Add index variables
    for (set_name in dim_names) {
      idx_var <- index_vars[set_name]
      if (!idx_var %in% fn_args) {
        next
      }
      args_list[[idx_var]] <- grid[i, set_name]
    }
    
    # Add parameter/set data
    for (arg in fn_args) {
      if (!arg %in% names(args_list)) {
        if (arg %in% names(all_data)) {
          args_list[[arg]] <- all_data[[arg]]
        } else {
          stop("Missing data for argument: ", arg)
        }
      }
    }
    
    # Evaluate
    results[i] <- tryCatch({
      do.call(fn, args_list)
    }, error = function(e) {
      stop("Error evaluating formula at indices ", 
           paste(names(grid[i,]), "=", grid[i,], collapse=", "), 
           ": ", e$message)
    })
  }
  
  # Convert to named array/matrix structure
  if (length(dim_names) == 1) {
    names(results) <- grid[[1]]
  } else {
    # Create multi-dimensional array
    dim_sizes <- sapply(dim_sets, length)
    results <- array(results, dim = dim_sizes, dimnames = dim_sets)
  }
  
  return(results)
}

#' Normalize dimension specifications to a character vector of set names
#' @keywords internal
normalize_dim_names <- function(dims) {
  if (is.null(dims) || length(dims) == 0) {
    return(character())
  }
  dim_list <- dims
  if (is.atomic(dim_list) && !is.list(dim_list)) {
    dim_list <- as.list(dim_list)
  }
  if (!is.list(dim_list)) {
    dim_list <- list(dim_list)
  }
  dim_names <- vapply(dim_list, dim_entry_to_name, character(1), USE.NAMES = FALSE)
  if (any(is.na(dim_names))) {
    stop("Unable to determine names for dimensions: ", paste(which(is.na(dim_names)), collapse = ", "))
  }
  dim_names
}

#' Convert a single dimension entry to its name
#' @keywords internal
dim_entry_to_name <- function(entry) {
  if (is.null(entry)) return(NA_character_)
  if (is.character(entry) && length(entry) >= 1) {
    return(entry[1])
  }
  if (is.list(entry) || inherits(entry, "ast")) {
    if (!is.null(entry$name) && is.character(entry$name)) {
      return(entry$name[1])
    }
    if (!is.null(entry$symbol) && is.character(entry$symbol)) {
      return(entry$symbol[1])
    }
    if (!is.null(entry$set) && is.character(entry$set)) {
      return(entry$set[1])
    }
  }
  if (is.atomic(entry) && length(entry) >= 1) {
    return(as.character(entry[1]))
  }
  stop("Unsupported dimension entry type: ", paste(class(entry), collapse = "/"))
}

#' Resolve iterator variable names for dimensions
#' @keywords internal
resolve_index_vars <- function(dims_index_aliases, dim_names) {
  if (length(dim_names) == 0) {
    return(character())
  }
  aliases <- normalize_index_aliases(dims_index_aliases, dim_names)
  if (is.null(aliases)) {
    return(default_index_aliases(dim_names))
  }
  missing <- is.na(aliases) | aliases == ""
  if (any(missing)) {
    fallback <- default_index_aliases(dim_names)
    aliases[missing] <- fallback[missing]
  }
  aliases
}

#' Normalize provided dimension aliases to align with dim names
#' @keywords internal
normalize_index_aliases <- function(aliases, dim_names) {
  if (is.null(aliases) || length(aliases) == 0) {
    return(NULL)
  }
  alias_vec <- aliases
  if (is.list(alias_vec) && !is.atomic(alias_vec)) {
    alias_vec <- unlist(alias_vec, use.names = TRUE)
  }
  alias_vec <- as.character(alias_vec)
  alias_names <- names(alias_vec)
  if (is.null(alias_names)) {
    alias_names <- rep(NA_character_, length(alias_vec))
  }
  aligned <- rep(NA_character_, length(dim_names))
  names(aligned) <- dim_names
  for (i in seq_along(alias_vec)) {
    target <- alias_names[i]
    if (is.na(target) || target == "") {
      if (i <= length(dim_names)) {
        target <- dim_names[i]
      } else {
        next
      }
    }
    if (target %in% dim_names) {
      aligned[target] <- alias_vec[i]
    }
  }
  aligned
}

#' Generate default iterator aliases from dimension names
#' @keywords internal
default_index_aliases <- function(dim_names) {
  aliases <- tolower(substr(dim_names, 1, 1))
  aliases <- make.unique(aliases, sep = "_")
  names(aliases) <- dim_names
  aliases
}


#' Evaluate default expression AST
#' @keywords internal
evaluate_default_expr <- function(defval_ast, all_data, model, verbose = FALSE) {
  
  # If it's an ast_formula with index variables, keep it unevaluated
  if (inherits(defval_ast, "ast_formula")) {
    if (length(defval_ast$index_vars) > 0) {
      if (verbose) message("      Default expression has index variables, keeping as ast_formula")
      return(defval_ast)
    }
    # No index vars - extract the expr and evaluate it
    if (verbose) message("      ast_formula has no index vars, evaluating expr")
    defval_ast <- defval_ast$expr
  }
  
  # Create R function from AST
  fn <- as_rfunction(defval_ast)
  fn_args <- names(formals(fn))
  
  if (verbose) message("      Default args: ", paste(fn_args, collapse=", "))
  
  # Build arguments
  args_list <- list()
  for (arg in fn_args) {
    if (arg %in% names(all_data)) {
      args_list[[arg]] <- all_data[[arg]]
    } else {
      stop("Missing data for default expression argument: ", arg)
    }
  }
  
  # For simple defaults, might evaluate to scalar
  # For complex defaults, might need to return a function or keep as AST
  # For now, try to evaluate if possible
  
  result <- tryCatch({
    do.call(fn, args_list)
  }, error = function(e) {
    # If evaluation fails, keep as AST with warning
    warning("Could not evaluate default expression, keeping as AST: ", e$message)
    defval_ast
  })
  
  return(result)
}


#' Restore AST from preserved copies
#'
#' Restore formula and defVal AST from param$misc storage
#'
#' @param model Model with evaluated parameters
#'
#' @return Model with AST restored
#' @export
restore_ast <- function(model) {
  for (pname in names(model$parameters)) {
    param <- model$parameters[[pname]]
    
    # Restore formula AST
    if (!is.null(param$misc$ast_formula)) {
      param$formula <- param$misc$ast_formula
      param$misc$ast_formula <- NULL
    }
    
    # Restore defVal AST
    if (!is.null(param$misc$ast_defVal)) {
      param$defVal <- param$misc$ast_defVal
      param$misc$ast_defVal <- NULL
    }
    
    model$parameters[[pname]] <- param
  }
  
  return(model)
}


#' Check if model has unevaluated AST formulas or defaults
#'
#' @param model Model object
#'
#' @return Logical
#' @export
has_ast_formulas <- function(model) {
  for (param in model$parameters) {
    if (!is.null(param$formula) && is_ast_node(param$formula)) {
      return(TRUE)
    }
    if (!is.null(param$defVal) && is_ast_node(param$defVal)) {
      return(TRUE)
    }
  }
  return(FALSE)
}
