
#' Write a GMPL/MathProg model file from a multimod object
#'
#' @param model A `multimod` model object
#' @param file Output file path (optional, ignored if model_dir is provided)
#' @param model_dir Optional directory path for organized model structure (creates solvers/gmpl/)
#' @param format_expr logical; whether to format expressions with line breaks
#' @param include_solve logical; whether to include solve and end statements
#' @param export_vars character vector of variable names to export to CSV; if NULL (default), all variables are exported; if FALSE or character(0), no custom export
#' @param use_table_output logical; whether to add GMPL table statement for output (CSV format); default FALSE
#' @param use_folded logical; whether to use folded equations/data if available (default: NULL = auto-detect)
#' @param objective character; name of the objective variable (default: "cost")
#' @param export_data logical; whether to insert data export code after parameters (default: FALSE)
#' @param drop_default_values logical; if TRUE with export_data, exclude values that match defaults (default: FALSE)
#' @param INF numeric; value to use for Inf in parameter defaults (default: 1e20)
#' @param check logical; if TRUE, run validation checks before writing (default: TRUE)
#' @param resolve_aliases logical; if TRUE, resolve set aliases to base sets in generated GMPL (default: TRUE)
#' @param verbose logical; if TRUE, print detailed validation progress (default: FALSE)
#' @param ... Additional arguments passed to formatting functions
#' @param export_mps logical; if TRUE, also export MPS format file (default: FALSE)
#' @param stop_on_error logical; if TRUE, stop on validation errors; if FALSE, issue warnings (default: TRUE)
#'
#' @return Character vector or writes file if `file` or `model_dir` is given
#' @export
write_gmpl <- function(model, file = NULL, model_dir = model$base_dir,
                       format_expr = FALSE, include_solve = TRUE,
                      export_vars = NULL, use_table_output = FALSE, use_folded = NULL, objective = "cost",
                      export_data = FALSE, drop_default_values = FALSE, INF = 1e20,
                      check = TRUE, resolve_aliases = TRUE, verbose = FALSE, export_mps = FALSE,
                      stop_on_error = TRUE, ...) {

  # Validate model if requested
  if (check) {
    validation <- validate(model, verbose = verbose, stop_on_error = stop_on_error)
    if (!validation$valid) {
      msg <- paste("Model validation failed:\n",
                   paste(validation$errors, collapse = "\n"))
      if (stop_on_error) stop(msg)
      warning(msg)
    }
    if (length(validation$warnings) > 0) {
      warning("Model validation warnings:\n", paste(validation$warnings, collapse = "\n"))
    }
  }

  # Store resolve_aliases option for as_gmpl methods to access
  old_resolve <- getOption("multimod.resolve_aliases", NULL)
  options(multimod.resolve_aliases = resolve_aliases)
  on.exit(options(multimod.resolve_aliases = old_resolve), add = TRUE)

  # Auto-detect if model is folded when use_folded is NULL
  if (is.null(use_folded)) {
    use_folded <- tryCatch(
      is_folded(model),
      error = function(e) FALSE  # Default to FALSE if check fails
    )
  }

  # If model_dir is provided, create directory structure
  if (!is.null(model_dir)) {
    # Ensure the model_dir itself exists first
    if (!dir.exists(model_dir)) {
      dir.create(model_dir, recursive = TRUE, showWarnings = TRUE)
    }

    gmpl_dir <- file.path(model_dir, "solvers", "gmpl")
    if (!dir.exists(gmpl_dir)) {
      dir.create(gmpl_dir, recursive = TRUE, showWarnings = TRUE)
    }

    # Create solution directory for results if include_solve is TRUE
    solution_dir <- file.path(gmpl_dir, "solution")
    if (!dir.exists(solution_dir)) {
      dir.create(solution_dir, recursive = TRUE, showWarnings = TRUE)
    }

    # Create data_export directory if export_data is TRUE
    if (export_data) {
      export_dir <- file.path(gmpl_dir, "data_export")
      if (!dir.exists(export_dir)) {
        dir.create(export_dir, recursive = TRUE, showWarnings = TRUE)
      }
    }

    mod_file <- file.path(gmpl_dir, "model.mod")
    dat_file <- file.path(gmpl_dir, "data.dat")

    # Set INF option for as_gmpl functions to use
    old_inf <- getOption("multimod.gmpl_inf", NULL)
    options(multimod.gmpl_inf = INF)
    on.exit(options(multimod.gmpl_inf = old_inf), add = TRUE)

    # Write model file
    model_code <- write_gmpl_internal(model, file = NULL, format_expr, include_solve,
                                      export_vars, use_table_output, use_folded, objective, export_data, drop_default_values, ...)
    writeLines(model_code, mod_file)

    # Write data file
    write_gmpl_data(model, file = dat_file, use_folded = use_folded, INF = INF)

    # Save solver configuration for solve_gmpl to use
    config <- list(
      export_vars = !is.null(export_vars) && (isTRUE(export_vars) || length(export_vars) > 0),
      export_mps = isTRUE(export_mps)
    )
    saveRDS(config, file.path(gmpl_dir, ".solver_config.rds"))

    invisible(list(model = mod_file, data = dat_file))
  } else {
    # Original behavior - use method dispatch
    UseMethod("write_gmpl")
  }
}


#' Write a full GMPL model file from a multimod object
#'
#' @param model A `multimod` model object
#' @param file Output file path (optional)
#' @param format_expr logical; whether to format expressions with line breaks
#' @param include_solve logical; whether to include solve and end statements
#' @param export_vars character vector of variable names to export to CSV; if NULL (default), all variables are exported; if FALSE or character(0), no custom export
#' @param use_table_output logical; whether to add GMPL table statement for output (CSV format); default FALSE
#' @param objective character; name of the objective variable (default: "cost")
#' @param export_data logical; whether to insert data export code after parameters (default: FALSE)
#' @param drop_default_values logical; if TRUE with export_data, exclude values that match defaults (default: FALSE)
#' @param ... Additional arguments passed to formatting functions
#'
#' @return Character vector or writes file if `file` is given
#' @method write_gmpl model
#' @export
write_gmpl.model <- function(model, file = NULL, format_expr = FALSE, include_solve = TRUE,
                            export_vars = NULL, use_table_output = FALSE, use_folded = NULL, objective = "cost",
                            export_data = FALSE, drop_default_values = FALSE, ...) {
  stopifnot(inherits(model, "model"))
  # Auto-detect if model is folded
  if (is.null(use_folded)) {
    use_folded <- is_folded(model)
  }
  write_gmpl_internal(model, file, format_expr, include_solve, export_vars, use_table_output, use_folded, objective, export_data, drop_default_values, ...)
}


#' Write a full GMPL model file from a model_structure object
#'
#' @param model A `model_structure` object from read_gams
#' @param file Output file path (optional)
#' @param format_expr logical; whether to format expressions with line breaks
#' @param include_solve logical; whether to include solve and end statements
#' @param export_vars character vector of variable names to export to CSV; if NULL (default), all variables are exported; if FALSE or character(0), no custom export
#' @param use_table_output logical; whether to add GMPL table statement for output (CSV format); default FALSE
#' @param ... Additional arguments passed to formatting functions
#'
#' @return Character vector or writes file if `file` is given
# write_gmpl.model_structure <- function(model, file = NULL, format_expr = FALSE, include_solve = TRUE,
#                                       export_vars = NULL, use_table_output = FALSE, use_folded = NULL, ...) {
#   stopifnot(inherits(model, "model_structure"))
#   # Auto-detect if model is folded
#   if (is.null(use_folded)) {
#     use_folded <- is_folded(model)
#   }
#   write_gmpl_internal(model, file, format_expr, include_solve, export_vars, use_table_output, use_folded, ...)
# }


#' Internal function to write GMPL model
#'
#' @keywords internal
write_gmpl_internal <- function(model, file = NULL, format_expr = FALSE, include_solve = TRUE,
                               export_vars = NULL, use_table_output = FALSE, use_folded = FALSE, objective = "cost",
                               export_data = FALSE, drop_default_values = FALSE, ...) {

  lines <- character()

  # Header with comments
  if (!is.null(model$name) && !is.na(model$name)) {
    lines <- c(lines, paste0("# Model: ", model$name))
  }
  if (!is.null(model$desc) && !is.na(model$desc)) {
    lines <- c(lines, paste0("# ", model$desc))
  }
  lines <- c(lines, "")

  # Sets (basic sets without dimensions)
  for (s in model$sets) {
    # Skip trimmed sets
    if (isTRUE(s$trimmed)) {
      next
    }

    set_line <- tryCatch({
      as_gmpl(s, declaration = TRUE, desc = TRUE)
    }, error = function(e) NULL, warning = function(w) NULL)

    if (!is.null(set_line) && is.character(set_line)) {
      lines <- c(lines, set_line)
    }
  }

  # Add dummy set for conditional evaluation (GAMS $ operator workaround)
  lines <- c(lines, "")
  lines <- c(lines, "")
  lines <- c(lines, "set _IF_;  # Singleton set for GAMS $ operator emulation")

  # NOTE: We do NOT declare set aliases in GMPL
  # Aliases are handled by using the base set name in iterations
  # and the dummy variable mapping takes care of index references

  lines <- c(lines, "")

  # Mappings (sets with dimen N)
  if (!is.null(model$mappings) && length(model$mappings) > 0) {
    for (m in model$mappings) {
      # Skip trimmed mappings
      if (isTRUE(m$trimmed)) {
        next
      }

      mapping_line <- tryCatch({
        as_gmpl(m, declaration = TRUE, desc = TRUE)
      }, error = function(e) NULL, warning = function(w) NULL)

      if (!is.null(mapping_line) && is.character(mapping_line)) {
        lines <- c(lines, mapping_line)
      }
    }
    lines <- c(lines, "")
  }

  # Parameters
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    for (p in model$parameters) {
      # Skip trimmed parameters
      if (isTRUE(p$trimmed)) {
        next
      }

      param_line <- tryCatch({
        as_gmpl(p, declaration = TRUE, desc = TRUE)
      }, error = function(e) NULL, warning = function(w) NULL)

      if (!is.null(param_line) && is.character(param_line)) {
        lines <- c(lines, param_line)
      }
    }
    lines <- c(lines, "")
  }

  # Insert data export code if requested
  if (export_data) {
    export_code <- export_data_gmpl(model, export_dir = "data_export", drop_default_values = drop_default_values)
    lines <- c(lines, strsplit(export_code, "\n")[[1]], "")

    # Note: The data_export directory will be created by the calling function
    # before running glpsol, or the user must ensure it exists
  }

  # Variables
  if (!is.null(model$variables) && length(model$variables) > 0) {
    # Get objective variable names to skip
    obj_var_names <- NULL
    if (!is.null(model$objectives) && length(model$objectives) > 0) {
      obj_var_names <- sapply(model$objectives, function(obj) obj$variable)
      obj_var_names <- obj_var_names[!sapply(obj_var_names, is.null)]
    }

    for (v in model$variables) {
      # Skip trimmed variables
      if (isTRUE(v$trimmed)) {
        next
      }

      # Skip objective variables - they're declared by the objective statement
      if (!is.null(obj_var_names) && v$name %in% obj_var_names) {
        next
      }

      var_line <- tryCatch({
        as_gmpl(v, declaration = TRUE, desc = TRUE)
      }, error = function(e) NULL, warning = function(w) NULL)

      if (!is.null(var_line) && is.character(var_line)) {
        lines <- c(lines, var_line)
      }
    }
    lines <- c(lines, "")
  }

  # Objective function (if specified) - write before constraints
  # In multimod, objectives is a named list: list(obj_name = list(equation = eq_name, variable = var_name))
  # Each objective equation has form: var = expr with sense metadata
  # We write: minimize var: expr
  if (!is.null(model$objectives) && length(model$objectives) > 0) {
    # Select which objective to write
    obj_info <- NULL
    if (!is.null(objective) && is.numeric(objective)) {
      # Select by index
      if (objective > 0 && objective <= length(model$objectives)) {
        obj_info <- model$objectives[[objective]]
      }
    } else if (!is.null(objective) && is.character(objective)) {
      # Select by name (check if it's an objective name)
      if (objective %in% names(model$objectives)) {
        obj_info <- model$objectives[[objective]]
      } else {
        # Maybe it's a variable name, search for matching objective
        for (obj_candidate in model$objectives) {
          if (!is.null(obj_candidate$variable) && obj_candidate$variable == objective) {
            obj_info <- obj_candidate
            break
          }
        }
        # If still not found, use first objective
        if (is.null(obj_info)) {
          obj_info <- model$objectives[[1]]
        }
      }
    } else {
      # Use first objective
      obj_info <- model$objectives[[1]]
    }

    if (!is.null(obj_info) && !is.null(obj_info$equation)) {
      obj_eq_name <- obj_info$equation

      if (obj_eq_name %in% names(model$equations)) {
        obj_eq <- model$equations[[obj_eq_name]]

        # Use as_gmpl.equation with is_objective = TRUE
        obj_statement <- as_gmpl(obj_eq, model = model, is_objective = TRUE, obj_info = obj_info)

        if (!is.null(obj_statement)) {
          lines <- c(lines, obj_statement)
          lines <- c(lines, "")
        } else {
          warning("Could not generate objective statement from equation '", obj_eq_name, "'")
        }
      }
    }
  }

  # Equations (constraints)
  # Use folded_equations if available, otherwise use regular equations
  equations_to_write <- if (!is.null(model$folded_equations) && length(model$folded_equations) > 0) {
    model$folded_equations
  } else {
    model$equations
  }

  if (!is.null(equations_to_write) && length(equations_to_write) > 0) {
    # Get list of objective equation names to skip
    obj_eq_names <- character(0)
    if (!is.null(model$objectives) && length(model$objectives) > 0) {
      obj_eq_names <- sapply(model$objectives, function(obj) obj$equation)
    }

    for (eq in equations_to_write) {
      # Skip trimmed equations
      if (isTRUE(eq$trimmed)) {
        next
      }

      # Skip objective equations (they're written separately)
      if (!is.null(eq$name) && eq$name %in% obj_eq_names) {
        next
      }

      # Add comment for equation description if present
      if (!is.null(eq$desc) && length(eq$desc) > 0 && !is.na(eq$desc) && nchar(eq$desc) > 0) {
        lines <- c(lines, paste0("# ", eq$desc))
      }

      # Pass model to as_gmpl for access to set names (for alias detection)
      gmpl_eq <- as_gmpl(eq, model = model)
      if (format_expr) {
        gmpl_eq <- format_gmpl_expression(gmpl_eq, ...)
      }

      # Ensure gmpl_eq is character
      if (!is.null(gmpl_eq) && is.character(gmpl_eq)) {
        lines <- c(lines, gmpl_eq, "")
      }
    }
  }

  # Solve statement
  if (include_solve) {
    lines <- c(lines, "")
    lines <- c(lines, "solve;")
    lines <- c(lines, "")

    # Export model statistics (constraints and variables)
    lines <- c(lines, "# Export model statistics")
    lines <- c(lines, 'printf "name,count\\n" > "constraint_stats.csv";')
    lines <- c(lines, 'printf "name,count\\n" > "variable_stats.csv";')
    lines <- c(lines, "")

    # Count constraints - get objective equation names to skip
    obj_eq_names <- character(0)
    if (!is.null(model$objectives) && length(model$objectives) > 0) {
      obj_eq_names <- sapply(model$objectives, function(obj) obj$equation)
    }

    # Use folded_equations if available, otherwise use regular equations
    equations_to_count <- if (!is.null(model$folded_equations) && length(model$folded_equations) > 0) {
      model$folded_equations
    } else {
      model$equations
    }

    for (eq in equations_to_count) {
      # Skip trimmed or objective equations
      if (isTRUE(eq$trimmed) || (!is.null(eq$name) && eq$name %in% obj_eq_names)) {
        next
      }

      eq_name <- eq$name
      if (is.null(eq_name) || is.na(eq_name)) next

      # Determine if this is a scalar or indexed equation
      dims <- eq$dims
      if (is.null(dims) || length(dims) == 0) {
        # Scalar constraint
        lines <- c(lines, sprintf('printf "%s,1\\n" >> "constraint_stats.csv";', eq_name))
      } else {
        # Indexed constraint - count actual instances considering domain filters
        # Extract dimension names
        if (inherits(dims, "dims")) {
          dim_names_vec <- vapply(seq_along(dims), function(i) {
            d <- dims[[i]]
            if (inherits(d, "symbol")) d$name
            else if (is.list(d) && !is.null(d$name)) d$name
            else as.character(d)
          }, character(1))
        } else {
          dim_names_vec <- as.character(dims)
        }

        # Resolve aliases to base set names
        if (!is.null(model)) {
          dim_names_vec <- vapply(dim_names_vec, function(dim_nm) {
            resolve_alias_to_set(dim_nm, model)
          }, character(1), USE.NAMES = FALSE)
        }

        # Get domain dimension names (may include aliases)
        if (!is.null(eq$domain) && !is.null(eq$domain$dims) && length(eq$domain$dims) > 0) {
          domain_dim_names <- sapply(eq$domain$dims, function(d) {
            if (inherits(d, "symbol")) d$name
            else if (inherits(d, "set")) d$name
            else if (is.character(d)) d
            else as.character(d)
          })
        } else {
          domain_dim_names <- dim_names_vec
        }

        # Generate iterator variables - use equation's dims_index_aliases if available
        if (!is.null(eq$dims_index_aliases) && length(eq$dims_index_aliases) > 0) {
          iter_vars <- as.character(eq$dims_index_aliases)
        } else if (!is.null(model$index_aliases)) {
          iter_vars <- character(length(domain_dim_names))
          for (i in seq_along(domain_dim_names)) {
            dim_name <- domain_dim_names[i]
            # Check if dimension is an alias and resolve to base set
            base_set <- resolve_alias_to_set(dim_name, model)
            # Use alias from index_aliases
            if (base_set %in% names(model$index_aliases)) {
              iter_vars[i] <- model$index_aliases[[base_set]]
            } else {
              iter_vars[i] <- tolower(substring(base_set, 1, 1))
            }
          }
        } else {
          iter_vars <- tolower(substring(domain_dim_names, 1, 1))
        }

        # Check if there's a domain filter condition
        if (!is.null(eq$domain) && inherits(eq$domain, "expression")) {
          # Has a filter condition - count by summing 1 over filtered domain
          iter_spec <- paste(sprintf("%s in %s", iter_vars, dim_names_vec), collapse = ", ")

          # Convert domain condition to GMPL
          domain_cond <- tryCatch({
            as_gmpl(eq$domain, model = model)
          }, error = function(e) {
            # Fallback: simple condition string
            ""
          })

          if (nchar(domain_cond) > 0) {
            count_expr <- sprintf("sum{%s: %s} 1", iter_spec, domain_cond)
          } else {
            # No valid condition - use Cartesian product
            count_expr <- paste(sprintf("card(%s)", dim_names_vec), collapse = "*")
          }
        } else {
          # No filter condition - use Cartesian product
          count_expr <- paste(sprintf("card(%s)", dim_names_vec), collapse = "*")
        }

        lines <- c(lines, sprintf('printf "%s,%%d\\n", %s >> "constraint_stats.csv";', eq_name, count_expr))
      }
    }

    lines <- c(lines, "")

    # Count variables - get objective variable names to skip
    obj_var_names <- character(0)
    if (!is.null(model$objectives) && length(model$objectives) > 0) {
      obj_var_names <- sapply(model$objectives, function(obj) obj$variable)
      obj_var_names <- obj_var_names[!sapply(obj_var_names, is.null)]
    }

    for (v in model$variables) {
      # Skip trimmed or objective variables
      if (isTRUE(v$trimmed) || (!is.null(v$name) && v$name %in% obj_var_names)) {
        next
      }

      var_name <- v$name
      if (is.null(var_name) || is.na(var_name)) next

      # Determine if this is a scalar or indexed variable
      dims <- v$dims
      if (is.null(dims) || length(dims) == 0) {
        # Scalar variable
        lines <- c(lines, sprintf('printf "%s,1\\n" >> "variable_stats.csv";', var_name))
      } else {
        # Indexed variable - count cardinality of index set
        # Extract dimension names
        if (inherits(dims, "dims")) {
          dim_names_vec <- vapply(seq_along(dims), function(i) {
            d <- dims[[i]]
            if (inherits(d, "symbol")) d$name
            else if (is.list(d) && !is.null(d$name)) d$name
            else as.character(d)
          }, character(1))
        } else {
          dim_names_vec <- as.character(dims)
        }

        if (!is.null(model)) {
          dim_names_vec <- vapply(dim_names_vec, function(dim_nm) {
            resolve_alias_to_set(dim_nm, model)
          }, character(1), USE.NAMES = FALSE)
        }

        # For multi-dimensional variables, compute product of cardinalities
        count_expr <- paste(sprintf("card(%s)", dim_names_vec), collapse = "*")
        lines <- c(lines, sprintf('printf "%s,%%d\\n", %s >> "variable_stats.csv";', var_name, count_expr))
      }
    }

    lines <- c(lines, "")
    lines <- c(lines, "end;")

    # Determine which variables to export
    do_custom_export <- !identical(export_vars, FALSE) && !identical(export_vars, character(0))

    if (do_custom_export || use_table_output) {
      lines <- c(lines, "")
      lines <- c(lines, '# Export results to output directory')

      # Log timestamps
      lines <- c(lines, 'printf  \'"solution status",1,"%s"\\n\', time2str(gmtime(), "%Y-%m-%d %M:%H:S %TZ") >> "solution/log.csv";')
      lines <- c(lines, 'printf  \'"export results",,"%s"\\n\', time2str(gmtime(), "%Y-%m-%d %M:%H:S %TZ") >> "solution/log.csv";')
    }

    # Custom CSV export (default behavior)
    if (do_custom_export) {
      # Determine which variables to export
      if (is.null(export_vars) || isTRUE(export_vars)) {
        # NULL or TRUE means export all variables
        vars_to_export <- model$variables
      } else {
        # Export only specified variables
        var_names <- sapply(model$variables, function(v) v$name)
        vars_to_export <- model$variables[var_names %in% export_vars]
      }

      # Generate output code for each variable
      if (!is.null(vars_to_export) && length(vars_to_export) > 0) {
        for (v in vars_to_export) {
          # Skip trimmed variables
          if (isTRUE(v$trimmed)) {
            next
          }

          output_code <- generate_variable_output(v)
          if (!is.null(output_code) && is.character(output_code) && length(output_code) > 0) {
            # Filter out any NA or NULL elements
            output_code <- output_code[!is.na(output_code)]
            if (length(output_code) > 0) {
              lines <- c(lines, output_code)
            }
          }
        }
      }

      # Variable list
      lines <- c(lines, "")
      lines <- c(lines, 'printf "value\\n" > "solution/variable_list.csv";')
      if (!is.null(vars_to_export) && length(vars_to_export) > 0) {
        for (v in vars_to_export) {
          # Skip trimmed variables
          if (isTRUE(v$trimmed)) {
            next
          }
          lines <- c(lines, sprintf('    printf "%s\\n" >> "solution/variable_list.csv";', v$name))
        }
      }

      # Export set members
      lines <- c(lines, "")
      lines <- c(lines, 'printf "set,value\\n" > "solution/raw_data_set.csv";')
      if (!is.null(model$sets) && length(model$sets) > 0) {
        for (s in model$sets) {
          # Skip trimmed sets
          if (isTRUE(s$trimmed)) {
            next
          }
          lines <- c(lines, sprintf('for {%s in %s} {', substr(s$name, 1, 1), s$name))
          lines <- c(lines, sprintf('    printf "%s,%%s\\n", %s >> "solution/raw_data_set.csv";', s$name, substr(s$name, 1, 1)))
          lines <- c(lines, '}')
        }
      }
    }

    # GMPL table statement for output
    if (use_table_output) {
      lines <- c(lines, "")
      lines <- c(lines, "# GMPL table output statement")
      lines <- c(lines, 'table tout {i in 1..1} OUT "CSV" "solution/table_output.csv" : ')

      # Add all variables to table export (skip trimmed)
      if (!is.null(model$variables) && length(model$variables) > 0) {
        var_names <- sapply(model$variables, function(v) {
          if (isTRUE(v$trimmed)) return(NULL)
          v$name
        })
        var_names <- var_names[!sapply(var_names, is.null)]
        if (length(var_names) > 0) {
          table_vars <- paste(var_names, collapse = ", ")
          lines <- c(lines, paste0("  ", table_vars, ";"))
        }
      }

      lines <- c(lines, "")
    }

    # Final log entry and end
    if (do_custom_export || use_table_output) {
      lines <- c(lines, 'printf  \'"done",,"%s"\\n\', time2str(gmtime(), "%Y-%m-%d %M:%H:S %TZ") >> "solution/log.csv";')
    }

    lines <- c(lines, "")
    lines <- c(lines, "end;")
  }

  # Output
  if (!is.null(file)) {
    writeLines(lines, con = file, useBytes = TRUE)
  } else {
    return(lines)
  }
}


#' Format GMPL expression with line breaks and indentation
#'
#' @param gmpl_lines Character vector of GMPL code lines
#' @param indent Number of spaces for indentation
#' @param ... Additional arguments (not used)
#'
#' @return Formatted character vector
#' @keywords internal
format_gmpl_expression <- function(gmpl_lines, indent = 2, ...) {
  # For now, return as-is
  # More sophisticated formatting can be added later if needed
  gmpl_lines
}


#' Generate GMPL output code for a variable
#'
#' Creates printf statements to export variable values to CSV file
#'
#' @param var A variable object with name and dims
#'
#' @return Character vector of GMPL output code
#' @keywords internal
generate_variable_output <- function(var) {
  if (is.null(var$name)) return(NULL)

  var_name <- var$name
  dims <- var$dims

  # Skip vObjective - handled separately
  if (var_name == "vObjective") {
    return(c(
      sprintf('printf "value\\n%%s\\n",%s > "solution/%s.csv";', var_name, var_name),
      ""
    ))
  }

  # If no dimensions, simple scalar variable
  if (is.null(dims) || length(dims) == 0) {
    return(c(
      sprintf('printf "value\\n%%f\\n",%s > "solution/%s.csv";', var_name, var_name),
      ""
    ))
  }

  # Extract dimension names from dims object (list of symbol objects)
  if (inherits(dims, "dims")) {
    dim_names_vec <- vapply(seq_along(dims), function(i) {
      d <- dims[[i]]
      if (inherits(d, "symbol")) d$name
      else if (is.list(d) && !is.null(d$name)) d$name
      else as.character(d)
    }, character(1))
  } else {
    dim_names_vec <- as.character(dims)
  }

  # Build header with dimension names
  dim_names <- paste(dim_names_vec, collapse = ",")
  header <- paste0(dim_names, ",value")

  # Build iterator: (d1, d2, ...) in mapping
  # Need to determine the mapping name from dimensions
  # For now, assume there's a mapping with these dimensions
  # In practice, we'd need to look this up in the model

  # Generate dummy iterator variable names from dimension names
  iter_vars <- sapply(seq_along(dim_names_vec), function(i) {
    d <- dim_names_vec[i]
    # Use first letter if set name, or abbreviated form
    if (nchar(d) > 0) {
      # Common convention: tech->t, comm->c, region->r, year->y, slice->s, etc.
      abbrev <- switch(d,
        "tech" = "t",
        "comm" = "c",
        "region" = "r",
        "year" = "y",
        "slice" = "s",
        "sup" = "s1",
        "dem" = "d",
        "stg" = "st1",
        "trade" = "t1",
        "expp" = "e",
        "imp" = "i",
        "slicep" = "sp",
        "yearp" = "yp",
        "src" = "src",
        "dst" = "dst",
        substr(d, 1, 1)  # Default: first letter
      )
      # Handle duplicate iterator names by adding suffix
      count <- sum(dim_names_vec[1:i] == d)
      if (count > 1) {
        abbrev <- paste0(abbrev, count)
      }
      abbrev
    } else {
      "i"
    }
  })

  # Build the iterator tuple
  if (length(iter_vars) == 1) {
    iterator <- sprintf("%s in %s", iter_vars[1], dim_names_vec[1])
  } else {
    # For multi-dimensional variables, iterate over Cartesian product of base sets
    # The "<> 0" condition will filter to only non-zero values
    # This avoids needing to find the right mapping name
    iterators <- sapply(seq_along(iter_vars), function(i) {
      sprintf("%s in %s", iter_vars[i], dim_names_vec[i])
    })
    # Create nested iteration: for{i1 in set1, i2 in set2, ...}
    iterator <- paste(iterators, collapse = ", ")
  }

  # Build the index reference: var[i1,i2,...]
  if (length(iter_vars) == 1) {
    var_ref <- sprintf("%s[%s]", var_name, iter_vars[1])
  } else {
    indices <- paste(iter_vars, collapse = ",")
    var_ref <- sprintf("%s[%s]", var_name, indices)
  }

  # Build printf format string
  format_str <- paste(rep("%s", length(iter_vars)), collapse = ",")
  format_str <- paste0(format_str, ",%f")

  # Build printf arguments
  printf_args <- paste(iter_vars, collapse = ",")
  printf_args <- paste0(printf_args, ",", var_ref)

  # Generate output code
  lines <- c(
    sprintf('printf "%s\\n" > "solution/%s.csv";', header, var_name),
    sprintf('for{%s : %s <> 0} {', iterator, var_ref),
    sprintf('  printf "%s\\n", %s >> "solution/%s.csv";', format_str, printf_args, var_name),
    '}',
    ""
  )

  lines
}

#' Generate GMPL code to export all sets and parameters to CSV files
#'
#' Creates GMPL printf statements that export all sets and parameters in a model
#' to CSV files in a data_export/ directory using standard CSV format (comma-separated
#' with headers). This code can be injected into a GMPL model file before the solve statement.
#'
#' @param model A model object with $sets and $parameters
#' @param export_dir Directory name for exports (default: "data_export")
#' @param drop_default_values logical; if TRUE, exclude values that match the default (default: FALSE)
#' @return Character string with GMPL export statements
#' @export
#'
#' @examples
#' \dontrun{
#' model <- read_gmpl("model.mod", "data.dat")
#' export_code <- export_data_gmpl(model)
#' cat(export_code)
#' }
export_data_gmpl <- function(model, export_dir = "data_export", drop_default_values = FALSE) {

  if (is.null(model$sets) || is.null(model$parameters)) {
    stop("Model must have $sets and $parameters")
  }

  # Helper function to extract dimension names from dims object
  get_dim_names <- function(dims) {
    if (is.null(dims) || length(dims) == 0) {
      return(character(0))
    }
    vapply(dims, function(d) {
      if (is.list(d) && !is.null(d$name)) {
        return(d$name)
      } else if (is.character(d)) {
        return(d)
      } else {
        return(as.character(d))
      }
    }, character(1))
  }

  lines <- c(
    "# ========== DATA EXPORT ==========",
    sprintf('printf "Exporting model data to %s/...\\n";', export_dir),
    ""
  )

  # Export sets
  if (length(model$sets) > 0) {
    lines <- c(lines, "# Export sets")

    for (set_name in names(model$sets)) {
      set_info <- model$sets[[set_name]]

      # Skip if it's a mapping or has dims (those are derived sets)
      if (!is.null(set_info$dims) && length(set_info$dims) > 0) {
        next
      }

      csv_file <- sprintf("%s/%s.csv", export_dir, set_name)

      # 1D set export with header
      lines <- c(lines,
        sprintf('printf "%s\\n" > "%s";', set_name, csv_file),
        sprintf('for {i in %s} { printf "%%s\\n", i >> "%s"; }',
                set_name, csv_file)
      )
    }
    lines <- c(lines, "")
  }

  # Export parameters
  if (length(model$parameters) > 0) {
    lines <- c(lines, "# Export parameters")

    for (param_name in names(model$parameters)) {
      param_info <- model$parameters[[param_name]]

      # Skip trimmed parameters
      if (isTRUE(param_info$trimmed)) {
        next
      }

      # Skip symbolic parameters (they're strings, not numeric)
      if (!is.null(param_info$symbolic) && param_info$symbolic) {
        next
      }

      csv_file <- sprintf("%s/%s.csv", export_dir, param_name)
      dims <- get_dim_names(param_info$dims)
      # # For folded parameters, use active_dims instead of dims
      # dims <- if (!is.null(param_info$active_dims) && length(param_info$active_dims) > 0) {
      #   get_dim_names(param_info$active_dims)
      # } else {
      #   get_dim_names(param_info$dims)
      # }

      # Get default value if present
      default_val <- param_info$defVal

      # Handle ast_formula - these are expressions, not simple defaults
      if (inherits(default_val, "ast_formula")) {
        # For export purposes, treat as no default (expression can't be used as filter)
        has_default <- FALSE
      } else {
        has_default <- !is.null(default_val) && !is.na(default_val)
      }

      if (is.null(dims) || length(dims) == 0) {
        # 0D parameter (scalar)
        lines <- c(lines,
          sprintf('printf "value\\n" > "%s";', csv_file),
          sprintf('printf "%%g\\n", %s >> "%s";', param_name, csv_file)
        )

      } else if (length(dims) == 1) {
        # 1D parameter
        idx_var <- "i"
        header <- sprintf("%s,value", dims[1])
        export_lines <- sprintf('printf "%s\\n" > "%s";', header, csv_file)

        # Add default value in first row with empty set column
        if (has_default) {
          default_str <- if (is.character(default_val)) default_val else sprintf("%g", default_val)
          export_lines <- c(export_lines,
            sprintf('printf ",%s\\n" >> "%s";', default_str, csv_file)
          )
        }

        # Build condition: either <> 0 or <> default if dropping defaults
        if (drop_default_values && has_default && !is.character(default_val)) {
          condition <- sprintf("%s[%s] <> %g", param_name, idx_var, default_val)
        } else {
          condition <- sprintf("%s[%s] <> 0", param_name, idx_var)
        }

        export_lines <- c(export_lines,
          sprintf('for {%s in %s: %s} { printf "%%s,%%g\\n", %s, %s[%s] >> "%s"; }',
                  idx_var, dims[1], condition, idx_var, param_name, idx_var, csv_file)
        )
        lines <- c(lines, export_lines)

      } else if (length(dims) == 2) {
        # 2D parameter
        idx_var1 <- "i"
        idx_var2 <- "j"
        header <- sprintf("%s,%s,value", dims[1], dims[2])
        export_lines <- sprintf('printf "%s\\n" > "%s";', header, csv_file)

        # Add default value in first row with empty set columns
        if (has_default) {
          default_str <- if (is.character(default_val)) default_val else sprintf("%g", default_val)
          export_lines <- c(export_lines,
            sprintf('printf ",,%s\\n" >> "%s";', default_str, csv_file)
          )
        }

        # Build condition: either <> 0 or <> default if dropping defaults
        if (drop_default_values && has_default && !is.character(default_val)) {
          condition <- sprintf("%s[%s,%s] <> %g", param_name, idx_var1, idx_var2, default_val)
        } else {
          condition <- sprintf("%s[%s,%s] <> 0", param_name, idx_var1, idx_var2)
        }

        export_lines <- c(export_lines,
          sprintf('for {%s in %s, %s in %s: %s} { printf "%%s,%%s,%%g\\n", %s, %s, %s[%s,%s] >> "%s"; }',
                  idx_var1, dims[1], idx_var2, dims[2], condition,
                  idx_var1, idx_var2, param_name, idx_var1, idx_var2, csv_file)
        )
        lines <- c(lines, export_lines)

      } else if (length(dims) == 3) {
        # 3D parameter
        idx_var1 <- "i"
        idx_var2 <- "j"
        idx_var3 <- "k"
        header <- sprintf("%s,%s,%s,value", dims[1], dims[2], dims[3])
        export_lines <- sprintf('printf "%s\\n" > "%s";', header, csv_file)

        # Add default value in first row with empty set columns
        if (has_default) {
          default_str <- if (is.character(default_val)) default_val else sprintf("%g", default_val)
          export_lines <- c(export_lines,
            sprintf('printf ",,,%s\\n" >> "%s";', default_str, csv_file)
          )
        }

        # Build condition: either <> 0 or <> default if dropping defaults
        if (drop_default_values && has_default && !is.character(default_val)) {
          condition <- sprintf("%s[%s,%s,%s] <> %g", param_name, idx_var1, idx_var2, idx_var3, default_val)
        } else {
          condition <- sprintf("%s[%s,%s,%s] <> 0", param_name, idx_var1, idx_var2, idx_var3)
        }

        export_lines <- c(export_lines,
          sprintf('for {%s in %s, %s in %s, %s in %s: %s} { printf "%%s,%%s,%%s,%%g\\n", %s, %s, %s, %s[%s,%s,%s] >> "%s"; }',
                  idx_var1, dims[1], idx_var2, dims[2], idx_var3, dims[3],
                  condition,
                  idx_var1, idx_var2, idx_var3, param_name,
                  idx_var1, idx_var2, idx_var3, csv_file)
        )
        lines <- c(lines, export_lines)

      } else if (length(dims) == 4) {
        # 4D parameter
        idx_vars <- c("i", "j", "k", "l")
        header <- paste(c(dims, "value"), collapse = ",")
        iter_str <- paste(sprintf("%s in %s", idx_vars, dims), collapse = ", ")
        param_ref <- sprintf("%s[%s]", param_name, paste(idx_vars, collapse = ","))
        printf_format <- paste(rep("%s", 4), collapse = ",")
        printf_args <- paste(c(idx_vars, param_ref), collapse = ", ")

        export_lines <- sprintf('printf "%s\\n" > "%s";', header, csv_file)

        # Add default value with empty set columns
        if (has_default) {
          default_str <- if (is.character(default_val)) default_val else sprintf("%g", default_val)
          empty_cols <- paste(rep(",", 4), collapse = "")
          export_lines <- c(export_lines,
            sprintf('printf "%s%s\\n" >> "%s";', empty_cols, default_str, csv_file)
          )
        }

        # Build condition: either <> 0 or <> default if dropping defaults
        if (drop_default_values && has_default && !is.character(default_val)) {
          condition <- sprintf("%s <> %g", param_ref, default_val)
        } else {
          condition <- sprintf("%s <> 0", param_ref)
        }

        export_lines <- c(export_lines,
          sprintf('for {%s: %s} { printf "%s,%%g\\n", %s >> "%s"; }',
                  iter_str, condition, printf_format, printf_args, csv_file)
        )
        lines <- c(lines, export_lines)

      } else if (length(dims) == 5) {
        # 5D parameter
        idx_vars <- c("i", "j", "k", "l", "m")
        header <- paste(c(dims, "value"), collapse = ",")
        iter_str <- paste(sprintf("%s in %s", idx_vars, dims), collapse = ", ")
        param_ref <- sprintf("%s[%s]", param_name, paste(idx_vars, collapse = ","))
        printf_format <- paste(rep("%s", 5), collapse = ",")
        printf_args <- paste(c(idx_vars, param_ref), collapse = ", ")

        export_lines <- sprintf('printf "%s\\n" > "%s";', header, csv_file)

        # Add default value with empty set columns
        if (has_default) {
          default_str <- if (is.character(default_val)) default_val else sprintf("%g", default_val)
          empty_cols <- paste(rep(",", 5), collapse = "")
          export_lines <- c(export_lines,
            sprintf('printf "%s%s\\n" >> "%s";', empty_cols, default_str, csv_file)
          )
        }

        # Build condition: either <> 0 or <> default if dropping defaults
        if (drop_default_values && has_default && !is.character(default_val)) {
          condition <- sprintf("%s <> %g", param_ref, default_val)
        } else {
          condition <- sprintf("%s <> 0", param_ref)
        }

        export_lines <- c(export_lines,
          sprintf('for {%s: %s} { printf "%s,%%g\\n", %s >> "%s"; }',
                  iter_str, condition, printf_format, printf_args, csv_file)
        )
        lines <- c(lines, export_lines)

      } else {
        # 6+ dimensions - generalize
        n_dims <- length(dims)
        idx_vars <- letters[9:(8+n_dims)]  # i, j, k, l, m, n, o, ...
        header <- paste(c(dims, "value"), collapse = ",")
        iter_str <- paste(sprintf("%s in %s", idx_vars, dims), collapse = ", ")
        param_ref <- sprintf("%s[%s]", param_name, paste(idx_vars, collapse = ","))
        printf_format <- paste(rep("%s", n_dims), collapse = ",")
        printf_args <- paste(c(idx_vars, param_ref), collapse = ", ")

        export_lines <- sprintf('printf "%s\\n" > "%s";', header, csv_file)

        # Add default value with empty set columns
        if (has_default) {
          default_str <- if (is.character(default_val)) default_val else sprintf("%g", default_val)
          empty_cols <- paste(rep(",", n_dims), collapse = "")
          export_lines <- c(export_lines,
            sprintf('printf "%s%s\\n" >> "%s";', empty_cols, default_str, csv_file)
          )
        }

        # Build condition: either <> 0 or <> default if dropping defaults
        if (drop_default_values && has_default && !is.character(default_val)) {
          condition <- sprintf("%s <> %g", param_ref, default_val)
        } else {
          condition <- sprintf("%s <> 0", param_ref)
        }

        export_lines <- c(export_lines,
          sprintf('for {%s: %s <> 0} { printf "%s,%%g\\n", %s >> "%s"; }',
                  iter_str, param_ref, printf_format, printf_args, csv_file)
        )
        lines <- c(lines, export_lines)
      }
    }
    lines <- c(lines, "")
  }

  lines <- c(lines,
    'printf "Data export complete!\\n";',
    "# ========== END DATA EXPORT =========="
  )

  return(paste(lines, collapse = "\n"))
}

#' Write GMPL data file from multimod model
#'
#' Creates a .dat file with set members, mapping tuples, and parameter values.
#' Supports lazy loading - data is loaded from disk only when needed.
#' If model has been folded, uses folded_data by default for reduced file size.
#'
#' @param model A multimod model object
#' @param file Output file path (optional)
#' @param use_folded Logical; use folded_data if available (default: TRUE)
#' @param INF numeric; value to use for Inf in parameter defaults (default: 1e20)
#' @return Character vector or writes file if `file` is given
#'
#' @export
write_gmpl_data <- function(model, file = NULL, use_folded = TRUE, INF = 1e20) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))

  lines <- character()

  # Add conditional set (for GAMS $ operator emulation)
  lines <- c(lines, "set _IF_ := _IF_;")
  lines <- c(lines, "")

  # Write basic sets (1-dimensional)
  if (!is.null(model$sets) && length(model$sets) > 0) {
    for (s in model$sets) {
      # Skip trimmed sets
      if (isTRUE(s$trimmed)) {
        next
      }

      set_data <- write_set_data(s, model)
      if (!is.null(set_data)) {
        lines <- c(lines, set_data, "")
      }
    }
  }

  # Write mappings (multi-dimensional sets)
  if (!is.null(model$mappings) && length(model$mappings) > 0) {
    for (m in model$mappings) {
      # Skip trimmed mappings
      if (isTRUE(m$trimmed)) {
        next
      }

      mapping_data <- write_mapping_data(m, model)
      if (!is.null(mapping_data)) {
        lines <- c(lines, mapping_data, "")
      }
    }
  }

  # Write parameters
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    for (p in model$parameters) {
      # Skip trimmed parameters
      if (isTRUE(p$trimmed)) {
        next
      }

      param_data <- write_parameter_data(p, model, use_folded = use_folded)
      if (!is.null(param_data)) {
        lines <- c(lines, param_data, "")
      }
    }
  }

  # Output
  if (!is.null(file)) {
    # Add end statement
    lines <- c(lines, "", "end;")
    writeLines(lines, con = file, useBytes = TRUE)
    invisible(NULL)
  } else {
    return(lines)
  }
}


#' Write set data (members)
#'
#' @param set_obj Set object
#' @param model Model object (for base_path if needed)
#' @return Character vector of GMPL data statements
#'
#' @keywords internal
write_set_data <- function(set_obj, model = NULL) {
  set_name <- set_obj$name
  if (is.null(set_name)) return(NULL)

  # Use direct data if available
  if (!is.null(set_obj$data) && length(set_obj$data) > 0) {
    members <- unique(sort(as.character(set_obj$data)))
    return(paste0("set ", set_name, " := ", paste(members, collapse = " "), ";"))
  }

  # Fallback: extract unique members from mappings and parameters
  members <- character()

  # From mappings
  if (!is.null(model$mappings)) {
    for (m in model$mappings) {
      # Skip trimmed mappings
      if (isTRUE(m$trimmed)) {
        next
      }

      m_data <- get_data_for_writing(m, model)
      if (!is.null(m_data) && nrow(m_data) > 0) {
        # Check if this set appears in mapping dimensions
        m_dims <- if (!is.null(m$dims)) {
          if (is.character(m$dims)) m$dims else as.character(m$dims)
        } else {
          NULL
        }
        if (!is.null(m_dims) && length(m_dims) > 0) {
          idx <- which(m_dims == set_name)
          if (length(idx) > 0 && idx[1] <= ncol(m_data)) {
            members <- c(members, as.character(m_data[[idx[1]]]))
          }
        }
      }
    }
  }

  # From parameters
  if (!is.null(model$parameters)) {
    for (p in model$parameters) {
      # Skip trimmed parameters
      if (isTRUE(p$trimmed)) {
        next
      }

      p_data <- get_data_for_writing(p, model)
      if (!is.null(p_data) && nrow(p_data) > 0) {
        # Check if this set appears in parameter dimensions
        p_dims <- if (is.character(p$dims)) p$dims else as.character(p$dims)
        if (!is.null(p_dims) && length(p_dims) > 0) {
          idx <- which(p_dims == set_name)
          if (length(idx) > 0 && idx[1] <= ncol(p_data)) {
            members <- c(members, as.character(p_data[[idx[1]]]))
          }
        }
      }
    }
  }

  # Write set even if empty (GMPL requires all sets to be declared)
  if (length(members) == 0) {
    return(paste0("set ", set_name, " := ;"))
  }

  # Get unique members and sort
  members <- sort(unique(members))

  # Format as GMPL set statement
  paste0("set ", set_name, " := ", paste(members, collapse = " "), ";")
}


#' Write mapping data (tuples)
#'
#' @param mapping_obj Mapping object
#' @param model Model object (for lazy loading)
#' @return Character vector of GMPL data statements
#'
#' @keywords internal
write_mapping_data <- function(mapping_obj, model = NULL) {
  mapping_name <- mapping_obj$name
  if (is.null(mapping_name)) return(NULL)

  # Get data (with lazy loading support)
  data <- get_data_for_writing(mapping_obj, model)

  # If no data, write as empty set
  if (is.null(data) || nrow(data) == 0) {
    return(paste0("set ", mapping_name, " := ;"))
  }

  # Special case: 1-dimensional mapping (GAMS subset)
  # Write as simple set instead of tuples
  if (length(mapping_obj$dims) == 1) {
    members <- unique(sort(as.character(data[[1]])))
    return(paste0("set ", mapping_name, " := ", paste(members, collapse = " "), ";"))
  }

  # Multi-dimensional: format as tuples
  lines <- paste0("set ", mapping_name, " := ")

  # Each row is a tuple
  for (i in seq_len(nrow(data))) {
    row_vals <- as.character(unlist(data[i, ]))
    tuple <- paste(row_vals, collapse = " ")  # Space-separated tuple elements
    if (i < nrow(data)) {
      lines <- c(lines, paste0(tuple, ","))
    } else {
      lines <- c(lines, paste0(tuple, ";"))
    }
  }

  lines
}


#' Write parameter data (values over dimensions)
#'
#' @param param_obj Parameter object
#' @param model Model object (for lazy loading)
#' @param use_folded Logical; use folded_data if available (default: TRUE)
#' @param validate_timestamp Logical; validate fold timestamp (default: TRUE)
#' @return Character vector of GMPL data statements
#'
#' @keywords internal
write_parameter_data <- function(param_obj, model = NULL,
                                 use_folded = TRUE,
                                 validate_timestamp = TRUE) {
  param_name <- param_obj$name
  if (is.null(param_name)) return(NULL)

  # Determine which data to use: folded or original
  use_folded_data <- FALSE
  if (use_folded && !is.null(param_obj$folded_data)) {
    # Safety check: validate fold timestamp
    if (validate_timestamp && !is.null(param_obj$misc$fold_info$fold_timestamp)) {
      # For now, assume fold is valid if timestamp exists
      # Future: could add data modification tracking
      use_folded_data <- TRUE
    } else if (!validate_timestamp) {
      use_folded_data <- TRUE
    }
  }

  # Get data and dimensions based on fold state
  if (use_folded_data) {
    data <- param_obj$folded_data
    dims <- param_obj$active_dims
  } else {
    # Get data (with lazy loading support)
    data <- get_data_for_writing(param_obj, model)
    dims <- param_obj$dims
  }

  n_dims <- if (!is.null(dims)) length(dims) else 0

  # Get default value (may be stored in original object)
  default_val <- if (!is.null(param_obj$defVal)) {
    param_obj$defVal
  } else {
    0  # fallback to 0
  }

  # Handle ast_formula - treat as if default is in model file
  if (inherits(default_val, "ast_formula")) {
    # ast_formula means default is an expression, already in model file
    has_infinite_default <- FALSE
    has_model_default <- TRUE
  } else {
    # Check if default is Inf (don't set default in GMPL for Inf)
    has_infinite_default <- is.infinite(default_val)
    has_model_default <- !is.null(param_obj$defVal)
  }

  # Filter out Inf values from data (GLPK cannot handle Inf)
  if (!is.null(data) && nrow(data) > 0) {
    # Use 'value' column if it exists, otherwise use last column
    value_col <- if ("value" %in% names(data)) {
      which(names(data) == "value")
    } else {
      ncol(data)
    }
    finite_rows <- is.finite(data[[value_col]])
    if (any(!finite_rows)) {
      data <- data[finite_rows, , drop = FALSE]
    }
  }

  # If no data after filtering, skip if there's a default (already in model file)
  # or write empty assignment
  if (is.null(data) || nrow(data) == 0) {
    # If parameter has default value or formula, it's already declared in model file
    # Don't write duplicate default in data file
    if (!is.null(param_obj$defVal) || !is.null(param_obj$formula)) {
      return(NULL)
    }
    # Otherwise write empty assignment
    if (has_infinite_default) {
      # No default for Inf - parameter is unbounded
      return(paste0("param ", param_name, " := ;"))
    } else {
      return(paste0("param ", param_name, " default ", format(default_val, scientific = FALSE), " := ;"))
    }
  }

  # Format depends on dimensionality
  if (n_dims == 0) {
    # Scalar parameter (no default needed)
    # Always use 'value' column, not last column (which might be n_original for folded data)
    value <- if ("value" %in% names(data)) {
      data[["value"]][1]
    } else {
      data[[ncol(data)]][1]  # Fallback to last column
    }
    return(paste0("param ", param_name, " := ", format(value, scientific = FALSE, digits = 17), ";"))
  }

  # Multi-dimensional parameter with data
  # GMPL format: param name [default <val>] := [index1,index2,...] value ...;
  # Only include default if it's NOT already in the model declaration
  # (i.e., if param doesn't have defVal set, meaning default wasn't in model file)

  if (has_model_default) {
    # Default already in model file, don't repeat it
    lines <- paste0("param ", param_name, " := ")
  } else if (has_infinite_default) {
    # No default for Inf - parameter is unbounded
    lines <- paste0("param ", param_name, " := ")
  } else {
    # No default in model, provide one in data file
    lines <- paste0("param ", param_name, " default ", format(default_val, scientific = FALSE), " := ")
  }

  for (i in seq_len(nrow(data))) {
    # Extract index values and value
    if (n_dims > 0) {
      indices <- as.character(unlist(data[i, 1:n_dims]))
      index_str <- paste(indices, collapse = ",")  # Comma-separated indices
    } else {
      index_str <- ""
    }

    # Always use 'value' column, not last column (which might be n_original for folded data)
    value <- if ("value" %in% names(data)) {
      data[["value"]][i]
    } else {
      data[[ncol(data)]][i]  # Fallback to last column
    }

    # No comma after the last value, semicolon instead
    if (i < nrow(data)) {
      if (n_dims > 0) {
        lines <- c(lines, paste0("[", index_str, "] ", format(value, scientific = FALSE, digits = 17)))
      } else {
        lines <- c(lines, format(value, scientific = FALSE, digits = 17))
      }
    } else {
      if (n_dims > 0) {
        lines <- c(lines, paste0("[", index_str, "] ", format(value, scientific = FALSE, digits = 17), ";"))
      } else {
        lines <- c(lines, paste0(format(value, scientific = FALSE, digits = 17), ";"))
      }
    }
  }

  lines
}


#' Get data for writing (with lazy loading support)
#'
#' Loads data from disk if needed, or returns in-memory data
#'
#' @param obj Parameter or mapping object
#' @param model Model object (for base_path)
#' @return Data frame with data
#'
#' @keywords internal
get_data_for_writing <- function(obj, model = NULL) {
  # Check if data is in memory
  if (!is.null(obj$data) && nrow(obj$data) > 0) {
    return(obj$data)
  }

  # Try lazy loading
  if (!is.null(model)) {
    data <- tryCatch({
      get_lazy_data(obj, base_path = model$base_path, collect = TRUE)
    }, error = function(e) {
      NULL
    })

    if (!is.null(data) && nrow(data) > 0) {
      return(data)
    }
  }

  NULL
}



