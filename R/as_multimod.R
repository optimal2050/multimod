#' Convert an object to a multimod structure
#'
#' This is a generic function that converts an object to a multimod-compatible format.
#'
#' @param x An object to convert.
#' @param ... Additional arguments passed to methods.
#'
#' @return A multimod object.
#' @export
as_multimod <- function(x, ...) {
  UseMethod("as_multimod")
}

#' @export
as_multimod.default <- function(x, ...) {
  stop("No as_multimod method for object of class: ", class(x))
}

#' @export
as_multimod.model_structure <- function(x, index_aliases = NULL, ...) {
  stopifnot(inherits(x, "model_structure"))
  # browser()
  args <- list(...)

  if (!is.null(args$name)) {
    name <- args$name; args$name <- NULL
  } else {
    name <- x$name
  }
  if (!is.null(args$desc)) {
    desc <- args$desc; args$desc <- NULL
  } else {
    desc <- x$desc
  }

  # Build symbol list for expression parsing
  symbols <- build_symbols_list(x)

  # Convert sets and aliases
  sets <- lapply(x$sets, function(s) {
    new_set(name = s$name, desc = s$desc, subset_of = s$subset_of)
  })

  # Reorganize aliases: combine all aliases by base set name
  # Input: list of character vectors like c("REGION", "r"), c("REGION", "region"), c("r", "dst")
  # Output: named list like list(REGION = c("REGION", "r", "region", "dst", "src"))
  aliases <- if (!is.null(x$aliases) && length(x$aliases) > 0) {
    # Collect all alias relationships
    alias_map <- list()

    for (alias_pair in x$aliases) {
      # Skip if NULL or empty
      if (is.null(alias_pair) || length(alias_pair) == 0) next
      
      # Convert list to character vector if needed
      if (is.list(alias_pair)) {
        alias_pair <- unlist(alias_pair, use.names = FALSE)
      }
      
      # Ensure it's a character vector
      if (!is.character(alias_pair)) {
        alias_pair <- as.character(alias_pair)
      }
      
      if (length(alias_pair) < 2) next

      # Find which existing group this belongs to (if any)
      found_group <- NULL
      for (group_name in names(alias_map)) {
        if (any(alias_pair %in% alias_map[[group_name]])) {
          found_group <- group_name
          break
        }
      }

      if (!is.null(found_group)) {
        # Add new aliases to existing group
        alias_map[[found_group]] <- unique(c(alias_map[[found_group]], alias_pair))
      } else {
        # Create new group - use first element as the base name
        # Prefer uppercase names as base (set names are typically uppercase)
        base_name <- if (any(alias_pair == toupper(alias_pair))) {
          alias_pair[alias_pair == toupper(alias_pair)][1]
        } else {
          alias_pair[1]
        }
        alias_map[[base_name]] <- unique(alias_pair)
      }
    }

    # Ensure base name is always first in each group, convert to list
    result <- lapply(names(alias_map), function(base_name) {
      group <- alias_map[[base_name]]
      # Put base name first, then others
      c(base_name, setdiff(group, base_name))
    })
    names(result) <- names(alias_map)

    result
  } else {
    list()
  }

  # Convert mappings
  mappings <- lapply(x$mappings, function(m) {
    # Use description field (from reconciliation) or desc field (legacy)
    mapping_desc <- if (!is.null(m$description)) m$description else m$desc
    new_mapping(
      name = m$name,
      desc = mapping_desc,
      dims = m$dims,
      data = if (!is.null(m$data)) m$data else NULL, # optional
      symbols = symbols
    )
  })

  # Convert parameters
  parameters <- lapply(x$parameters, function(p) {
    param <- new_parameter(
      name = p$name,
      desc = p$desc,
      dims = p$dims,
      data = if (!is.null(p$data)) p$data else NULL, # optional
      defVal = p$defVal,
      symbolic = if (!is.null(p$symbolic)) p$symbolic else FALSE,
      formula = p$formula,  # Preserve computed parameter formulas
      symbols = symbols
    )
    # Preserve dims_index_aliases for computed parameters
    if (!is.null(p$dims_index_aliases)) {
      param$dims_index_aliases <- p$dims_index_aliases
    }
    param
  })

  # Convert variables
  variables <- lapply(x$variables, function(v) {
    var <- new_variable(
      name = v$name,
      desc = v$desc,
      dims = v$dims,
      domain = v$domain,    # Preserve sparse domain mapping from read_gams
      vtype = v$vtype,      # Preserve variable type (positive, integer, binary, etc.)
      bounds = v$bounds,    # Preserve bounds (lo, up)
      comment = v$comment,   # Preserve *@ comment
      symbols = symbols
    )
    # Preserve dims_index_aliases for variables
    if (!is.null(v$dims_index_aliases)) {
      var$dims_index_aliases <- v$dims_index_aliases
    }
    var
  })

  # Convert equations with error capture
  equations <- list()
  objectives <- list()  # Store objective metadata: list(eq_name = list(equation, variable, sense, ...))

  # Process objectives from model_structure
  if (!is.null(x$objectives) && length(x$objectives) > 0) {
    for (i in seq_along(x$objectives)) {
      obj_info <- x$objectives[[i]]
      
      # For linopy models, objectives don't have equation matches - they're standalone
      # Pyomo (AbstractModel-first import) also provides objective *signatures* without equations.
      # Just pass through the objective info as-is.
      if (!is.null(x$language) && grepl("linopy|pyomo", x$language, ignore.case = TRUE)) {
        objectives[[length(objectives) + 1]] <- obj_info
        next
      }
      
      # For GAMS/GMPL models: find matching equations
      obj_var_name <- obj_info$variable
      obj_sense <- obj_info$sense

      # Find ALL equations that reference this objective variable
      matching_equations <- character(0)

      for (eq_name in names(x$equations)) {
        eq_body <- x$equations[[eq_name]]$gams
        if (is.null(eq_body)) eq_body <- x$equations[[eq_name]]$gmpl
        if (is.null(eq_body)) eq_body <- x$equations[[eq_name]]$body

        # Check if objective variable appears in the equation
        if (!is.null(eq_body) && grepl(paste0("\\b", obj_var_name, "\\b"), eq_body)) {
          matching_equations <- c(matching_equations, eq_name)
        }
      }

      # Report if multiple or no matches
      if (length(matching_equations) == 0) {
        warning(sprintf(
          "Objective variable '%s' not found in any equation. Cannot identify objective equation.",
          obj_var_name
        ))
      } else if (length(matching_equations) > 1) {
        warning(sprintf(
          "Objective variable '%s' appears in %d equations: %s. All will be marked as objectives.",
          obj_var_name,
          length(matching_equations),
          paste(matching_equations, collapse = ", ")
        ))
      }

      # Create multimod objective entries for all matching equations
      # Append to unnamed list (allows duplicates)
      for (eq_name in matching_equations) {
        obj_entry <- list(
          equation = eq_name,
          variable = obj_var_name,
          sense = obj_sense
        )
        
        # Copy all additional metadata from model_structure (solver, model, gams/gmpl source)
        for (field in names(obj_info)) {
          if (!field %in% c("variable", "sense")) {
            obj_entry[[field]] <- obj_info[[field]]
          }
        }
        
        objectives[[length(objectives) + 1]] <- obj_entry
      }
    }
  }

  for (eq_name in names(x$equations)) {
    eqn_info <- x$equations[[eq_name]]

    # Pyomo (AbstractModel-first): constraints may be exported as signatures only
    # (no algebra yet). Keep them as lightweight placeholder equations so users
    # can explore model structure without requiring expression parsing.
    if (!is.null(x$language) && grepl("pyomo", x$language, ignore.case = TRUE)) {
      eq_body <- eqn_info$body %||% eqn_info$pyomo %||% NULL
      if (is.null(eq_body) && is.null(eqn_info$lhs) && is.null(eqn_info$rhs)) {
        relation <- eqn_info$relation %||% "=="
        if (!relation %in% c("==", "<=", ">=")) relation <- "=="

        placeholder_desc <- eqn_info$desc %||% NULL
        if (is.null(placeholder_desc) || !nzchar(as.character(placeholder_desc))) {
          placeholder_desc <- "Pyomo constraint signature only (algebra not imported)"
        } else {
          placeholder_desc <- paste0(as.character(placeholder_desc), " [signature only]")
        }

        eq_obj <- new_equation(
          name = eq_name,
          desc = placeholder_desc,
          dims = eqn_info$dims %||% character(0),
          lhs = ast_constant(0),
          rhs = ast_constant(0),
          relation = relation,
          dims_index_aliases = eqn_info$dims_index_aliases %||% NULL,
          symbols = symbols
        )
        class(eq_obj) <- c("equation_signature", class(eq_obj))
        equations[[eq_name]] <- eq_obj
        next
      }
    }
    
    # Skip placeholder equations that can't be parsed:
    # 1. Fallback constraints: "[fallback] name_lhs >= name_rhs"
    if (!is.null(eqn_info$body)) {
      if (grepl("^\\[fallback\\]", eqn_info$body)) {
        next
      }
      
      # 2. Special handling for objective placeholders:
      #    "objective = [linopy objective with N terms]"
      #    Create a minimal equation for the objective even if we can't parse the full expression
      if (eq_name == "objective" && grepl("linopy objective with.*terms", eqn_info$body)) {
        # Create minimal objective equation: objective (no lhs/rhs AST, just metadata)
        eqn <- structure(
          list(
            name = "objective",
            desc = eqn_info$desc %||% "Objective function",
            dims = character(0),
            dims_index_aliases = eqn_info$dims_index_aliases %||% character(0),
            relation = "==",
            # No lhs/rhs - this is just a placeholder equation
            sense = eqn_info$sense %||% "minimize",
            body = eqn_info$body
          ),
          class = c("objective_equation", "equation", "multimod")
        )
        equations[[eq_name]] <- eqn
        next
      }
    }

    eqn <- tryCatch(
      {
        # Use appropriate parser based on source language
        if (!is.null(x$language) &&
            grepl("linopy", x$language, ignore.case = TRUE)) {
          stop("linopy equation parsing is not part of the package; ",
               "the experimental reader lives in drafts/R/read_linopy.R")
        } else if (!is.null(x$language) &&
            grepl("gmpl|glpk|mathprog", x$language, ignore.case = TRUE)) {
          parse_gmpl_equation_to_ast(eqn_info, symbols)
        } else if (!is.null(x$language) &&
                  grepl("gams", x$language, ignore.case = TRUE)) {

          parse_gams_equation(eqn_info, symbols)

        } else {
          stop(paste("Unrecognized modeling lagnguage", x$language))
        }
      },
      error = function(e) {
        stop(sprintf("Failed to convert equation '%s': %s", eq_name, e$message))
        # NULL
      }
    )
    if (!is.null(eqn)) {
      equations[[eq_name]] <- eqn
    }
  }

  metadata <- args$metadata
  if (!is.null(metadata)) {
    stopifnot(is.list(metadata))
    args$metadata <- NULL
  } else {
    metadata <- list()
  }
  
  # Copy symbol_name_map and latex_names from model_structure if available
  if (!is.null(x$metadata$symbol_name_map)) {
    metadata$symbol_name_map <- x$metadata$symbol_name_map
  }
  if (!is.null(x$metadata$latex_names)) {
    metadata$latex_names <- x$metadata$latex_names
  }

  data_source <- NULL
  if (!is.null(args$data_source)) {
    data_source <- args$data_source
    args$data_source <- NULL
  } else if (!is.null(x$data_source)) {
    data_source <- x$data_source
  }

  language <- x$language
  if (is.null(language)) {
    language <- attr(x, "language", exact = TRUE)
  }

  metadata_defaults <- list(
    language = language,
    source_file = x$source,
    data_source = data_source
  )
  metadata_defaults <- metadata_defaults[!vapply(metadata_defaults, is.null, logical(1))]

  if (length(metadata_defaults) > 0) {
    for (nm in names(metadata_defaults)) {
      if (is.null(metadata[[nm]])) {
        metadata[[nm]] <- metadata_defaults[[nm]]
      }
    }
  }

  args <- c(list(
    name = name,
    desc = desc,
    sets = sets,
    aliases = aliases,
    mappings = mappings,
    parameters = parameters,
    variables = variables,
    equations = equations,
    objectives = if (length(objectives) > 0) objectives else NULL,  # Add objectives list
    metadata = metadata
  ), args)

  model <- do.call(new_model, args)

  # Handle index_aliases by merging user input, existing metadata, and iterators
  # discovered in component-specific dims_index_aliases. Fall back to auto-
  # generation only for sets still missing coverage.
  collect_component_aliases <- function(alias_map, components) {
    if (is.null(components) || length(components) == 0) {
      return(alias_map)
    }
    for (component in components) {
      comp_aliases <- component$dims_index_aliases
      if (is.null(comp_aliases) || length(comp_aliases) == 0) next
      for (i in seq_along(comp_aliases)) {
        set_name <- names(comp_aliases)[i]
        alias_value <- comp_aliases[[i]]
        alias_map <- register_index_alias(alias_map, set_name, alias_value)
      }
    }
    alias_map
  }

  alias_map <- character(0)
  if (!is.null(index_aliases)) {
    alias_map <- index_aliases
  } else if (!is.null(x$index_aliases)) {
    alias_map <- x$index_aliases
  }

  alias_map <- collect_component_aliases(alias_map, model$parameters)
  alias_map <- collect_component_aliases(alias_map, model$variables)
  alias_map <- collect_component_aliases(alias_map, model$equations)

  all_set_names <- character(0)
  if (!is.null(model$sets)) {
    all_set_names <- c(all_set_names, names(model$sets))
  }
  if (!is.null(model$aliases)) {
    for (alias_group in model$aliases) {
      all_set_names <- c(all_set_names, alias_group)
    }
  }
  all_set_names <- unique(all_set_names)

  if (length(all_set_names) > 0) {
    missing_sets <- setdiff(all_set_names, names(alias_map))
    if (length(missing_sets) > 0) {
      used_iterators <- unique(unname(alias_map))
      symbol_names <- list(
        sets = names(model$sets),
        parameters = names(model$parameters),
        variables = names(model$variables)
      )
      for (set_name in missing_sets) {
        new_alias <- generate_index_alias(
          set_name,
          used_names = used_iterators,
          all_set_names = all_set_names,
          all_symbols = symbol_names
        )
        alias_map <- register_index_alias(alias_map, set_name, new_alias)
        used_iterators <- c(used_iterators, new_alias)
      }
    }
  }

  if (length(alias_map) > 0) {
    model$index_aliases <- alias_map
  } else {
    model$index_aliases <- character(0)
  }

  model
  #   list(
  #     name = name,
  #     desc = desc,
  #     sets = sets,
  #     aliases = aliases,
  #     mappings = mappings,
  #     parameters = parameters,
  #     variables = variables,
  #     equations = equations,
  #     args
  #   )
  # )


  # new_model(
  #   name = name,
  #   desc = desc,
  #   sets = sets,
  #   aliases = aliases,
  #   mappings = mappings,
  #   parameters = parameters,
  #   variables = variables,
  #   equations = equations,
  #   args
  # )
}

# coerce_param <- function(name, param_info) {
#   new_parameter(
#     name = name,
#     dims = param_info$dims,
#     data = param_info$data
#   )
# }
#
# coerce_variable <- function(name, var_info) {
#   new_variable(
#     name = name,
#     dims = var_info$dims,
#     data = var_info$data,
#     domain = var_info$domain %||% "continuous"
#   )
# }



