# Symbol analysis utilities -------------------------------------------------

#' Collect declared symbols from a model object
#'
#' @param model A `multimod` model
#' @param include_aliases Logical; include set alias information
#' @param include_index_aliases Logical; include index alias information
#' @return A list containing the symbol table, lookup map, and collision info
#' @keywords internal
collect_model_symbols <- function(model,
                                  include_aliases = TRUE,
                                  include_index_aliases = TRUE) {
  stopifnot(inherits(model, "model"))

  component_names <- function(items) {
    if (is.null(items) || length(items) == 0) {
      return(character(0))
    }
    nm <- names(items)
    if (is.null(nm) || any(nm == "")) {
      nm <- vapply(items, function(item) item$name %||% NA_character_, character(1))
    }
    nm
  }

  build_records <- function(names, type, alias_for = NA_character_, source = "model") {
    if (length(names) == 0) {
      return(data.frame(
        name = character(0),
        lower = character(0),
        type = character(0),
        alias_for = character(0),
        source = character(0),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      name = names,
      lower = tolower(names),
      type = type,
      alias_for = alias_for,
      source = source,
      stringsAsFactors = FALSE
    )
  }

  sets <- build_records(component_names(model$sets), "set")
  mappings <- build_records(component_names(model$mappings), "mapping")
  parameters <- build_records(component_names(model$parameters), "parameter")
  variables <- build_records(component_names(model$variables), "variable")

  alias_records <- data.frame()
  if (include_aliases && !is.null(model$aliases) && length(model$aliases) > 0) {
    alias_list <- lapply(model$aliases, function(group) {
      if (length(group) <= 1) return(NULL)
      canonical <- group[[1]]
      alias_names <- setdiff(unique(group), canonical)
      build_records(alias_names, "set_alias", alias_for = canonical, source = "alias")
    })
    alias_records <- do.call(rbind, Filter(Negate(is.null), alias_list))
  }

  index_alias_records <- data.frame()
  if (include_index_aliases && !is.null(model$index_aliases) && length(model$index_aliases) > 0) {
    # Use unlist to convert list to character vector
    index_alias_records <- build_records(
      unlist(unname(model$index_aliases)),
      "index_alias",
      alias_for = names(model$index_aliases),
      source = "index_alias"
    )
  }

  table <- do.call(rbind, Filter(Negate(is.null), list(
    sets, mappings, parameters, variables, alias_records, index_alias_records
  )))
  if (is.null(table)) {
    table <- build_records(character(0), character(0))
  } else {
    table <- unique(table)
  }
  
  # Add symbol name map aliases (underscore variants for hyphenated names)
  name_map_records <- data.frame()
  if (!is.null(model$metadata$symbol_name_map)) {
    name_map <- model$metadata$symbol_name_map
    
    # For each actual name (with hyphens), create an alias record with underscores
    if (!is.null(name_map$reverse)) {
      for (actual_name in names(name_map$reverse)) {
        gmpl_name <- name_map$reverse[[actual_name]]
        
        # Only add if different from actual name
        if (actual_name != gmpl_name) {
          # Find the type of the actual symbol
          actual_row <- table[table$name == actual_name & table$source == "model", ]
          
          if (nrow(actual_row) > 0) {
            # Create alias record
            type_suffix <- switch(actual_row$type[1],
                                  variable = "variable_alias",
                                  parameter = "parameter_alias",
                                  set = "set_alias",
                                  mapping = "mapping_alias",
                                  paste0(actual_row$type[1], "_alias"))
            
            name_map_records <- rbind(name_map_records,
                                      build_records(gmpl_name, type_suffix, 
                                                   alias_for = actual_name, 
                                                   source = "name_map"))
          }
        }
      }
    }
  }
  
  # Combine with name map aliases
  if (nrow(name_map_records) > 0) {
    table <- rbind(table, name_map_records)
    table <- unique(table)
  }

  lookup <- split(table, table$lower)

  primary_types <- c("set", "mapping", "parameter", "variable")
  primary <- table[table$type %in% primary_types, , drop = FALSE]
  dup_mask <- duplicated(primary$lower) | duplicated(primary$lower, fromLast = TRUE)
  collisions <- if (any(dup_mask)) {
    split(primary[dup_mask, , drop = FALSE], primary$lower[dup_mask])
  } else {
    list()
  }

  result <- list(
    table = table,
    lookup = lookup,
    collisions = collisions
  )
  
  # Attach symbol_name_map if available in model metadata
  if (!is.null(model$metadata$symbol_name_map)) {
    attr(result, "symbol_name_map") <- model$metadata$symbol_name_map
  }
  
  result
}

new_symbol_issue_log <- function() {
  empty_undef <- data.frame(
    component = character(0),
    name = character(0),
    field = character(0),
    symbol = character(0),
    role = character(0),
    stringsAsFactors = FALSE
  )
  empty_case <- data.frame(
    component = character(0),
    name = character(0),
    field = character(0),
    symbol = character(0),
    expected = character(0),
    role = character(0),
    stringsAsFactors = FALSE
  )
  empty_fixes <- data.frame(
    component = character(0),
    name = character(0),
    field = character(0),
    from = character(0),
    to = character(0),
    type = character(0),
    stringsAsFactors = FALSE
  )
  list(undefined = empty_undef, case = empty_case, fixes = empty_fixes)
}

append_symbol_issue <- function(log, type, entry) {
  if (is.null(entry) || length(entry) == 0) {
    return(log)
  }
  stopifnot(type %in% names(log))
  df <- as.data.frame(entry, stringsAsFactors = FALSE)
  log[[type]] <- rbind(log[[type]], df)
  log
}

symbol_scope <- function(bound = character()) {
  bound <- bound[!is.na(bound)]
  bound <- bound[nchar(bound) > 0]
  list(bound = unique(bound))
}

scope_contains <- function(scope, name) {
  if (is.null(scope$bound) || length(scope$bound) == 0) {
    return(FALSE)
  }
  lname <- tolower(name)
  any(tolower(scope$bound) == lname)
}

symbol_role_priority <- function(role = "expr") {
  switch(role,
    set_ref = c("mapping", "set", "set_alias"),
    expr = c("variable", "variable_alias", "parameter", "parameter_alias", 
             "mapping", "mapping_alias", "set", "set_alias", "index_alias"),
    domain = c("mapping", "mapping_alias", "parameter", "parameter_alias", 
               "variable", "variable_alias", "set", "set_alias"),
    default = c("variable", "variable_alias", "parameter", "parameter_alias", 
                "mapping", "mapping_alias", "set", "set_alias", "index_alias")
  )
}

choose_symbol_entry <- function(entries, role = "expr", symbol_info = NULL) {
  if (is.null(entries) || nrow(entries) == 0) {
    return(NULL)
  }
  priority <- symbol_role_priority(role)
  entries$priority <- match(entries$type, priority, nomatch = NA_integer_)
  entries <- entries[order(entries$priority), , drop = FALSE]
  for (i in seq_len(nrow(entries))) {
    entry <- entries[i, , drop = FALSE]
    if (is.na(entry$priority)) next
    if (!is.na(entry$alias_for) && nzchar(entry$alias_for)) {
      if (!is.null(symbol_info)) {
        canonical <- symbol_info$lookup[[tolower(entry$alias_for)]]
        if (!is.null(canonical)) {
          resolved <- choose_symbol_entry(canonical, role, symbol_info)
          if (!is.null(resolved)) {
            return(resolved)
          }
        }
      }
      next
    }
    entry$priority <- NULL
    return(entry)
  }
  NULL
}

normalize_symbol_dims <- function(dims_value) {
  if (is.null(dims_value)) {
    return(ast_dims())
  }
  if (inherits(dims_value, "dims")) {
    return(dims_value)
  }
  if (inherits(dims_value, "ast")) {
    return(ast_dims(dims_value))
  }
  if (is.list(dims_value) && length(dims_value) > 0) {
    return(do.call(ast_dims, dims_value))
  }
  ast_dims()
}

coerce_symbol_node <- function(node, entry) {
  entry <- as.list(entry)
  # Symbols only have dims field (not index)
  dims <- normalize_symbol_dims(node$dims)
  # Extract extras but exclude fields that are handled explicitly
  extras <- node[setdiff(names(node), c("name", "dims"))]
  ctor <- switch(entry$type,
    variable = ast_variable,
    parameter = ast_parameter,
    mapping = ast_mapping,
    set = function(name, ...) ast_set(name),
    NULL
  )
  if (is.null(ctor)) {
    return(node)
  }
  args <- c(list(name = entry$name), if (entry$type == "set") list() else list(dims = dims), extras)
  do.call(ctor, args)
}

resolve_ast_symbols <- function(node,
                                symbol_info,
                                scope = symbol_scope(),
                                role = "expr",
                                context = list(component = NA_character_,
                                               name = NA_character_,
                                               field = NA_character_),
                                fix = FALSE,
                                log = new_symbol_issue_log()) {
  if (is.null(node)) {
    return(list(node = NULL, log = log))
  }

  cls <- node_type(node)
  if (is.null(cls)) {
    if (is.list(node) && !is.data.frame(node)) {
      for (i in seq_along(node)) {
        value <- node[[i]]
        if (inherits(value, "ast")) {
          res <- resolve_ast_symbols(value, symbol_info, scope, role, context, fix, log)
          node[[i]] <- res$node
          log <- res$log
        }
      }
    }
    return(list(node = node, log = log))
  }

  if (cls == "symbol") {
    name <- node$name
    if (scope_contains(scope, name)) {
      return(list(node = node, log = log))
    }
    
    # Try direct lookup first
    match <- symbol_info$lookup[[tolower(name)]]
    
    # If not found and we have symbol_name_map in table metadata,
    # try resolving the name (GMPL underscore -> actual hyphen)
    if (is.null(match) && !is.null(attr(symbol_info, "symbol_name_map"))) {
      name_map <- attr(symbol_info, "symbol_name_map")
      if (name %in% names(name_map$forward)) {
        resolved_name <- name_map$forward[[name]]
        match <- symbol_info$lookup[[tolower(resolved_name)]]
      }
    }
    
    if (is.null(match)) {
      entry <- data.frame(
        component = context$component,
        name = context$name,
        field = context$field,
        symbol = name,
        role = role,
        stringsAsFactors = FALSE
      )
      log <- append_symbol_issue(log, "undefined", entry)
      return(list(node = node, log = log))
    }
    entry <- choose_symbol_entry(match, role, symbol_info)
    if (is.null(entry)) {
      entry <- data.frame(
        component = context$component,
        name = context$name,
        field = context$field,
        symbol = name,
        role = role,
        stringsAsFactors = FALSE
      )
      log <- append_symbol_issue(log, "undefined", entry)
      return(list(node = node, log = log))
    }
    entry <- as.list(entry)
    # Check if the original match was an alias
    is_alias <- any(match$type %in% c("set_alias", "index_alias"), na.rm = TRUE)
    if (!identical(entry$name, name) && !is_alias) {
      warn_row <- data.frame(
        component = context$component,
        name = context$name,
        field = context$field,
        symbol = name,
        expected = entry$name,
        role = role,
        stringsAsFactors = FALSE
      )
      log <- append_symbol_issue(log, "case", warn_row)
    }
    if (fix && entry$type %in% c("variable", "parameter", "mapping", "set")) {
      new_node <- coerce_symbol_node(node, entry)
      fix_row <- data.frame(
        component = context$component,
        name = context$name,
        field = context$field,
        from = name,
        to = entry$name,
        type = entry$type,
        stringsAsFactors = FALSE
      )
      log <- append_symbol_issue(log, "fixes", fix_row)
      return(list(node = new_node, log = log))
    }
    return(list(node = node, log = log))
  }

  aggregator_classes <- c("sum", "prod", "setmin", "setmax")
  if (cls %in% aggregator_classes) {
    idx_context <- context
    idx_context$field <- paste0(context$field %||% "expr", "::index")
    res_idx <- resolve_ast_symbols(node$index, symbol_info, scope, role = "set_ref", idx_context, fix, log)
    node$index <- res_idx$node
    log <- res_idx$log
    alias_names <- names(node$index)
    alias_names <- alias_names[!is.na(alias_names) & alias_names != ""]
    child_scope <- symbol_scope(c(scope$bound, alias_names))
    val_context <- context
    val_context$field <- paste0(context$field %||% "expr", "::value")
    res_val <- resolve_ast_symbols(node$value, symbol_info, child_scope, role = "expr", val_context, fix, log)
    node$value <- res_val$node
    log <- res_val$log
    return(list(node = node, log = log))
  }

  if (cls == "expression") {
    res_lhs <- resolve_ast_symbols(node$lhs, symbol_info, scope, "expr", context, fix, log)
    node$lhs <- res_lhs$node
    log <- res_lhs$log
    res_rhs <- resolve_ast_symbols(node$rhs, symbol_info, scope, "expr", context, fix, log)
    node$rhs <- res_rhs$node
    log <- res_rhs$log
    return(list(node = node, log = log))
  }

  if (cls == "unary") {
    res_rhs <- resolve_ast_symbols(node$rhs, symbol_info, scope, "expr", context, fix, log)
    node$rhs <- res_rhs$node
    log <- res_rhs$log
    return(list(node = node, log = log))
  }

  if (cls == "call") {
    if (length(node$args) > 0) {
      for (i in seq_along(node$args)) {
        res_arg <- resolve_ast_symbols(node$args[[i]], symbol_info, scope, "expr", context, fix, log)
        node$args[[i]] <- res_arg$node
        log <- res_arg$log
      }
    }
    return(list(node = node, log = log))
  }

  if (cls == "func") {
    if (!is.null(node$index)) {
      res_index <- resolve_ast_symbols(node$index, symbol_info, scope, "set_ref", context, fix, log)
      node$index <- res_index$node
      log <- res_index$log
    }
    res_value <- resolve_ast_symbols(node$value, symbol_info, scope, "expr", context, fix, log)
    node$value <- res_value$node
    log <- res_value$log
    return(list(node = node, log = log))
  }

  if (cls == "when") {
    res_cond <- resolve_ast_symbols(node$condition, symbol_info, scope, "expr", context, fix, log)
    node$condition <- res_cond$node
    log <- res_cond$log
    if (!is.null(node$then)) {
      res_then <- resolve_ast_symbols(node$then, symbol_info, scope, "expr", context, fix, log)
      node$then <- res_then$node
      log <- res_then$log
    }
    if (!is.null(node$otherwise)) {
      res_else <- resolve_ast_symbols(node$otherwise, symbol_info, scope, "expr", context, fix, log)
      node$otherwise <- res_else$node
      log <- res_else$log
    }
    return(list(node = node, log = log))
  }

  if (cls == "where") {
    if (!is.null(node$content)) {
      res_content <- resolve_ast_symbols(node$content, symbol_info, scope, role, context, fix, log)
      node$content <- res_content$node
      log <- res_content$log
    }
    return(list(node = node, log = log))
  }

  if (cls == "dims") {
    if (length(node) > 0) {
      for (i in seq_along(node)) {
        value <- node[[i]]
        if (inherits(value, "ast")) {
          res_dim <- resolve_ast_symbols(value, symbol_info, scope, "set_ref", context, fix, log)
          node[[i]] <- res_dim$node
          log <- res_dim$log
        }
      }
    }
    return(list(node = node, log = log))
  }

  if (cls == "shift") {
    if (!scope_contains(scope, node$symbol)) {
      match <- symbol_info$lookup[[tolower(node$symbol)]]
      if (is.null(match)) {
        entry <- data.frame(
          component = context$component,
          name = context$name,
          field = context$field,
          symbol = node$symbol,
          role = "index",
          stringsAsFactors = FALSE
        )
        log <- append_symbol_issue(log, "undefined", entry)
      }
    }
    return(list(node = node, log = log))
  }

  # Default: walk all AST children
  for (slot_name in names(node)) {
    value <- node[[slot_name]]
    if (inherits(value, "ast")) {
      child_role <- if (slot_name %in% c("dims", "active_dims", "index")) "set_ref" else role
      res_child <- resolve_ast_symbols(value, symbol_info, scope, child_role, context, fix, log)
      node[[slot_name]] <- res_child$node
      log <- res_child$log
    } else if (is.list(value) && !is.data.frame(value)) {
      for (i in seq_along(value)) {
        if (inherits(value[[i]], "ast")) {
          res_child <- resolve_ast_symbols(value[[i]], symbol_info, scope, role, context, fix, log)
          value[[i]] <- res_child$node
          log <- res_child$log
        }
      }
      node[[slot_name]] <- value
    }
  }

  list(node = node, log = log)
}

inspect_model_symbols <- function(model,
                                  symbol_info = NULL,
                                  fix = FALSE,
                                  verbose = FALSE) {
  stopifnot(inherits(model, "model"))
  symbol_info <- symbol_info %||% collect_model_symbols(model)
  log <- new_symbol_issue_log()

  # Equations
  if (!is.null(model$equations) && length(model$equations) > 0) {
    for (eq_name in names(model$equations)) {
      eq <- model$equations[[eq_name]]
      scope <- symbol_scope(unique(unname(eq$dims_index_aliases)))
      context <- list(component = "equation", name = eq_name, field = "lhs")
      res_lhs <- resolve_ast_symbols(eq$lhs, symbol_info, scope, "expr", context, fix, log)
      eq$lhs <- res_lhs$node
      log <- res_lhs$log

      context$field <- "rhs"
      res_rhs <- resolve_ast_symbols(eq$rhs, symbol_info, scope, "expr", context, fix, log)
      eq$rhs <- res_rhs$node
      log <- res_rhs$log

      if (!is.null(eq$domain)) {
        context$field <- "domain"
        res_domain <- resolve_ast_symbols(eq$domain, symbol_info, scope, "expr", context, fix, log)
        eq$domain <- res_domain$node
        log <- res_domain$log
      }

      model$equations[[eq_name]] <- eq
    }
  }

  # Parameter formulas and defaults
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    for (param_name in names(model$parameters)) {
      param <- model$parameters[[param_name]]
      scope <- symbol_scope(unique(unname(param$dims_index_aliases)))
      if (inherits(param$defVal, "ast_formula")) {
        context <- list(component = "parameter", name = param_name, field = "defVal")
        res_def <- resolve_ast_symbols(param$defVal$expr, symbol_info, scope, "expr", context, fix, log)
        param$defVal$expr <- res_def$node
        log <- res_def$log
      }
      if (inherits(param$formula, "ast_formula")) {
        context <- list(component = "parameter", name = param_name, field = "formula")
        res_for <- resolve_ast_symbols(param$formula$expr, symbol_info, scope, "expr", context, fix, log)
        param$formula$expr <- res_for$node
        log <- res_for$log
      }
      model$parameters[[param_name]] <- param
    }
  }

  list(model = model, issues = log, symbol_info = symbol_info)
}

summarize_symbol_issues <- function(symbol_info, issues) {
  errors <- character(0)
  warnings <- character(0)

  if (length(symbol_info$collisions) > 0) {
    for (lower_name in names(symbol_info$collisions)) {
      defs <- symbol_info$collisions[[lower_name]]
      detail <- paste(sprintf("%s '%s'", defs$type, defs$name), collapse = ", ")
      errors <- c(errors, sprintf("Symbol name '%s' is ambiguous (%s)", lower_name, detail))
    }
  }

  # Undefined symbols are now warnings (not errors) to allow symbolic parameters
  if (nrow(issues$undefined) > 0) {
    for (i in seq_len(nrow(issues$undefined))) {
      row <- issues$undefined[i, ]
      target <- sprintf("%s '%s'", row$component %||% "component", row$name %||% "<unnamed>")
      field <- row$field %||% "expression"
      warnings <- c(warnings, sprintf("%s uses undefined symbol '%s' in %s", target, row$symbol, field))
    }
  }

  if (nrow(issues$case) > 0) {
    for (i in seq_len(nrow(issues$case))) {
      row <- issues$case[i, ]
      target <- sprintf("%s '%s'", row$component %||% "component", row$name %||% "<unnamed>")
      field <- row$field %||% "expression"
      warnings <- c(warnings, sprintf("%s references '%s' but declared as '%s' in %s", target, row$symbol, row$expected, field))
    }
  }

  list(errors = errors, warnings = warnings)
}

#' Make a multimod model valid by reconciling symbols
#'
#' Converts unresolved `symbol` nodes inside equations and parameter formulas
#' into typed AST nodes when possible, correcting casing and ensuring references
#' match declared parameters, variables, mappings, or sets.
#'
#' @param x A `multimod` model object
#' @param verbose Logical; emit a short summary of applied fixes
#' @param stop_on_error Logical; passed to [validate()] when `revalidate = TRUE`
#' @param revalidate Logical; run [validate()] after applying fixes
#' @param ... Reserved for future use
#'
#' @return A model object with updated AST nodes
#' @export
make_valid <- function(x, ...) {
  UseMethod("make_valid")
}

#' @export
make_valid.model <- function(x,
                             verbose = FALSE,
                             stop_on_error = FALSE,
                             revalidate = FALSE,
                             ...) {
  scan <- inspect_model_symbols(x, fix = TRUE)
  model <- scan$model
  attr(model, "symbol_fixes") <- scan$issues
  if (verbose && nrow(scan$issues$fixes) > 0) {
    message(sprintf("make_valid applied %d symbol fixes", nrow(scan$issues$fixes)))
  }
  if (revalidate) {
    validate(model, stop_on_error = stop_on_error, ...)
  }
  model
}
