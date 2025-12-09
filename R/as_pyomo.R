#' Convert a multimod AST node to Pyomo syntax
#'
#' This helper mirrors `as_jump()` but emits Python/Pyomo-friendly strings.
#' The implementation focuses on linear expressions (sum/prod, arithmetic
#' operators, variable/parameter references) which cover the current OSeMOSYS
#' workflow. Unsupported node types raise informative errors so we can extend
#' coverage incrementally.
#'
#' @param x An AST node (e.g. equation, expression, variable, parameter).
#' @param ... Additional arguments used internally:
#'   * `model`: multimod model for alias lookups.
#'   * `var_names`: named vector mapping base set names to iterator aliases
#'     (used when emitting constraint bodies).
#'   * `scope`: local iterator scope for nested aggregations.
#'
#' @return Character string containing Pyomo-compatible code.
#' @export
as_pyomo <- function(x, ...) {
  UseMethod("as_pyomo", x)
}

#' @export
as_pyomo.default <- function(x, ...) {
  if (is.null(x)) {
    return("None")
  }
  if (is.numeric(x) && length(x) == 1) {
    return(pyomo_format_scalar(x))
  }
  if (is.character(x) && length(x) == 1) {
    return(pyomo_quote(x))
  }
  if (is.logical(x) && length(x) == 1) {
    return(if (isTRUE(x)) "True" else "False")
  }
  stop("No as_pyomo method for object of class: ", paste(class(x), collapse = "/"))
}

# Internal utilities ---------------------------------------------------------

pyomo_quote <- function(x) {
  quoted <- gsub("\\\\", "\\\\\\\\", x, fixed = TRUE)
  quoted <- gsub('"', '\\"', quoted, fixed = TRUE)
  sprintf('"%s"', quoted)
}

pyomo_format_scalar <- function(x) {
  if (inherits(x, "Date")) {
    return(pyomo_quote(as.character(x)))
  }
  if (is.numeric(x) && length(x) == 1 && is.infinite(x)) {
    return(if (x > 0) "float('inf')" else "-float('inf')")
  }
  fmt <- format(x, scientific = FALSE, trim = TRUE)
  if (!grepl("[.eE]", fmt, perl = TRUE)) {
    return(fmt)
  }
  fmt
}

pyomo_dim_name <- function(dim) {
  if (inherits(dim, "symbol")) return(dim$name)
  if (inherits(dim, "set")) return(dim$name)
  if (inherits(dim, "character")) return(dim[[1]])
  if (is.list(dim) && !is.null(dim$name)) return(dim$name)
  as.character(dim)[1]
}

pyomo_collect_dim_names <- function(dims_obj) {
  if (is.null(dims_obj) || length(dims_obj) == 0) return(character(0))
  dims_obj <- normalize_symbol_dims(dims_obj)
  dim_names <- character(length(dims_obj))
  for (i in seq_along(dims_obj)) {
    dim_names[[i]] <- pyomo_dim_name(dims_obj[[i]])
  }
  dim_names
}

pyomo_safe_name <- function(name) {
  gsub("[^A-Za-z0-9_]", "_", name)
}

pyomo_resolve_symbol <- function(name, var_names = NULL, scope = NULL, model = NULL) {
  if (length(name) != 1) {
    name <- as.character(name)[1]
  }
  if (!is.null(scope) && name %in% names(scope)) {
    return(scope[[name]])
  }
  if (!is.null(var_names) && name %in% names(var_names)) {
    return(var_names[[name]])
  }
  if (!is.null(model) && !is.null(model$index_aliases) && name %in% names(model$index_aliases)) {
    return(model$index_aliases[[name]])
  }
  name
}

pyomo_resolve_base_set <- function(name, model = NULL) {
  if (is.null(model) || is.null(model$aliases)) return(name)
  for (alias_group in model$aliases) {
    if (name %in% alias_group) {
      return(alias_group[1])
    }
  }
  name
}

pyomo_index_brackets <- function(dims, model = NULL, var_names = NULL, scope = NULL) {
  dims <- normalize_symbol_dims(dims)
  if (length(dims) == 0) return("")
  symbols <- character(length(dims))
  for (i in seq_along(dims)) {
    dim <- dims[[i]]
    # If it's an expression or shift (like ls+1 or y-1), render it as Pyomo code
    if (inherits(dim, c("expression", "shift"))) {
      symbols[[i]] <- as_pyomo(dim, model = model, var_names = var_names, scope = scope)
    } else {
      sym_name <- pyomo_dim_name(dim)
      symbols[[i]] <- pyomo_resolve_symbol(sym_name, var_names = var_names, scope = scope, model = model)
    }
  }
  sprintf("[%s]", paste(symbols, collapse = ", "))
}

pyomo_build_iterator_name <- function(base, used) {
  name <- base
  counter <- 1
  while (name %in% used) {
    counter <- counter + 1
    name <- paste0(base, counter)
  }
  name
}

pyomo_build_iterators <- function(dim_names, model = NULL, existing = character(0)) {
  iterators <- vector("list", length(dim_names))
  used <- existing
  for (i in seq_along(dim_names)) {
    dim_name <- dim_names[i]
    base <- pyomo_resolve_base_set(dim_name, model)
    alias <- pyomo_resolve_symbol(dim_name, model = model)
    iter_name <- pyomo_build_iterator_name(alias, used)
    used <- c(used, iter_name)
    iterators[[i]] <- list(symbol = dim_name, iterator = iter_name, set = base)
  }
  iterators
}

pyomo_tuple_from_dims <- function(dims_obj, model = NULL, var_names = NULL, scope = NULL) {
  dims <- normalize_symbol_dims(dims_obj)
  pieces <- character(length(dims))
  for (i in seq_along(dims)) {
    dim <- dims[[i]]
    # If it's an expression or shift (like ls+1 or y-1), render it as Pyomo code
    if (inherits(dim, c("expression", "shift"))) {
      pieces[[i]] <- as_pyomo(dim, model = model, var_names = var_names, scope = scope)
    } else {
      sym_name <- pyomo_dim_name(dim)
      pieces[[i]] <- pyomo_resolve_symbol(sym_name, var_names = var_names, scope = scope, model = model)
    }
  }
  if (length(pieces) == 1) {
    return(pieces)
  }
  paste0("(", paste(pieces, collapse = ", "), ")")
}

pyomo_membership_expr <- function(mapping, model = NULL, var_names = NULL, scope = NULL) {
  stopifnot(inherits(mapping, "mapping"))
  dims <- mapping$dims
  tuple <- pyomo_tuple_from_dims(dims, model = model, var_names = var_names, scope = scope)
  paste0(tuple, " in model.", mapping$name)
}

# Basic symbols ----------------------------------------------------------------

#' @export
as_pyomo.symbol <- function(x, var_names = NULL, scope = NULL, model = NULL, ...) {
  pyomo_resolve_symbol(x$name, var_names = var_names, scope = scope, model = model)
}

#' @export
as_pyomo.shift <- function(x, var_names = NULL, scope = NULL, model = NULL, ...) {
  # shift nodes represent index offsets like ls+1 or y-1
  symbol <- pyomo_resolve_symbol(x$symbol, var_names = var_names, scope = scope, model = model)
  offset <- x$offset
  if (offset > 0) {
    paste0(symbol, " + ", offset)
  } else if (offset < 0) {
    paste0(symbol, " - ", abs(offset))
  } else {
    symbol
  }
}

#' @export
as_pyomo.set <- function(x, scope = NULL, ...) {
  # Check if this set name maps to an iterator variable in scope
  # This happens when we merge expressions into a sum - the merged expression
  # may have set nodes that need to be resolved to the sum's iterator variable
  if (!is.null(scope) && x$name %in% names(scope)) {
    return(scope[[x$name]])
  }
  x$name
}

#' @export
as_pyomo.call <- function(x, model = NULL, var_names = NULL, scope = NULL, ...) {
  func_name <- x$name
  args <- x$args %||% list()
  if (length(args) == 0) {
    return(paste0(func_name, "()"))
  }
  arg_text <- character(length(args))
  for (i in seq_along(args)) {
    arg_text[[i]] <- as_pyomo(args[[i]], model = model, var_names = var_names, scope = scope, ...)
  }
  paste0(func_name, "(", paste(arg_text, collapse = ", "), ")")
}

#' @export
as_pyomo.constant <- function(x, ...) {
  value <- x$value
  if (length(value) != 1) {
    stop("Pyomo export only supports scalar constants at the moment")
  }
  if (is.numeric(value)) {
    return(pyomo_format_scalar(value))
  }
  if (is.character(value)) {
    return(pyomo_quote(value))
  }
  if (is.logical(value)) {
    return(if (value) "True" else "False")
  }
  stop("Unsupported constant type for Pyomo export: ", paste(class(value), collapse = "/"))
}

#' @export
as_pyomo.mapping <- function(x, context = c("reference", "membership"), model = NULL, var_names = NULL, scope = NULL, ...) {
  context <- match.arg(context)
  if (context == "membership") {
    return(pyomo_membership_expr(x, model = model, var_names = var_names, scope = scope))
  }
  paste0("model.", x$name)
}

#' @export
as_pyomo.parameter <- function(x, model = NULL, var_names = NULL, scope = NULL, ...) {
  stopifnot(inherits(x, "parameter"))
  dims <- x$dims %||% x$active_dims
  if (is.null(dims) || length(dims) == 0) {
    return(paste0("model.", x$name))
  }
  subs <- pyomo_index_brackets(dims, model = model, var_names = var_names, scope = scope)
  paste0("model.", x$name, subs)
}

#' @export
as_pyomo.variable <- function(x, model = NULL, var_names = NULL, scope = NULL, ...) {
  stopifnot(inherits(x, "variable"))
  dims <- x$dims
  if (is.null(dims) || length(dims) == 0) {
    return(paste0("model.", x$name))
  }
  subs <- pyomo_index_brackets(dims, model = model, var_names = var_names, scope = scope)
  paste0("model.", x$name, subs)
}

# Expressions ------------------------------------------------------------------

# Helper function to collect all symbol names referenced in an AST node
# Returns both symbol names (like "l") and set names (like "TIMESLICE") 
# since GMPL parsing may resolve iterator symbols to their set types
collect_symbols <- function(node) {
  if (is.null(node)) return(character(0))
  
  symbols <- character(0)
  
  if (inherits(node, "symbol")) {
    return(node$name)
  }
  
  # Also collect set names - they may represent iterator variables outside their scope
  if (inherits(node, "set")) {
    return(node$name)
  }
  
  if (inherits(node, c("parameter", "variable"))) {
    symbols <- c(symbols, node$name)
    if (!is.null(node$dims)) {
      for (dim in node$dims) {
        symbols <- c(symbols, collect_symbols(dim))
      }
    }
    return(symbols)
  }
  
  if (inherits(node, "expression")) {
    symbols <- c(symbols, collect_symbols(node$lhs))
    symbols <- c(symbols, collect_symbols(node$rhs))
    return(symbols)
  }
  
  if (inherits(node, c("sum", "prod", "setmin", "setmax"))) {
    symbols <- c(symbols, collect_symbols(node$value))
    return(symbols)
  }
  
  if (inherits(node, "when")) {
    symbols <- c(symbols, collect_symbols(node$condition))
    symbols <- c(symbols, collect_symbols(node$then))
    symbols <- c(symbols, collect_symbols(node$otherwise))
    return(symbols)
  }
  
  if (inherits(node, "unary")) {
    symbols <- c(symbols, collect_symbols(node$rhs))
    return(symbols)
  }
  
  if (inherits(node, "func")) {
    symbols <- c(symbols, collect_symbols(node$arg))
    return(symbols)
  }
  
  if (inherits(node, "call")) {
    for (arg in node$args) {
      symbols <- c(symbols, collect_symbols(arg))
    }
    return(symbols)
  }
  
  return(symbols)
}

# Python operator precedence (higher number = binds tighter)
# Used to determine when parentheses are needed
pyomo_precedence <- function(op) {
  switch(op,
         "or" = 1,
         "and" = 2,
         "not" = 3,
         "==" = 4, "!=" = 4, "<" = 4, "<=" = 4, ">" = 4, ">=" = 4, "<>" = 4,
         "=" = 4, "EQ" = 4, "NE" = 4, "GT" = 4, "LT" = 4, "GE" = 4, "LE" = 4,
         "+" = 5, "-" = 5,
         "*" = 6, "/" = 6,
         "^" = 7, "**" = 7,
         10)  # default for unknown ops (atoms, function calls, etc.)
}

# Check if a node needs parentheses when used as operand of parent_op
pyomo_needs_parens <- function(node, parent_op, is_rhs = FALSE) {
  if (!inherits(node, "expression")) return(FALSE)
  
  node_prec <- pyomo_precedence(node$op)
  parent_prec <- pyomo_precedence(parent_op)
  
  # Lower precedence always needs parens
  if (node_prec < parent_prec) return(TRUE)
  
  # For right-associative power operator, LHS with same precedence needs parens
  # e.g., (a**b)**c needs parens, but a**(b**c) doesn't
  if (parent_op %in% c("^", "**") && !is_rhs && node_prec == parent_prec) return(TRUE)
  
  # For left-associative ops, RHS with same precedence needs parens for subtraction/division
  # e.g., a - (b - c) needs parens, a - (b + c) needs parens if we want clarity
  if (parent_op %in% c("-", "/") && is_rhs && node_prec == parent_prec) return(TRUE)
  
  FALSE
}

#' @export
as_pyomo.expression <- function(x, model = NULL, var_names = NULL, scope = NULL, in_filter = FALSE, ...) {
  # Special handling for expressions where one side is sum/prod and the other side
  # references variables from the sum's iterator scope (GMPL precedence quirk)
  # Example: sum{l in S} a[l] * b[l] parses as (sum{l in S} a[l]) * b[l]
  # but should be rendered as sum(a[l] * b[l] for l in S)
  if (x$op %in% c("*", "/") && (inherits(x$lhs, c("sum", "prod")) || inherits(x$rhs, c("sum", "prod")))) {
    sum_side <- if (inherits(x$lhs, c("sum", "prod"))) "lhs" else "rhs"
    other_side <- if (sum_side == "lhs") "rhs" else "lhs"
    
    sum_node <- x[[sum_side]]
    other_node <- x[[other_side]]
    
    # Get iterator variable names and their set names from the sum's index
    if (!is.null(sum_node$index) && inherits(sum_node$index, "dims")) {
      iter_vars <- names(sum_node$index)  # e.g., "l"
      # Also get the set names (e.g., "TIMESLICE") since parsing may resolve symbols to sets
      set_names <- character(0)
      for (idx in sum_node$index) {
        if (inherits(idx, "set")) {
          set_names <- c(set_names, idx$name)
        }
      }
      check_names <- c(iter_vars, set_names)
      
      if (length(check_names) > 0) {
        # Check if other_node references any of these iterator variables or their sets
        other_symbols <- collect_symbols(other_node)
        if (any(check_names %in% other_symbols)) {
          # Merge: create new sum with combined value expression
          new_value <- if (sum_side == "lhs") {
            ast_expression(x$op, lhs = sum_node$value, rhs = other_node)
          } else {
            ast_expression(x$op, lhs = other_node, rhs = sum_node$value)
          }
          merged_sum <- sum_node
          merged_sum$value <- new_value
          return(as_pyomo(merged_sum, model = model, var_names = var_names, scope = scope, in_filter = in_filter, ...))
        }
      }
    }
  }
  
  lhs <- as_pyomo(x$lhs, model = model, var_names = var_names, scope = scope, in_filter = in_filter, ...)
  rhs <- as_pyomo(x$rhs, model = model, var_names = var_names, scope = scope, in_filter = in_filter, ...)
  
  # Add parentheses if needed for correct precedence
  if (pyomo_needs_parens(x$lhs, x$op, is_rhs = FALSE)) {
    lhs <- paste0("(", lhs, ")")
  }
  if (pyomo_needs_parens(x$rhs, x$op, is_rhs = TRUE)) {
    rhs <- paste0("(", rhs, ")")
  }
  
  op <- switch(x$op,
               "=" = "==",
               "==" = "==",
               "EQ" = "==",
               "NE" = "!=",
               "<>" = "!=",
               "!=" = "!=",
               "GT" = ">",
               "LT" = "<",
               "GE" = ">=",
               "LE" = "<=",
               "and" = "and",
               "or" = "or",
               "^" = "**",
               "**" = "**",
               x$op)
  paste(lhs, op, rhs)
}

#' @export
as_pyomo.unary <- function(x, model = NULL, var_names = NULL, scope = NULL, ...) {
  rhs <- as_pyomo(x$rhs, model = model, var_names = var_names, scope = scope, ...)
  if (x$op == "-") {
    if (grepl("[-+*/]", rhs)) {
      return(paste0("-(", rhs, ")"))
    }
    return(paste0("-", rhs))
  }
  if (tolower(x$op) == "not") {
    return(paste0("not (", rhs, ")"))
  }
  stop("Unsupported unary operator for Pyomo export: ", x$op)
}

#' @export
as_pyomo.func <- function(x, model = NULL, var_names = NULL, scope = NULL, in_filter = FALSE, ...) {
  fname <- tolower(x$name)
  if (fname %in% c("sum", "prod")) {
    return(pyomo_sum_prod(x, model = model, var_names = var_names, scope = scope, wrapper = fname, ...))
  }
  stop("Unsupported function for Pyomo export: ", x$name)
}

#' @export
as_pyomo.sum <- function(x, model = NULL, var_names = NULL, scope = NULL, ...) {
  pyomo_sum_prod(x, model = model, var_names = var_names, scope = scope, wrapper = "sum", ...)
}

#' @export
as_pyomo.prod <- function(x, model = NULL, var_names = NULL, scope = NULL, ...) {
  pyomo_sum_prod(x, model = model, var_names = var_names, scope = scope, wrapper = "prod", ...)
}

#' @export
as_pyomo.setmin <- function(x, model = NULL, var_names = NULL, scope = NULL, ...) {
  pyomo_min_max(x, wrapper = "min", model = model, var_names = var_names, scope = scope, ...)
}

#' @export
as_pyomo.setmax <- function(x, model = NULL, var_names = NULL, scope = NULL, ...) {
  pyomo_min_max(x, wrapper = "max", model = model, var_names = var_names, scope = scope, ...)
}

pyomo_sum_prod <- function(x, model = NULL, var_names = NULL, scope = NULL, wrapper = NULL, ...) {
  if (is.null(x$index)) {
    stop("sum/prod without index is not supported in Pyomo export")
  }
  iter_info <- pyomo_iter_from_index(x$index, model = model, var_names = var_names, scope = scope)
  local_scope <- scope
  if (!is.null(local_scope) && !is.list(local_scope)) {
    local_scope <- as.list(local_scope)
  }
  if (length(iter_info$scope) > 0) {
    iter_scope <- iter_info$scope
    if (!is.null(iter_scope) && !is.list(iter_scope)) {
      iter_scope <- as.list(iter_scope)
    }
    local_scope <- modifyList(local_scope %||% list(), iter_scope)
  }
  value_expr <- as_pyomo(x$value, model = model, var_names = var_names, scope = local_scope, ...)
  iterators <- paste(iter_info$iterators, collapse = " for ")
  filter_clause <- if (!is.null(iter_info$filter) && nzchar(iter_info$filter)) paste0(" if ", iter_info$filter) else ""
  wrapper <- if (is.null(wrapper)) {
    if (!is.null(x$name) && tolower(x$name) == "prod") "prod" else "sum"
  } else {
    wrapper
  }
  paste0(wrapper, "(", value_expr, " for ", iterators, filter_clause, ")")
}

pyomo_min_max <- function(x, wrapper = c("min", "max"), model = NULL, var_names = NULL, scope = NULL, ...) {
  wrapper <- match.arg(wrapper)
  iter_info <- NULL
  if (!is.null(x$index) && inherits(x$index, "ast")) {
    iter_info <- pyomo_iter_from_index(x$index, model = model, var_names = var_names, scope = scope)
  }

  local_scope <- scope
  if (!is.null(local_scope) && !is.list(local_scope)) {
    local_scope <- as.list(local_scope)
  }

  iterators <- character(0)
  filter_clause <- ""
  if (!is.null(iter_info) && length(iter_info$iterators) > 0) {
    iter_scope <- iter_info$scope
    if (!is.null(iter_scope) && !is.list(iter_scope)) {
      iter_scope <- as.list(iter_scope)
    }
    if (!is.null(iter_scope) && length(iter_scope) > 0) {
      local_scope <- modifyList(local_scope %||% list(), iter_scope)
    }
    iterators <- iter_info$iterators
    if (!is.null(iter_info$filter) && nzchar(iter_info$filter)) {
      filter_clause <- paste0(" if ", iter_info$filter)
    }
  }

  # Optimization: if value is just wrapper(iterator), simplify to wrapper(set)
  # e.g., min{yy in YEAR} min(yy) becomes min(model.YEAR)
  if (!is.null(iter_info) && length(iterators) == 1 && 
      inherits(x$value, "call") && !is.null(x$value$name) &&
      x$value$name == wrapper && length(x$value$args) == 1) {
    arg <- x$value$args[[1]]
    if (inherits(arg, "symbol") && !is.null(arg$name)) {
      # Extract iterator variable name from the iterator string (format: "var in set")
      iter_var <- sub("\\s+in\\s+.*$", "", iterators[1])
      if (arg$name == iter_var) {
        # The value is wrapper(iterator_var), so just use wrapper(set)
        # Extract the set from the iterator string
        set_expr <- sub("^.*\\s+in\\s+", "", iterators[1])
        return(paste0(wrapper, "(", set_expr, ")"))
      }
    }
  }

  value_expr <- as_pyomo(x$value, model = model, var_names = var_names, scope = local_scope, ...)

  if (length(iterators) == 0) {
    return(paste0(wrapper, "(", value_expr, ")"))
  }
  iterator_chain <- paste(iterators, collapse = " for ")
  paste0(wrapper, "(", value_expr, " for ", iterator_chain, filter_clause, ")")
}

pyomo_iter_from_index <- function(index_node, model = NULL, var_names = NULL, scope = NULL) {
  if (inherits(index_node, "when")) {
    iter_syms <- index_node$then
    if (inherits(iter_syms, "symbol")) {
      iter_list <- list(iter_syms)
    } else if (inherits(iter_syms, "dims")) {
      iter_list <- iter_syms
    } else if (is.list(iter_syms)) {
      iter_list <- iter_syms
    } else {
      stop("Unsupported iterator specification in sum/prod when-node")
    }
    iter_names <- character(length(iter_list))
    for (i in seq_along(iter_list)) {
      iter_names[[i]] <- pyomo_dim_name(iter_list[[i]])
    }
    existing_names <- if (!is.null(scope)) unname(unlist(scope)) else character(0)
    iterator_specs <- pyomo_build_iterators(iter_names, model = model, existing = existing_names)
    iterators <- character(length(iterator_specs))
    scope_map <- character(0)
    for (i in seq_along(iterator_specs)) {
      spec <- iterator_specs[[i]]
      iterators[[i]] <- paste0(spec$iterator, " in model.", pyomo_resolve_base_set(spec$set, model))
      scope_map[iter_names[[i]]] <- spec$iterator
    }
    filter_expr <- NULL
    if (!is.null(index_node$condition) && inherits(index_node$condition, "mapping")) {
      base_scope <- scope
      if (!is.null(base_scope) && !is.list(base_scope)) {
        base_scope <- as.list(base_scope)
      }
      iter_scope <- scope_map
      if (!is.null(iter_scope) && !is.list(iter_scope)) {
        iter_scope <- as.list(iter_scope)
      }
      filter_scope <- modifyList(base_scope %||% list(), iter_scope)
      filter_expr <- pyomo_membership_expr(index_node$condition, model = model, var_names = var_names, scope = filter_scope)
    }
    return(list(iterators = iterators, filter = filter_expr, scope = scope_map))
  }

  dims <- index_node
  if (!inherits(dims, "dims")) {
    dims <- ast_dims(list(dims))
  }
  
  # Build iterator specs from dims, using iterator names from names(dims) if available
  iterator_specs <- vector("list", length(dims))
  existing_names <- if (!is.null(scope)) unname(unlist(scope)) else character(0)
  used_names <- existing_names
  
  for (i in seq_along(dims)) {
    set_name <- pyomo_dim_name(dims[[i]])
    base_set <- pyomo_resolve_base_set(set_name, model)
    
    # Use iterator name from names(dims) if available, otherwise use set name
    if (!is.null(names(dims)) && i <= length(names(dims)) && nzchar(names(dims)[i])) {
      iter_var <- names(dims)[i]
    } else {
      iter_var <- set_name
    }
    
    # Ensure unique iterator name
    iter_name <- pyomo_build_iterator_name(iter_var, used_names)
    used_names <- c(used_names, iter_name)
    
    iterator_specs[[i]] <- list(symbol = set_name, iterator = iter_name, set = base_set)
  }
  
  # Build iterator strings and scope map
  iterators <- character(length(iterator_specs))
  scope_map <- character(0)
  for (i in seq_along(iterator_specs)) {
    spec <- iterator_specs[[i]]
    iterators[[i]] <- paste0(spec$iterator, " in model.", spec$set)
    scope_map[spec$symbol] <- spec$iterator
    # Also map iterator variable name if different from set name
    if (!is.null(names(dims)) && i <= length(names(dims)) && nzchar(names(dims)[i])) {
      scope_map[names(dims)[i]] <- spec$iterator
    }
  }
  list(iterators = iterators, filter = NULL, scope = scope_map)
}

#' @export
as_pyomo.when <- function(x, model = NULL, var_names = NULL, scope = NULL, ...) {
  cond <- as_pyomo(x$condition, model = model, var_names = var_names, scope = scope, ...)
  then <- as_pyomo(x$then, model = model, var_names = var_names, scope = scope, ...)
  otherwise <- if (is.null(x$otherwise)) "0" else as_pyomo(x$otherwise, model = model, var_names = var_names, scope = scope, ...)
  paste0("(", then, " if ", cond, " else ", otherwise, ")")
}

# Equations -------------------------------------------------------------------

#' Convert an equation node to Pyomo rule definition (internal helper)
#' @keywords internal
render_pyomo_equation <- function(eq, model = NULL) {
  stopifnot(inherits(eq, "equation"))
  dim_names <- pyomo_collect_dim_names(eq$dims)
  iter_map <- if (!is.null(eq$dims_index_aliases)) eq$dims_index_aliases else {
    aliases <- list()
    if (length(dim_names) > 0) {
      specs <- pyomo_build_iterators(dim_names, model = model)
      aliases <- character(0)
      for (i in seq_along(specs)) {
        aliases[dim_names[[i]]] <- specs[[i]]$iterator
      }
    }
    aliases
  }
  scope <- iter_map
  lhs <- as_pyomo(eq$lhs, model = model, var_names = iter_map, scope = scope)
  rhs <- as_pyomo(eq$rhs, model = model, var_names = iter_map, scope = scope)
  relation <- switch(eq$relation,
                     "==" = "==",
                     "<=" = "<=",
                     ">=" = ">=",
                     stop("Unsupported equation relation: ", eq$relation))
  comparison <- paste(lhs, relation, rhs)
  rule_name <- paste0("_", pyomo_safe_name(eq$name), "_rule")
  iter_args <- if (length(iter_map) > 0) paste(names(iter_map), collapse = ", ") else ""
  header <- if (nzchar(iter_args)) sprintf("def %s(model, %s):", rule_name, paste(iter_map, collapse = ", ")) else sprintf("def %s(model):", rule_name)
  lines <- c(header)
  if (!is.null(eq$domain)) {
    if (inherits(eq$domain, "mapping")) {
      membership <- pyomo_membership_expr(eq$domain, model = model, var_names = iter_map, scope = scope)
      lines <- c(lines, sprintf("    if not (%s):", membership), "        return Constraint.Skip")
    } else if (inherits(eq$domain, "expression") || inherits(eq$domain, "ast")) {
      # Domain is a conditional expression - convert to Python boolean check
      domain_expr <- as_pyomo(eq$domain, model = model, var_names = iter_map, scope = scope)
      lines <- c(lines, sprintf("    if not (%s):", domain_expr), "        return Constraint.Skip")
    }
  }
  lines <- c(lines, sprintf("    _expr = %s", comparison))
  lines <- c(lines, "    if _expr is True or _expr is False:")
  lines <- c(lines, "        return Constraint.Feasible if _expr else Constraint.Infeasible")
  lines <- c(lines, "    return _expr", "")
  assign_line <- if (length(dim_names) > 0) {
    base_sets <- character(length(dim_names))
    for (i in seq_along(dim_names)) {
      base_sets[[i]] <- pyomo_resolve_base_set(dim_names[[i]], model = model)
    }
    index_sets <- paste(sprintf("model.%s", base_sets), collapse = ", ")
    sprintf("model.%s = Constraint(%s, rule=%s)", eq$name, index_sets, rule_name)
  } else {
    sprintf("model.%s = Constraint(rule=%s)", eq$name, rule_name)
  }
  list(rule = lines, assignment = assign_line)
}