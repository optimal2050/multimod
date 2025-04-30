#' Create a multimod parameter
#'
#' @param name character, name of the parameter
#' @param declared_dims character vector, names of the declared dimensions
#' @param data data frame, data for the parameter
#' @param active_dims character vector, names of the active dimensions
#' @param auto_fold logical, whether to automatically fold dimensions
#'
#' @returns a multimod_param object
#' @export
#'
#' @examples
new_multimod_param <- function(
    name,
    declared_dims,
    data,
    active_dims = NULL,
    auto_fold = FALSE
    ) {
  if (is.null(active_dims)) {
    if (auto_fold) {
      dims_folded <- fold_param_dims(data, declared_dims)
      active_dims <- dims_folded$active_dims
    } else {
      active_dims <- setdiff(names(data), "value")
    }
  }

  structure(
    list(
      name = name,
      declared_dims = declared_dims,
      active_dims = active_dims,
      data = as.data.frame(data)
    ),
    class = "multimod_param"
  )
}

#' Create a multimod variable
#'
#' @param name character, name of the variable
#' @param declared_dims character vector, names of the declared dimensions
#' @param data data frame, data for the variable
#' @param active_dims character vector, names of the active dimensions
#' @param domain character, domain of the variable (e.g., "continuous", "integer", "binary")
#' @param auto_fold logical, whether to automatically fold dimensions
#'
#' @returns a multimod_var object
#' @export
#'
#' @examples
new_multimod_var <- function(name, declared_dims, data,
                             active_dims = NULL,
                             domain = "continuous",
                             auto_fold = TRUE) {
  stopifnot(domain %in% c("continuous", "integer", "binary"))

  if (is.null(active_dims)) {
    if (auto_fold) {
      dims_folded <- fold_param_dims(data, declared_dims)
      active_dims <- dims_folded$active_dims
    } else {
      active_dims <- setdiff(names(data), c("value", "lb", "ub", "start"))
    }
  }

  structure(
    list(
      name = name,
      declared_dims = declared_dims,
      active_dims = active_dims,
      domain = domain,
      data = as.data.frame(data)
    ),
    class = "multimod_var"
  )
}


#' Create a multimod equation object
#'
#' @param name
#' @param declared_dims
#' @param lhs
#' @param rhs
#' @param sense
#' @param domain
#' @param desc
#'
#' @returns
#' @export
#'
#' @examples
new_multimod_eqn <- function(
    name, declared_dims, lhs, rhs, sense = "==", domain = NULL, desc = NULL) {
  stopifnot(sense %in% c("==", "<=", ">="))

  structure(
    list(
      name = name,
      declared_dims = declared_dims,
      domain = domain,
      lhs = lhs,
      sense = sense,
      rhs = rhs,
      desc = desc
    ),
    class = "multimod_eqn"
  )
}

#' Create a multimod model object
#'
#' @param sets named list of sets
#' @param mappings named list of mappings
#' @param parameters named list of multimod_param objects
#' @param variables named list of multimod_var objects
#' @param equations named list of multimod_eqn objects
#' @param desc
#'
#' @returns
#' @export
#'
#' @examples
new_multimod_model <- function(
    sets = list(),
    mappings = list(),
    parameters = list(),
    variables = list(),
    equations = list(),
    desc = NULL) {
  structure(
    list(
      sets = sets, # Named character vectors (optional descriptions)
      mappings = mappings, # Named list of mapping sets
      parameters = parameters, # Named list of multimod_param
      variables = variables, # Named list of multimod_var
      equations = equations, # Named list of multimod_eqn
      desc = desc
    ),
    class = "multimod_model"
  )
}


fold_param_dims <- function(data_df, declared_dims) {
  require(data.table)
  dt <- as.data.table(data_df)

  folded_dims <- c()

  for (dim in declared_dims) {
    if (!(dim %in% names(dt))) next

    all_na_check <- all(is.na(dt$value))

    other_dims <- setdiff(declared_dims, dim)
    if (length(other_dims) == 0) {
      unique_values <- unique(dt$value)
      no_variation <- length(unique_values[!is.na(unique_values)]) <= 1
    } else {
      variation_by_group <- dt[, .(unique_vals = unique(value)), by = other_dims]
      no_variation <- all(variation_by_group[, .N == 1])
    }

    if (all_na_check || no_variation) {
      folded_dims <- c(folded_dims, dim)
    }
  }

  remaining_dims <- setdiff(declared_dims, folded_dims)

  list(
    active_dims = remaining_dims,
    folded_dims = folded_dims
  )
}

expand_multimod_param <- function(param, full_domains = NULL, domain_table = NULL) {
  stopifnot(inherits(param, "multimod_param"))

  declared <- param$declared_dims
  active <- param$active_dims
  data <- param$data

  if (!is.null(domain_table)) {
    grid <- as.data.frame(domain_table)
  } else if (!is.null(full_domains)) {
    grid <- do.call(expand.grid, full_domains[declared])
  } else {
    stop("Either `full_domains` or `domain_table` must be provided.")
  }

  merged <- merge(
    grid,
    data,
    by = intersect(active, names(grid)),
    all.x = TRUE,
    sort = FALSE
  )

  if (anyNA(merged$value)) {
    warning("Some parameter values are missing after expansion (NAs present).")
  }

  return(merged)
}

validate_multimod_eqn <- function(eqn, symbols = list()) {
  errors <- character()

  if (!inherits(eqn, "multimod_eqn")) {
    errors <- c(errors, "Object is not of class 'multimod_eqn'.")
    return(errors)
  }

  if (is.null(eqn$name) || !is.character(eqn$name) || nchar(eqn$name) == 0) {
    errors <- c(errors, "Equation 'name' is missing or invalid.")
  }

  if (!is.character(eqn$declared_dims)) {
    errors <- c(errors, "Declared dimensions must be a character vector.")
  }

  if (!eqn$sense %in% c("==", "<=", ">=")) {
    errors <- c(errors, paste0("Equation sense '", eqn$sense, "' is invalid. Must be '==', '<=', or '>='."))
  }

  check_expr_dims <- function(expr, dims = eqn$declared_dims) {
    if (is.null(expr)) {
      return(NULL)
    }
    if (expr$type %in% c("var", "param", "symbol", "func", "set")) {
      if (!is.null(expr$args)) {
        undeclared <- setdiff(expr$args, dims)
        if (length(undeclared) > 0) {
          return(paste("Expression uses undeclared indices:", paste(undeclared, collapse = ", ")))
        }
      }
    } else if (expr$type %in% c("expr", "cond", "sum", "prod", "compare", "logic")) {
      return(c(
        check_expr_dims(expr$left, dims),
        check_expr_dims(expr$right, dims),
        check_expr_dims(expr$value, dims),
        check_expr_dims(expr$condition, dims),
        check_expr_dims(expr$domain, dims)
      ))
    }
    return(NULL)
  }

  expr_problems <- c(
    check_expr_dims(eqn$lhs),
    check_expr_dims(eqn$rhs),
    check_expr_dims(eqn$domain)
  )
  expr_problems <- expr_problems[!sapply(expr_problems, is.null)]

  if (length(expr_problems) > 0) {
    errors <- c(errors, expr_problems)
  }

  if (length(errors) == 0) {
    return(TRUE)
  } else {
    return(errors)
  }
}

detect_symbol_type <- function(
    name, symbols,
    known_funcs = c("log", "exp", "abs", "sqrt")) {
  if (!is.null(symbols$variables) && name %in% symbols$variables) {
    return("var")
  } else if (!is.null(symbols$parameters) && name %in% symbols$parameters) {
    return("param")
  } else if (!is.null(symbols$mappings) && name %in% symbols$mappings) {
    return("mapping")
  } else if (!is.null(symbols$sets) && name %in% symbols$sets) {
    return("set")
  } else if (name %in% known_funcs) {
    return("func")
  } else {
    return("param") # fallback (could also be "unknown")
  }
}
