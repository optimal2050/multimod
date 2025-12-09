# Helpers -----------------------------------------------------------------

coalesce_scalar <- function(x, fallback) {
  if (!is.null(x) && length(x) > 0 && !is.na(x[1]) && nzchar(as.character(x[1]))
      ) {
    return(as.character(x[1]))
  }
  fallback
}

format_validation_prefix <- function(context) {
  if (is.null(context) || !nzchar(context)) "" else sprintf("[%s] ", context)
}

finalize_ast_validation <- function(errors, warnings, info, context, stop_on_error) {
  result <- list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    info = info,
    context = context
  )
  if (stop_on_error && !result$valid) {
    prefix <- format_validation_prefix(context)
    stop(paste0(prefix, result$errors[1]), call. = FALSE)
  }
  result
}

is_validation_result <- function(x) {
  is.list(x) && !is.null(x$valid) && !is.null(x$errors) &&
    !is.null(x$warnings) && !is.null(x$info) && !is.null(x$context)
}

format_result_messages <- function(result, field) {
  values <- result[[field]]
  if (length(values) == 0) {
    return(character())
  }
  prefix <- format_validation_prefix(result$context)
  paste0(prefix, values)
}

flatten_validation_messages <- function(results, field = c("errors", "warnings", "info")) {
  field <- match.arg(field)
  messages <- character()
  walker <- function(node) {
    if (is_validation_result(node)) {
      messages <<- c(messages, format_result_messages(node, field))
      return(invisible(NULL))
    }
    if (is.list(node) && length(node) > 0) {
      for (child in node) {
        walker(child)
      }
    }
    invisible(NULL)
  }

  walker(results)
  messages
}


#' Validate multimod or AST objects
#'
#' Performs structural validation on low-level AST nodes and multimod objects
#' (parameters, variables, equations, etc.) as well as whole model objects.
#' These helpers are intended to catch malformed objects early (e.g.,
#' dimensions stored as raw character vectors instead of `ast_dims`). Call with
#' `stop_on_error = FALSE` to collect diagnostics without raising errors.
#'
#' @param x Object to validate (AST node, multimod component, or model)
#' @param context Optional character label identifying the object (used in
#'   error messages for AST or component validators)
#' @param stop_on_error Logical; if TRUE (default) validation errors raise
#'   immediately. When FALSE, a structured result is returned.
#' @param recursive Logical; when TRUE (default) validators recursively inspect
#'   known child AST objects (e.g., dims, formulas). Set to FALSE to limit
#'   validation to the top-level object only (ignored for model validation).
#' @param ... Additional arguments passed to class-specific validators (e.g.,
#'   `verbose` for models or `detailed_report` for parameters).
#'
#' @return A list containing `valid`, `errors`, `warnings`, `info`, and `context`
#'   when `stop_on_error = FALSE`. Invisibly returns the same result when
#'   `stop_on_error = TRUE` and validation passes.
#' @export
validate <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  UseMethod("validate")
}

#' @export
#' @rdname validate
validate.default <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  class_label <- paste(class(x), collapse = "/")
  context <- coalesce_scalar(context, sprintf("unsupported object (%s)", class_label))
  info <- sprintf("Skipping validation for class '%s' (no validate() method)", class_label)
  result <- list(
    valid = NA,
    errors = character(),
    warnings = character(),
    info = info,
    context = context
  )
  result
}

#' @export
#' @rdname validate
validate.ast <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  context <- coalesce_scalar(context, "ast")
  errors <- character()
  if (!inherits(x, "ast")) {
    errors <- c(errors, "Object does not inherit class 'ast'")
    return(finalize_ast_validation(errors, character(), character(), context, stop_on_error))
  }
  if (!is.list(x)) {
    errors <- c(errors, "AST nodes must be stored as lists")
  }
  finalize_ast_validation(errors, character(), character(), context, stop_on_error)
}

#' @export
#' @rdname validate
validate.dims <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  context <- coalesce_scalar(context, "dims")
  errors <- character()
  warnings <- character()

  if (!inherits(x, "dims")) {
    errors <- c(errors, "Object does not inherit class 'dims'")
  } else {
    if (!is.list(x)) {
      errors <- c(errors, "dims must be represented as a list of AST nodes")
    } else if (length(x) > 0) {
      invalid_entries <- which(!vapply(x, function(entry) inherits(entry, "ast"), logical(1)))
      if (length(invalid_entries) > 0) {
        errors <- c(errors, sprintf("Non-AST entries found at positions: %s", paste(invalid_entries, collapse = ", ")))
      }
    }

    dim_names <- names(x)
    if (!is.null(dim_names) && any(dim_names == "")) {
      warnings <- c(warnings, "Some dimension entries are unnamed; consider adding labels for clarity")
    }
  }

  finalize_ast_validation(errors, warnings, character(), context, stop_on_error)
}

#' @export
#' @rdname validate
validate.set <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  dots <- list(...)
  detailed_report <- isTRUE(dots$detailed_report)

  context <- coalesce_scalar(context, sprintf("set %s", coalesce_scalar(x$name, "<unnamed>")))
  errors <- character()
  warnings <- character()
  info <- character()

  if (!inherits(x, "set")) {
    errors <- c(errors, "Object does not inherit class 'set'")
  }
  if (!inherits(x, "ast")) {
    warnings <- c(warnings, "Set does not inherit class 'ast'; unexpected constructor?")
  }

  if (!is.list(x)) {
    errors <- c(errors, "Set objects must be represented as named lists")
    return(finalize_ast_validation(errors, warnings, info, context, stop_on_error))
  }

  field_check <- check_named_fields(
    x,
    required_fields = "name",
    optional_fields = c("desc", "subset_of", "data"),
    context = context,
    detailed_report = detailed_report
  )
  errors <- c(errors, field_check$errors)
  info <- c(info, field_check$info)

  name_value <- x[["name"]]
  if (is.null(name_value) || !is.character(name_value) || length(name_value) != 1 || !nzchar(name_value)) {
    errors <- c(errors, "Set must have a single non-empty name")
  }

  desc_value <- x[["desc"]]
  if (!is.null(desc_value) && (!is.character(desc_value) || length(desc_value) != 1)) {
    warnings <- c(warnings, "Set desc should be a single character value when provided")
  }

  subset_value <- x[["subset_of"]]
  if (!is.null(subset_value) && !is.character(subset_value)) {
    errors <- c(errors, "subset_of must be a character vector when provided")
  }

  data_value <- x[["data"]]
  if (!is.null(data_value) && !is.vector(data_value) && !is.list(data_value) && !is.data.frame(data_value)) {
    warnings <- c(warnings, "Set data should be a vector, list, data.frame, or NULL")
  }

  finalize_ast_validation(errors, warnings, info, context, stop_on_error)
}

#' @export
#' @rdname validate
validate.symbol <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  dots <- list(...)
  detailed_report <- isTRUE(dots$detailed_report)

  context <- coalesce_scalar(context, sprintf("symbol %s", coalesce_scalar(x$name, "<unnamed>")))
  errors <- character()
  warnings <- character()
  info <- character()

  if (!inherits(x, "symbol")) {
    errors <- c(errors, "Object does not inherit class 'symbol'")
  }
  if (!inherits(x, "ast")) {
    warnings <- c(warnings, "Symbol does not inherit class 'ast'; unexpected constructor?")
  }

  if (!is.list(x)) {
    errors <- c(errors, "Symbol objects must be represented as named lists")
    return(finalize_ast_validation(errors, warnings, info, context, stop_on_error))
  }

  field_check <- check_named_fields(x, required_fields = "name", context = context, detailed_report = detailed_report)
  errors <- c(errors, field_check$errors)
  info <- c(info, field_check$info)

  name_value <- x[["name"]]
  if (is.null(name_value) || !is.character(name_value) || length(name_value) != 1 || !nzchar(name_value)) {
    errors <- c(errors, "Symbol must have a single non-empty name")
  }

  finalize_ast_validation(errors, warnings, info, context, stop_on_error)
}

#' @export
#' @rdname validate
validate.mapping <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  dots <- list(...)
  detailed_report <- isTRUE(dots$detailed_report)

  context <- coalesce_scalar(context, sprintf("mapping %s", coalesce_scalar(x$name, "<unnamed>")))
  errors <- character()
  warnings <- character()
  info <- character()

  if (!inherits(x, "mapping")) {
    errors <- c(errors, "Object does not inherit class 'mapping'")
  }
  if (!inherits(x, "ast")) {
    warnings <- c(warnings, "Mapping does not inherit class 'ast'; unexpected constructor?")
  }

  if (!is.list(x)) {
    errors <- c(errors, "Mapping objects must be represented as named lists")
    return(finalize_ast_validation(errors, warnings, info, context, stop_on_error))
  }

  field_check <- check_named_fields(
    x,
    required_fields = c("name", "dims"),
    optional_fields = c("desc", "active_dims", "data"),
    context = context,
    detailed_report = detailed_report
  )
  errors <- c(errors, field_check$errors)
  info <- c(info, field_check$info)

  name_value <- x[["name"]]
  if (is.null(name_value) || !is.character(name_value) || length(name_value) != 1 || !nzchar(name_value)) {
    errors <- c(errors, "Mapping must have a single non-empty name")
  }

  desc_value <- x[["desc"]]
  if (!is.null(desc_value) && (!is.character(desc_value) || length(desc_value) != 1)) {
    warnings <- c(warnings, "Mapping desc should be a single character value when provided")
  }

  dims_value <- x[["dims"]]
  if (is.null(dims_value)) {
    errors <- c(errors, "Mapping dims slot is NULL; expected ast_dims output")
  } else {
    if (!inherits(dims_value, "dims")) {
      errors <- c(errors, "Mapping dims must inherit class 'dims'")
    } else if (recursive) {
      dim_result <- validate(dims_value, context = paste0(context, "::dims"), stop_on_error = FALSE, recursive = recursive)
      errors <- c(errors, dim_result$errors)
      warnings <- c(warnings, dim_result$warnings)
    }
  }

  active_dims_value <- x[["active_dims"]]
  if (inherits(x, "multimod") && is.null(active_dims_value)) {
    errors <- c(errors, "Mapping active_dims slot is NULL; expected ast_dims output")
  } else if (!is.null(active_dims_value)) {
    if (!inherits(active_dims_value, "dims")) {
      errors <- c(errors, "Mapping active_dims must inherit class 'dims'")
    } else if (recursive) {
      active_result <- validate(active_dims_value, context = paste0(context, "::active_dims"), stop_on_error = FALSE, recursive = recursive)
      errors <- c(errors, active_result$errors)
      warnings <- c(warnings, active_result$warnings)
    }
  }

  data_value <- x[["data"]]
  if (!is.null(data_value) && !is.data.frame(data_value)) {
    warnings <- c(warnings, "Mapping data should be NULL or a data.frame (as produced by new_mapping())")
  }

  finalize_ast_validation(errors, warnings, info, context, stop_on_error)
}

#' @export
#' @rdname validate
validate.ast_formula <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  dots <- list(...)
  detailed_report <- isTRUE(dots$detailed_report)

  context <- coalesce_scalar(context, sprintf("ast_formula %s", coalesce_scalar(x$target, "<unknown>")))
  errors <- character()
  warnings <- character()
  info <- character()

  if (!inherits(x, "ast_formula")) {
    errors <- c(errors, "Object does not inherit class 'ast_formula'")
  }
  if (!inherits(x, "ast")) {
    warnings <- c(warnings, "ast_formula does not inherit class 'ast'; unexpected constructor?")
  }

  if (!is.list(x)) {
    errors <- c(errors, "ast_formula objects must be represented as named lists")
    return(finalize_ast_validation(errors, warnings, info, context, stop_on_error))
  }

  field_check <- check_named_fields(
    x,
    required_fields = c("target", "field", "expr"),
    optional_fields = c("index_vars", "index_sets", "args"),
    context = context,
    detailed_report = detailed_report
  )
  errors <- c(errors, field_check$errors)
  info <- c(info, field_check$info)

  field_value <- x[["field"]]
  if (is.null(field_value) || !field_value %in% c("defVal", "formula")) {
    errors <- c(errors, "ast_formula$field must be either 'defVal' or 'formula'")
  }

  expr_value <- x[["expr"]]
  if (is.null(expr_value) || !inherits(expr_value, "ast")) {
    errors <- c(errors, "ast_formula$expr must be an AST node")
  }

  target_value <- x[["target"]]
  if (is.null(target_value) || !is.character(target_value) || length(target_value) != 1 || !nzchar(target_value)) {
    warnings <- c(warnings, "ast_formula$target should be a single non-empty character value")
  }

  if (!is.null(x[["index_vars"]]) && !is.character(x[["index_vars"]])) {
    errors <- c(errors, "index_vars must be character")
  }
  if (!is.null(x[["index_sets"]]) && !is.character(x[["index_sets"]])) {
    errors <- c(errors, "index_sets must be character")
  }
  if (!is.null(x[["args"]]) && !is.character(x[["args"]])) {
    errors <- c(errors, "args must be character")
  }

  finalize_ast_validation(errors, warnings, info, context, stop_on_error)
}

validate_alias_vector <- function(alias_vec, context) {
  errors <- character()
  warnings <- character()
  if (!is.character(alias_vec)) {
    errors <- c(errors, "Alias metadata must be stored as a character vector")
    return(list(errors = errors, warnings = warnings))
  }

  alias_names <- names(alias_vec)
  if (!is.null(alias_names) && any(alias_names == "")) {
    warnings <- c(warnings, "Alias vector has empty names; unable to associate with dimensions")
  }
  list(errors = errors, warnings = warnings)
}

check_named_fields <- function(x, required_fields, optional_fields = character(), context, detailed_report = FALSE) {
  errors <- character()
  info <- character()

  fields <- names(x)
  if (is.null(fields)) {
    errors <- c(errors, sprintf("%s list must have named fields to match constructor output", context))
    return(list(errors = errors, info = info))
  }

  missing_fields <- setdiff(required_fields, fields)
  if (length(missing_fields) > 0) {
    errors <- c(errors, sprintf(
      "%s missing required field(s): %s",
      context,
      paste(missing_fields, collapse = ", ")
    ))
  }

  if (detailed_report) {
    extra_fields <- setdiff(fields, union(required_fields, optional_fields))
    if (length(extra_fields) > 0) {
      info <- c(info, sprintf(
        "%s has additional field(s) beyond constructor output: %s",
        context,
        paste(extra_fields, collapse = ", ")
      ))
    }
  }

  list(errors = errors, info = info)
}

#' @export
#' @rdname validate
validate.parameter <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  dots <- list(...)
  detailed_report <- isTRUE(dots$detailed_report)

  context <- coalesce_scalar(context, sprintf("parameter %s", coalesce_scalar(x$name, "<unnamed>")))
  errors <- character()
  warnings <- character()
  info <- character()

  expected_fields <- c("name", "desc", "dims", "active_dims", "data", "defVal", "defInt", "symbolic", "formula", "comment")

  if (!inherits(x, "parameter")) {
    errors <- c(errors, "Object does not inherit class 'parameter'")
  }
  if (!inherits(x, "ast")) {
    errors <- c(errors, "Parameter must inherit class 'ast'")
  }
  if (!inherits(x, "multimod")) {
    warnings <- c(warnings, "Parameter does not inherit class 'multimod'; downstream tooling may rely on this")
  }

  if (!is.list(x)) {
    errors <- c(errors, "Parameter objects must be represented as named lists")
    return(finalize_ast_validation(errors, warnings, info, context, stop_on_error))
  }

  field_check <- check_named_fields(x, required_fields = expected_fields, context = context, detailed_report = detailed_report)
  errors <- c(errors, field_check$errors)
  info <- c(info, field_check$info)

  name_value <- x[["name"]]
  if (is.null(name_value) || !is.character(name_value) || length(name_value) != 1 || !nzchar(name_value)) {
    errors <- c(errors, "Parameter must have a single non-empty name")
  }

  desc_value <- x[["desc"]]
  if (!is.null(desc_value) && (!is.character(desc_value) || length(desc_value) != 1)) {
    warnings <- c(warnings, "Parameter desc should be a single character value when provided")
  }

  dims_value <- x[["dims"]]
  if (is.null(dims_value)) {
    errors <- c(errors, "Parameter dims slot is NULL; expected ast_dims output")
  } else {
    if (!inherits(dims_value, "dims")) {
      errors <- c(errors, "dims slot must inherit class 'dims'")
    } else if (recursive) {
      dims_result <- validate(dims_value, context = paste0(context, "::dims"), stop_on_error = FALSE, recursive = recursive)
      errors <- c(errors, dims_result$errors)
      warnings <- c(warnings, dims_result$warnings)
    }
  }

  active_dims_value <- x[["active_dims"]]
  if (is.null(active_dims_value)) {
    errors <- c(errors, "Parameter active_dims slot is NULL; expected ast_dims output")
  } else {
    if (!inherits(active_dims_value, "dims")) {
      errors <- c(errors, "active_dims slot must inherit class 'dims'")
    } else if (recursive) {
      active_result <- validate(active_dims_value, context = paste0(context, "::active_dims"), stop_on_error = FALSE, recursive = recursive)
      errors <- c(errors, active_result$errors)
      warnings <- c(warnings, active_result$warnings)
    }
  }

  data_value <- x[["data"]]
  if (!is.null(data_value) && !is.data.frame(data_value)) {
    warnings <- c(warnings, "Parameter data should be NULL or a data.frame (as produced by new_parameter())")
  }

  symbolic_value <- x[["symbolic"]]
  if (is.null(symbolic_value) || !is.logical(symbolic_value) || length(symbolic_value) != 1 || is.na(symbolic_value)) {
    errors <- c(errors, "Parameter symbolic flag must be a single logical value")
  }

  formula_value <- x[["formula"]]
  if (!is.null(formula_value) && !inherits(formula_value, "ast_formula")) {
    errors <- c(errors, "Parameter formula must be stored as an ast_formula object")
  }

  comment_value <- x[["comment"]]
  if (!is.null(comment_value) && (!is.character(comment_value) || length(comment_value) != 1)) {
    warnings <- c(warnings, "Parameter comment should be a single character value when provided")
  }

  if (!is.null(x[["dims_index_aliases"]])) {
    alias_check <- validate_alias_vector(x[["dims_index_aliases"]], context)
    errors <- c(errors, alias_check$errors)
    warnings <- c(warnings, alias_check$warnings)
  }

  finalize_ast_validation(errors, warnings, info, context, stop_on_error)
}

#' @export
#' @rdname validate
validate.variable <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  dots <- list(...)
  detailed_report <- isTRUE(dots$detailed_report)

  context <- coalesce_scalar(context, sprintf("variable %s", coalesce_scalar(x$name, "<unnamed>")))
  errors <- character()
  warnings <- character()
  info <- character()

  if (!inherits(x, "variable")) {
    errors <- c(errors, "Object does not inherit class 'variable'")
  }
  if (!inherits(x, "ast")) {
    warnings <- c(warnings, "Variable does not inherit class 'ast'; unexpected constructor?")
  }

  if (!is.list(x)) {
    errors <- c(errors, "Variable objects must be represented as named lists")
    return(finalize_ast_validation(errors, warnings, info, context, stop_on_error))
  }

  field_check <- check_named_fields(
    x,
    required_fields = c("name", "dims"),
    optional_fields = c("desc", "active_dims", "domain", "vtype", "bounds", "comment", "data"),
    context = context,
    detailed_report = detailed_report
  )
  errors <- c(errors, field_check$errors)
  info <- c(info, field_check$info)

  name_value <- x[["name"]]
  if (is.null(name_value) || !is.character(name_value) || length(name_value) != 1 || !nzchar(name_value)) {
    errors <- c(errors, "Variable must have a single non-empty name")
  }

  desc_value <- x[["desc"]]
  if (!is.null(desc_value) && (!is.character(desc_value) || length(desc_value) != 1)) {
    warnings <- c(warnings, "Variable desc should be a single character value when provided")
  }

  dims_value <- x[["dims"]]
  if (is.null(dims_value)) {
    errors <- c(errors, "Variable dims slot is NULL; expected ast_dims output")
  } else if (!inherits(dims_value, "dims")) {
    errors <- c(errors, "Variable dims must inherit class 'dims'")
  } else if (recursive) {
    dims_result <- validate(dims_value, context = paste0(context, "::dims"), stop_on_error = FALSE, recursive = recursive)
    errors <- c(errors, dims_result$errors)
    warnings <- c(warnings, dims_result$warnings)
  }

  active_dims_value <- x[["active_dims"]]
  if (inherits(x, "multimod") && is.null(active_dims_value)) {
    errors <- c(errors, "Variable active_dims slot is NULL; expected ast_dims output")
  } else if (!is.null(active_dims_value)) {
    if (!inherits(active_dims_value, "dims")) {
      errors <- c(errors, "Variable active_dims must inherit class 'dims'")
    } else if (recursive) {
      active_result <- validate(active_dims_value, context = paste0(context, "::active_dims"), stop_on_error = FALSE, recursive = recursive)
      errors <- c(errors, active_result$errors)
      warnings <- c(warnings, active_result$warnings)
    }
  }

  domain_value <- x[["domain"]]
  if (!is.null(domain_value) && !inherits(domain_value, "mapping") && !inherits(domain_value, "ast")) {
    warnings <- c(warnings, "Variable domain should be NULL, a mapping, or an AST node")
  }

  vtype_value <- x[["vtype"]]
  if (!is.null(vtype_value) && !vtype_value %in% c("continuous", "integer", "binary")) {
    warnings <- c(warnings, sprintf("Unexpected variable type '%s'", vtype_value))
  }

  comment_value <- x[["comment"]]
  if (!is.null(comment_value) && (!is.character(comment_value) || length(comment_value) != 1)) {
    warnings <- c(warnings, "Variable comment should be a single character value when provided")
  }

  data_value <- x[["data"]]
  if (!is.null(data_value) && !is.data.frame(data_value)) {
    warnings <- c(warnings, "Variable data should be NULL or a data.frame")
  }

  finalize_ast_validation(errors, warnings, info, context, stop_on_error)
}

#' @export
#' @rdname validate
validate.equation <- function(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...) {
  dots <- list(...)
  detailed_report <- isTRUE(dots$detailed_report)

  context <- coalesce_scalar(context, sprintf("equation %s", coalesce_scalar(x$name, "<unnamed>")))
  errors <- character()
  warnings <- character()
  info <- character()

  if (!inherits(x, "equation")) {
    errors <- c(errors, "Object does not inherit class 'equation'")
  }
  if (!inherits(x, "ast")) {
    warnings <- c(warnings, "Equation does not inherit class 'ast'; unexpected constructor?")
  }

  if (!is.list(x)) {
    errors <- c(errors, "Equation objects must be represented as named lists")
    return(finalize_ast_validation(errors, warnings, info, context, stop_on_error))
  }

  field_check <- check_named_fields(
    x,
    required_fields = c("name", "lhs", "rhs", "relation"),
    optional_fields = c("desc", "dims", "domain", "comment", "dims_index_aliases"),
    context = context,
    detailed_report = detailed_report
  )
  errors <- c(errors, field_check$errors)
  info <- c(info, field_check$info)

  name_value <- x[["name"]]
  if (is.null(name_value) || !is.character(name_value) || length(name_value) != 1 || !nzchar(name_value)) {
    errors <- c(errors, "Equation must have a single non-empty name")
  }

  desc_value <- x[["desc"]]
  if (!is.null(desc_value) && (!is.character(desc_value) || length(desc_value) != 1)) {
    warnings <- c(warnings, "Equation desc should be a single character value when provided")
  }

  relation_value <- x[["relation"]]
  if (is.null(relation_value) || !relation_value %in% c("==", "<=", ">=")) {
    errors <- c(errors, "Equation relation must be '==', '<=', or '>='")
  }

  lhs_value <- x[["lhs"]]
  if (is.null(lhs_value) || !inherits(lhs_value, "ast")) {
    errors <- c(errors, "Equation lhs must be an AST node")
  }
  rhs_value <- x[["rhs"]]
  if (is.null(rhs_value) || !inherits(rhs_value, "ast")) {
    errors <- c(errors, "Equation rhs must be an AST node")
  }

  dims_value <- x[["dims"]]
  if (inherits(x, "multimod") && is.null(dims_value)) {
    errors <- c(errors, "Equation dims slot is NULL; expected ast_dims output")
  } else if (!is.null(dims_value)) {
    if (!inherits(dims_value, "dims")) {
      errors <- c(errors, "Equation dims must inherit class 'dims'")
    } else if (recursive) {
      dims_result <- validate(dims_value, context = paste0(context, "::dims"), stop_on_error = FALSE, recursive = recursive)
      errors <- c(errors, dims_result$errors)
      warnings <- c(warnings, dims_result$warnings)
    }
  }

  domain_value <- x[["domain"]]
  if (!is.null(domain_value) && !inherits(domain_value, "mapping") && !inherits(domain_value, "ast")) {
    warnings <- c(warnings, "Equation domain should be NULL, a mapping, or an AST node")
  }

  if (!is.null(x[["dims_index_aliases"]])) {
    alias_check <- validate_alias_vector(x[["dims_index_aliases"]], context)
    errors <- c(errors, alias_check$errors)
    warnings <- c(warnings, alias_check$warnings)
  }

  finalize_ast_validation(errors, warnings, info, context, stop_on_error)
}


#' @describeIn validate Validate a multimod model
#'
#' Checks a multimod model for structural issues, naming conflicts, and other
#' potential problems that could cause issues during export or solving.
#'
#' @param x A multimod model object
#' @param verbose Logical; if TRUE, print detailed progress messages for all checks.
#'   Default is FALSE.
#' @param stop_on_error Logical; if TRUE, stop with an error if validation fails.
#'   Default is TRUE.
#' @param ... Additional arguments (currently unused)
#'
#' @return A list with components:
#'   \item{valid}{Logical indicating if model passed all checks}
#'   \item{errors}{Character vector of error messages (critical issues)}
#'   \item{warnings}{Character vector of warning messages (potential issues)}
#'   \item{info}{Character vector of informational messages}
#'
#' @details
#' Current checks include:
#' \itemize{
#'   \item Presence of objective function
#'   \item Collision detection between set aliases and iterator variable names
#'   \item Verification that base sets referenced by aliases exist
#'   \item Detection of potential ambiguous naming
#' }
#'
#' @examples
#' \dontrun{
#' m <- read_gmpl("model.mod", data_file = "data.dat")
#' validation <- validate(m)
#' if (!validation$valid) {
#'   cat("Errors found:\n")
#'   cat(paste(validation$errors, collapse = "\n"))
#' }
#' }
#'
#' @export
validate.model <- function(x, verbose = FALSE, stop_on_error = TRUE, ...) {
  errors <- character(0)
  warnings <- character(0)
  info <- character(0)

  merge_alias_vectors <- function(existing, global_aliases) {
    if (is.null(global_aliases) || length(global_aliases) == 0) {
      return(existing)
    }
    merged <- existing
    if (is.null(merged)) {
      merged <- character(0)
    }
    for (i in seq_along(global_aliases)) {
      set_name <- names(global_aliases)[i]
      alias_value <- global_aliases[[i]]
      merged <- register_index_alias(merged, set_name, alias_value)
    }
    merged
  }

  inject_global_aliases <- function(items, global_aliases) {
    if (is.null(items) || length(items) == 0) {
      return(items)
    }
    for (idx in seq_along(items)) {
      item <- items[[idx]]
      item$dims_index_aliases <- merge_alias_vectors(item$dims_index_aliases, global_aliases)
      items[[idx]] <- item
    }
    items
  }

  apply_global_alias_scope <- function(model) {
    if (is.null(model$index_aliases) || length(model$index_aliases) == 0) {
      return(model)
    }
    global_aliases <- model$index_aliases
    model$parameters <- inject_global_aliases(model$parameters, global_aliases)
    model$variables <- inject_global_aliases(model$variables, global_aliases)
    model$equations <- inject_global_aliases(model$equations, global_aliases)
    model
  }

  model <- apply_global_alias_scope(x)

  if (verbose) cat("Validating model structure...\n")

  # Check for objective function
  if (verbose) cat("  Checking objective function...\n")
  if (is.null(model$objectives) || length(model$objectives) == 0) {
    errors <- c(errors, "Model has no objective function defined")
  } else {
    # Check each objective has valid sense
    for (i in seq_along(model$objectives)) {
      obj <- model$objectives[[i]]
      if (is.null(obj$sense)) {
        errors <- c(errors, sprintf("Objective %d is missing 'sense' field", i))
      } else if (!obj$sense %in% c("minimize", "maximize")) {
        errors <- c(errors, sprintf(
          "Objective %d has invalid sense '%s' (must be 'minimize' or 'maximize')",
          i, obj$sense
        ))
      }
    }
    # Report first objective if there are multiple
    first_obj <- model$objectives[[1]]
    info <- c(info, sprintf(
      "Objective function: %s %s",
      first_obj$sense,
      first_obj$variable
    ))
  }

  # Validate index_aliases
  if (verbose) cat("  Validating index_aliases...\n")
  index_alias_validation <- validate_index_aliases(model, verbose = verbose)
  errors <- c(errors, index_alias_validation$errors)
  warnings <- c(warnings, index_alias_validation$warnings)
  info <- c(info, index_alias_validation$info)

  component_results <- collect_component_validation(model, verbose = verbose)
  errors <- c(errors, flatten_validation_messages(component_results, "errors"))
  warnings <- c(warnings, flatten_validation_messages(component_results, "warnings"))
  info <- c(info, flatten_validation_messages(component_results, "info"))

  # Symbol inspection (undefined names, casing issues)
  symbol_info <- collect_model_symbols(model)
  symbol_scan <- inspect_model_symbols(model, symbol_info = symbol_info, fix = FALSE)
  symbol_messages <- summarize_symbol_issues(symbol_info, symbol_scan$issues)
  errors <- c(errors, symbol_messages$errors)
  warnings <- c(warnings, symbol_messages$warnings)

  if (verbose) cat("  Validating symbol identifiers...
")
  symbol_name_validation <- validate_model_symbol_names(model)
  errors <- c(errors, symbol_name_validation$errors)
  warnings <- c(warnings, symbol_name_validation$warnings)

  # Additional checks can be added here

  # Determine if model is valid (no errors)
  valid <- length(errors) == 0

  if (verbose) {
    cat("\nValidation summary:\n")
    cat(sprintf("  Valid: %s\n", valid))
    cat(sprintf("  Errors: %d\n", length(errors)))
    cat(sprintf("  Warnings: %d\n", length(warnings)))
    cat(sprintf("  Info: %d\n", length(info)))
  }

  # Stop if requested and validation failed
  if (!valid && stop_on_error) {
    error_msg <- paste(c(
      "Model validation failed:",
      paste("  -", errors)
    ), collapse = "\n")
    stop(error_msg, call. = FALSE)
  }

  structure(
    list(
      valid = valid,
      errors = errors,
      warnings = warnings,
      info = info,
      details = component_results
    ),
    class = "model_validation"
  )
}

#' Print method for model validation results
#'
#' @param x A model_validation object
#' @param ... Additional arguments (currently unused)
#'
#' @export
print.model_validation <- function(x, ...) {
  cat("Model Validation Results\n")
  cat("========================\n\n")

  if (x$valid) {
    cat("Status: VALID \u2713\n\n")
  } else {
    cat("Status: INVALID \u2717\n\n")
  }

  if (length(x$errors) > 0) {
    cat("ERRORS:\n")
    for (err in x$errors) {
      cat("  \u2717", err, "\n")
    }
    cat("\n")
  }

  if (length(x$warnings) > 0) {
    cat("WARNINGS:\n")
    for (warn in x$warnings) {
      cat("  \u26a0", warn, "\n")
    }
    cat("\n")
  }

  if (length(x$info) > 0) {
    cat("INFO:\n")
    for (inf in x$info) {
      cat("  \u2139", inf, "\n")
    }
    cat("\n")
  }

  invisible(x)
}

#' Validate index_aliases for a model
#'
#' Checks that index_aliases are properly defined and don't conflict with
#' set names, aliases, or other symbols in the model.
#'
#' @param x A multimod model object
#' @param verbose Logical; if TRUE, print detailed messages
#'
#' @return A list with components: errors, warnings, info
#'
#' @export
validate_index_aliases <- function(x, verbose = FALSE) {
  errors <- character(0)
  warnings <- character(0)
  info <- character(0)

  if (is.null(x$index_aliases) || length(x$index_aliases) == 0) {
    warnings <- c(warnings, "Model has no index_aliases defined")
    return(list(errors = errors, warnings = warnings, info = info))
  }

  # Check that index_aliases is a named character vector
  if (!is.character(x$index_aliases) || is.null(names(x$index_aliases))) {
    errors <- c(errors, "index_aliases must be a named character vector")
    return(list(errors = errors, warnings = warnings, info = info))
  }

  # Get all set names (including aliases)
  all_set_names <- character(0)
  if (!is.null(x$sets)) {
    all_set_names <- c(all_set_names, names(x$sets))
  }
  if (!is.null(x$aliases)) {
    for (alias_group in x$aliases) {
      all_set_names <- c(all_set_names, alias_group)
    }
  }
  all_set_names <- unique(all_set_names)

  # Check that all sets and aliases have index_aliases
  # Skip check for sets whose names are already short index alias values
  index_alias_values <- unname(x$index_aliases)
  missing_aliases <- setdiff(all_set_names, c(names(x$index_aliases), index_alias_values))
  if (length(missing_aliases) > 0) {
    warnings <- c(warnings, sprintf(
      "Missing index_aliases for: %s",
      paste(missing_aliases, collapse = ", ")
    ))
  }

  # Check that index_aliases don't have extra entries
  extra_aliases <- setdiff(names(x$index_aliases), all_set_names)
  if (length(extra_aliases) > 0) {
    warnings <- c(warnings, sprintf(
      "index_aliases defined for non-existent sets: %s",
      paste(extra_aliases, collapse = ", ")
    ))
  }

  # Check for duplicate iterator variable names
  iterator_vars <- unname(x$index_aliases)
  if (any(duplicated(iterator_vars))) {
    dup_vars <- unique(iterator_vars[duplicated(iterator_vars)])
    errors <- c(errors, sprintf(
      "Duplicate iterator variable(s): %s",
      paste(dup_vars, collapse = ", ")
    ))
  }

  # Check for collisions between iterator vars and set names/aliases
  # Exclude valid cases where the set name IS its own index alias
  # (e.g., REGION->r, and 'r' is also a set alias)
  iterator_vars <- unname(x$index_aliases)
  set_names_not_aliases <- setdiff(all_set_names, iterator_vars)
  collisions <- intersect(set_names_not_aliases, iterator_vars)
  if (length(collisions) > 0) {
    warnings <- c(warnings, sprintf(
      "Iterator variable(s) collide with set/alias names: %s",
      paste(collisions, collapse = ", ")
    ))
  }

  # Check for collisions with parameter, variable, equation names
  all_symbols <- c(
    names(x$parameters),
    names(x$variables),
    names(x$equations)
  )
  symbol_collisions <- intersect(all_symbols, iterator_vars)
  if (length(symbol_collisions) > 0) {
    warnings <- c(warnings, sprintf(
      "Iterator variable(s) collide with symbol names: %s",
      paste(symbol_collisions, collapse = ", ")
    ))
  }

  # Note: We do NOT check that aliases in the same group map to the same iterator
  # because aliases often need different iterators in different contexts
  # (e.g., src and dst are both aliases of region but need different iterator vars)

  # Report summary
  info <- c(info, sprintf(
    "Found %d index_aliases covering %d sets/aliases",
    length(x$index_aliases),
    length(intersect(names(x$index_aliases), all_set_names))
  ))

  list(errors = errors, warnings = warnings, info = info)
}

validate_model_symbol_names <- function(model) {
  errors <- character(0)
  warnings <- character(0)

  describe_value <- function(value) {
    if (is.null(value)) {
      return("<NULL>")
    }
    if (length(value) == 0) {
      return("<empty>")
    }
    parts <- ifelse(is.na(value), "NA", value)
    if (length(parts) == 1) {
      return(parts)
    }
    paste0("[", paste(parts, collapse = ", "), "]")
  }

  ensure_scalar_name <- function(value, owner_context) {
    if (is.null(value)) {
      return()
    }
    if (!is.character(value) || length(value) != 1 || !nzchar(value)) {
      errors <<- c(errors, sprintf(
        "%s references invalid symbol identifier; expected single non-empty string, got %s",
        owner_context,
        describe_value(value)
      ))
    }
  }

  extract_symbol_identifier <- function(node) {
    if (is.null(node) || !inherits(node, "ast")) {
      return(NULL)
    }
    if (!is.null(node$name)) {
      return(node$name)
    }
    if (!is.null(node$symbol)) {
      return(node$symbol)
    }
    NULL
  }

  inspect_dims <- function(dims_obj, owner_context, field_label) {
    if (is.null(dims_obj) || !inherits(dims_obj, "dims") || length(dims_obj) == 0) {
      return()
    }
    for (i in seq_along(dims_obj)) {
      entry <- dims_obj[[i]]
      symbol_value <- extract_symbol_identifier(entry)
      if (!is.null(symbol_value)) {
        entry_context <- sprintf("%s %s[[%d]]", owner_context, field_label, i)
        ensure_scalar_name(symbol_value, entry_context)
      }
    }
  }

  get_field_value <- function(item, field_path) {
    value <- item
    for (part in strsplit(field_path, "\\.", fixed = FALSE)[[1]]) {
      if (is.null(value)) {
        return(NULL)
      }
      value <- value[[part]]
    }
    value
  }

  inspect_component_list <- function(items, type_label, dim_fields = c("dims")) {
    if (is.null(items) || length(items) == 0) {
      return()
    }
    item_names <- names(items)
    for (idx in seq_along(items)) {
      item <- items[[idx]]
      if (is.null(item)) {
        next
      }
      display_name <- coalesce_scalar(
        if (!is.null(item_names)) item_names[[idx]] else NULL,
        coalesce_scalar(item$name, sprintf("[[%d]]", idx))
      )
      owner_context <- sprintf("%s '%s'", type_label, display_name)
      for (field in dim_fields) {
        dims_obj <- get_field_value(item, field)
        inspect_dims(dims_obj, owner_context, field)
      }
    }
  }

  inspect_component_list(model$mappings, "mapping", dim_fields = c("dims", "active_dims"))
  inspect_component_list(model$parameters, "parameter", dim_fields = c("dims", "active_dims"))
  inspect_component_list(model$variables, "variable", dim_fields = "dims")
  inspect_component_list(model$equations, "equation", dim_fields = c("dims"))

  # Equations can also embed domain dims and iterator domains
  if (!is.null(model$equations) && length(model$equations) > 0) {
    item_names <- names(model$equations)
    for (idx in seq_along(model$equations)) {
      eq <- model$equations[[idx]]
      if (is.null(eq)) next
      display_name <- coalesce_scalar(
        if (!is.null(item_names)) item_names[[idx]] else NULL,
        coalesce_scalar(eq$name, sprintf("[[%d]]", idx))
      )
      owner_context <- sprintf("equation '%s'", display_name)
      if (!is.null(eq$domain) && !is.null(eq$domain$dims)) {
        inspect_dims(eq$domain$dims, owner_context, "domain$dims")
      }
    }
  }

  list(errors = errors, warnings = warnings)
}

validation_skip_result <- function(context, message) {
  list(
    valid = NA,
    errors = character(),
    warnings = character(),
    info = message,
    context = context
  )
}

resolve_component_context <- function(component, item_name, item) {
  item_label <- "<unnamed>"
  if (!is.null(item) && !is.null(item$name)) {
    item_label <- coalesce_scalar(item$name, item_label)
  }
  item_label <- coalesce_scalar(item_name, item_label)
  sprintf("%s '%s'", component, item_label)
}

collect_component_validation <- function(model, verbose = FALSE) {
  component_lists <- list(
    sets = model$sets,
    mappings = model$mappings,
    parameters = model$parameters,
    variables = model$variables,
    equations = model$equations
  )

  results <- list()

  for (comp_name in names(component_lists)) {
    comp_items <- component_lists[[comp_name]]
    if (is.null(comp_items) || length(comp_items) == 0) {
      next
    }

    if (verbose) {
      cat(sprintf("  Validating %s (%d item%s) ...\n", comp_name, length(comp_items), if (length(comp_items) == 1) "" else "s"))
    }

    comp_results <- list()
    comp_item_names <- names(comp_items)
    for (idx in seq_along(comp_items)) {
      item <- comp_items[[idx]]
      item_name <- if (!is.null(comp_item_names)) comp_item_names[[idx]] else NULL
      ctx <- resolve_component_context(comp_name, item_name, item)
      storage_name <- coalesce_scalar(item_name, sprintf("[[%d]]", idx))

      if (is.null(item)) {
        comp_results[[storage_name]] <- validation_skip_result(ctx, sprintf("Skipping %s; object is NULL", ctx))
        next
      }

      if (!inherits(item, "ast") && !inherits(item, "multimod")) {
        comp_results[[storage_name]] <- validation_skip_result(ctx, sprintf("Skipping %s; expected object inheriting 'ast' or 'multimod'", ctx))
        next
      }

      result <- tryCatch(
        validate(item, context = ctx, stop_on_error = FALSE),
        error = function(e) list(
          valid = FALSE,
          errors = c(e$message),
          warnings = character(),
          info = character(),
          context = ctx
        )
      )

      if (length(result$errors) == 0) {
        next
      }

      comp_results[[storage_name]] <- result
    }

    if (length(comp_results) > 0) {
      results[[comp_name]] <- comp_results
    }
  }

  results
}

