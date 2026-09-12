#' @title Convert multimod object to Julia/JuMP syntax
#' @description Render a `multimod` object as a JuMP code string.
#' @param x A `multimod` object.
#' @param ... Additional arguments (not used).
#' @returns A character string with valid Julia/JuMP syntax.
#' @export
as_jump <- function(x, ...) {
  UseMethod("as_jump", x)
}

#' @export
as_jump.default <- function(x, ...) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x) && length(x) == 1) {
    return(format(x, scientific = FALSE))
  }
  stop("No as_jump method for object of class: ", class(x))
}

.jump_context_values <- c("model", "declaration", "data")

.resolve_jump_context <- function(dots, default = "model") {
  ctx <- dots$context
  if (is.null(ctx)) {
    ctx <- default
  }
  match.arg(ctx, .jump_context_values)
}

.julia_escape_string <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x, fixed = TRUE)
  gsub("\"", "\\\\\"", x, fixed = TRUE)
}

.julia_quote <- function(x) {
  sprintf('"%s"', .julia_escape_string(x))
}

.format_julia_scalar <- function(value, symbolic = FALSE) {
  if (is.na(value)) {
    return("missing")
  }
  if (symbolic || is.character(value)) {
    return(.julia_quote(as.character(value)))
  }
  if (is.logical(value)) {
    return(tolower(as.character(value)))
  }
  if (is.numeric(value)) {
    return(format(value, scientific = FALSE, trim = TRUE))
  }
  if (inherits(value, "Date")) {
    return(.julia_quote(as.character(value)))
  }
  .julia_quote(as.character(value))
}

.dim_name <- function(dim) {
  if (is.null(dim)) return(NULL)
  if (is.character(dim)) {
    return(dim)
  }
  if (!is.null(dim$name)) {
    return(dim$name)
  }
  if (!is.null(dim$label)) {
    return(dim$label)
  }
  as.character(dim)
}

.collect_dim_names <- function(dims) {
  if (is.null(dims) || length(dims) == 0) {
    return(character(0))
  }
  vapply(dims, .dim_name, character(1))
}

.format_julia_key <- function(values) {
  if (length(values) == 1) {
    return(values[[1]])
  }
  paste0("(", paste(values, collapse = ", "), ")")
}

.format_key_components <- function(values) {
  lapply(values, function(v) .format_julia_scalar(v, symbolic = FALSE))
}

.mapping_data_literal <- function(mapping) {
  data <- mapping$data
  if (is.null(data) || nrow(data) == 0) {
    return("Set([])")
  }
  dim_names <- .collect_dim_names(mapping$dims)
  if (length(dim_names) == 0) {
    return("Set([])")
  }
  missing_cols <- setdiff(dim_names, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Mapping data missing columns: ", paste(missing_cols, collapse = ", "))
  }
  key_strings <- vapply(seq_len(nrow(data)), function(idx) {
    row_values <- .format_key_components(as.list(data[idx, dim_names, drop = TRUE]))
    .format_julia_key(row_values)
  }, character(1))
  key_strings <- unique(key_strings)
  sprintf("Set([%s])", paste(key_strings, collapse = ", "))
}

.set_data_literal <- function(x) {
  members <- character(0)
  if (!is.null(x$members)) {
    members <- unique(x$members)
  }
  if (!is.null(x$data)) {
    cols <- colnames(x$data)
    if (length(cols) > 0) {
      members <- unique(c(members, as.character(unlist(x$data[cols[1]], use.names = FALSE))))
    }
  }
  members <- members[!is.na(members) & nzchar(members)]
  if (length(members) == 0) {
    return("String[]")
  }
  values <- vapply(members, .julia_quote, character(1))
  paste0("[", paste(values, collapse = ", "), "]")
}

.parameter_data_literal <- function(param, symbolic = FALSE) {
  dims <- .collect_dim_names(param$dims)
  data <- param$data
  if (!is.null(param$formula)) {
    return(NULL)
  }
  if (is.null(data) || nrow(data) == 0) {
    if (length(dims) == 0) {
      default_val <- if (!is.null(param$defVal)) param$defVal else 0
      return(.format_julia_scalar(default_val, symbolic = symbolic))
    }
    return("Dict()")
  }
  if (!"value" %in% colnames(data)) {
    stop("Parameter data frame must include a 'value' column for ", param$name)
  }
  missing_cols <- setdiff(dims, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Parameter data missing columns: ", paste(missing_cols, collapse = ", "))
  }
  if (length(dims) == 0) {
    return(.format_julia_scalar(data$value[[1]], symbolic = symbolic))
  }
  entry_strings <- vapply(seq_len(nrow(data)), function(idx) {
    row_vals <- .format_key_components(as.list(data[idx, dims, drop = TRUE]))
    key <- .format_julia_key(row_vals)
    value <- .format_julia_scalar(data$value[[idx]], symbolic = symbolic)
    paste0(key, " => ", value)
  }, character(1))
  entry_strings <- unique(entry_strings)
  sprintf("Dict(%s)", paste(entry_strings, collapse = ", "))
}

#' @export
#' @method as_jump character
as_jump.character <- function(x, ...) x

#' @export
#' @method as_jump set
as_jump.set <- function(x, declaration = FALSE, desc = declaration, model = NULL, ...) {
  stopifnot(inherits(x, "set"))
  dots <- list(...)
  default_context <- if (declaration) "declaration" else "model"
  context <- .resolve_jump_context(dots, default_context)
  if (context == "declaration") {
    paste0("setdef = [", x$name, "]  #", ifelse(desc, x$desc, ""))
  } else if (context == "data") {
    .set_data_literal(x)
  } else {
    x$name
  }
}

#' @export
#' @method as_jump dims
as_jump.dims <- function(x, model = NULL, var_names = NULL, use_index_aliases = TRUE, ...) {
  dim_names <- sapply(x, function(d) {
    set_name <- if (inherits(d, "symbol")) d$name 
    else if (inherits(d, "set")) d$name
    else if (inherits(d, "character")) d
    else return(as_jump(d, model = model, var_names = var_names, use_index_aliases = use_index_aliases, ...))
    
    # First check var_names mapping (for domain expressions in constraints)
    if (!is.null(var_names) && set_name %in% names(var_names)) {
      return(var_names[[set_name]])
    }
    
    # Check if set_name is an iterator variable (appears as a VALUE in var_names)
    # If so, it's already the correct iterator name, return it unchanged
    if (!is.null(var_names) && set_name %in% var_names) {
      return(set_name)
    }
    
    # Use index alias if available and not disabled
    if (use_index_aliases && !is.null(model) && !is.null(model$index_aliases) && set_name %in% names(model$index_aliases)) {
      return(model$index_aliases[[set_name]])
    }
    set_name
  })
  paste(dim_names, collapse = ", ")
}

#' @export
#' @method as_jump mapping
as_jump.mapping <- function(x, declaration = FALSE, desc = declaration, model = NULL, ...) {
  stopifnot(inherits(x, "mapping"))
  dots <- list(...)
  default_context <- if (declaration) "declaration" else "model"
  context <- .resolve_jump_context(dots, default_context)
  if (context == "declaration") {
    paste0("setdef = [", as.character(x), "]  #", ifelse(desc, x$desc, ""))
  } else if (context == "data") {
    .mapping_data_literal(x)
  } else {
    # If mapping has dimensions, convert to membership test
    if (!is.null(x$dims) && length(x$dims) > 0) {
      subscripts <- as_jump(x$dims, model = model, ...)
      paste0("(", subscripts, ") in ", x$name)
    } else {
      # Just return the name - let context handle subscripts
      x$name
    }
  }
}

# Get appropriate default value for a parameter in sparse data context
# @param param_name Name of the parameter
# @return Default value as string
# get_parameter_default <- function(param_name) {
#   # Parameter-specific defaults based on semantic meaning
#   # Using 0.0 for all parameters causes infeasibility!
#   defaults <- list(
#     # Factors that should default to 1.0 (100% available/active)
#     "CapacityFactor" = "1.0",
#     "AvailabilityFactor" = "1.0",
#     "CapacityToActivityUnit" = "1.0",
#     "SpecifiedDemandProfile" = "1.0",
    
#     # Ratios that should default to 0.0 (no input/output if not specified)
#     "InputActivityRatio" = "0.0",
#     "OutputActivityRatio" = "0.0",
#     "EmissionActivityRatio" = "0.0",
    
#     # Costs that should default to 0.0
#     "CapitalCost" = "0.0",
#     "FixedCost" = "0.0",
#     "VariableCost" = "0.0",
#     "EmissionsPenalty" = "0.0",
    
#     # Capacities and quantities that should default to 0.0
#     "ResidualCapacity" = "0.0",
#     "SpecifiedAnnualDemand" = "0.0",
#     "AccumulatedAnnualDemand" = "0.0",
    
#     # Lifetimes default to 1 (avoid infinite/undefined lifetimes)
#     "OperationalLife" = "1.0",
#     "OperationalLifeStorage" = "1.0",
    
#     # YearSplit: Critical! Default 0 causes division by zero
#     # Should equal 1/number of timeslices, but we don't have that info here
#     # Use 1.0 as safe default (means 100% of year if only one timeslice active)
#     "YearSplit" = "1.0",
    
#     # Storage parameters
#     "StorageLevelStart" = "0.0",
#     "MinStorageCharge" = "0.0",
#     "StorageMaxChargeRate" = "1e12",  # Effectively no limit (JuMP doesn't allow Inf in constraints)
#     "StorageMaxDischargeRate" = "1e12",  # Effectively no limit
    
#     # Days in period (should be positive)
#     "DaysInDayType" = "1.0",
#     "DaySplit" = "1.0",
#     "Conversionls" = "1.0",
#     "Conversionld" = "1.0",
#     "Conversionlh" = "1.0"
#   )
  
#   # Return specific default if defined, otherwise 0.0
#   if (param_name %in% names(defaults)) {
#     defaults[[param_name]]
#   } else {
#     "0.0"
#   }
# }

#' @export
#' @method as_jump parameter
as_jump.parameter <- function(x, declaration = FALSE, desc = declaration, model = NULL, var_names = NULL, use_index_aliases = TRUE, in_filter = FALSE, ...) {
  stopifnot(inherits(x, "parameter"))
  if (!is.null(x$dims) && length(x$dims) > 0 && !inherits(x$dims, "dims")) {
    x$dims <- normalize_symbol_dims(x$dims)
  }
  dots <- list(...)
  default_context <- if (declaration) "declaration" else "model"
  context <- .resolve_jump_context(dots, default_context)
  if (context == "declaration") {
    paste0("# Parameter: ", x$name, ifelse(desc, paste0(" — ", x$desc), ""))
  } else if (context == "data") {
    .parameter_data_literal(x, symbolic = isTRUE(x$symbolic))
  } else {
    # Include subscripts if parameter is indexed
    if (!is.null(x$dims) && length(x$dims) > 0) {
      subscripts <- as_jump(x$dims, model = model, var_names = var_names, use_index_aliases = use_index_aliases, in_filter = in_filter, ...)
      is_single_dim <- length(x$dims) == 1
      tuple_index <- if (is_single_dim) subscripts else paste0("(", subscripts, ")")
      use_haskey <- isTRUE(dots$use_haskey)
      in_sum_filter <- isTRUE(dots$in_sum_filter)
      if (use_haskey && !in_sum_filter) {
        has_data <- FALSE
        if (!is.null(model) && !is.null(model$base_path)) {
          data_file <- file.path(model$base_path, "parameters", x$name, "data.csv")
          has_data <- file.exists(data_file) && file.size(data_file) > 100
        }
        default_val <- paste0(x$name, "Def")
        if (has_data) {
          # Always use get() for sparse parameters, both in filters and constraint body
          paste0("get(", x$name, ", ", tuple_index, ", ", default_val, ")")
        } else {
          # Always use get() for sparse parameters, both in filters and constraint body
          paste0("get(", x$name, ", ", tuple_index, ", ", default_val, ")")
        }
      } else {
        default_val <- paste0(x$name, "Def")
        # Always use get() for sparse parameters, both in filters and constraint body
        paste0("get(", x$name, ", ", tuple_index, ", ", default_val, ")")
      }
    } else {
      x$name
    }
  }
}

#' @export
#' @method as_jump variable
as_jump.variable <- function(x, declaration = FALSE, desc = declaration, model = NULL, var_names = NULL, use_index_aliases = TRUE, ...) {
  stopifnot(inherits(x, "variable"))
  if (!is.null(x$dims) && length(x$dims) > 0 && !inherits(x$dims, "dims")) {
    x$dims <- normalize_symbol_dims(x$dims)
  }
  dots <- list(...)
  default_context <- if (declaration) "declaration" else "model"
  context <- .resolve_jump_context(dots, default_context)
  if (context == "declaration") {
    # Build variable declaration with bounds
    var_decl <- paste0("@variable(model, ", x$name)
    
    # Add index set if variable has domain (sparse indexing)
    if (!is.null(x$domain) && is.character(x$domain) && length(x$domain) == 1 && nzchar(x$domain)) {
      var_decl <- paste0(var_decl, "[", x$domain, "]")
    } else if (!is.null(x$dims) && length(x$dims) > 0) {
      # Cartesian product indexing
      dim_names <- sapply(x$dims, function(d) {
        if (inherits(d, "symbol")) d$name
        else if (inherits(d, "set")) d$name
        else if (is.character(d)) d
        else as_jump(d, model = model, ...)
      })
      var_decl <- paste0(var_decl, "[", paste(dim_names, collapse = ", "), "]")
    }
    
    # Add bounds (skip default -Inf/Inf bounds)
    bounds_str <- ""
    if (!is.null(x$bounds)) {
      lo <- x$bounds$lo
      up <- x$bounds$up
      
      # Skip default bounds: -Inf lower and Inf upper
      # Note: is.finite() returns FALSE for -Inf/Inf, and we only add bounds if they are finite
      has_lower <- !is.null(lo) && is.finite(lo)
      has_upper <- !is.null(up) && is.finite(up)
      
      if (has_lower && has_upper) {
        # Both bounds are non-default
        if (lo == up) {
          bounds_str <- paste0(" == ", lo)
        } else {
          bounds_str <- paste0(" >= ", lo)
          # Note: JuMP doesn't support chained bounds in declaration
          # Upper bound would need separate constraint if needed
        }
      } else if (has_lower) {
        bounds_str <- paste0(" >= ", lo)
      } else if (has_upper) {
        bounds_str <- paste0(" <= ", up)
      }
      # Otherwise no bounds_str (default -Inf to Inf)
    }
    
    # Add variable type (binary, integer)
    vtype_str <- ""
    if (!is.null(x$vtype)) {
      if (tolower(x$vtype) == "binary") {
        vtype_str <- ", Bin"
      } else if (tolower(x$vtype) == "integer") {
        vtype_str <- ", Int"
      }
    }
    
    var_decl <- paste0(var_decl, bounds_str, vtype_str, ")")
    
    if (desc && !is.null(x$desc) && nzchar(x$desc)) {
      var_decl <- paste0(var_decl, "  # ", x$desc)
    }
    
    return(var_decl)
  } else {
    # Include subscripts if variable is indexed
    if (!is.null(x$dims) && length(x$dims) > 0) {
      subscripts <- as_jump(x$dims, model = model, var_names = var_names, use_index_aliases = use_index_aliases, ...)
      
      # Check if variable uses sparse indexing (domain mapping)
      # If domain is a mapping name (character string), wrap in parentheses for tuple indexing
      # Otherwise, use comma-separated for Cartesian product
      use_tuple <- FALSE
      if (!is.null(model) && !is.null(model$variables) && x$name %in% names(model$variables)) {
        var_obj <- model$variables[[x$name]]
        # Variable uses sparse indexing if domain is a non-empty string (mapping name)
        use_tuple <- !is.null(var_obj$domain) && is.character(var_obj$domain) && 
                     length(var_obj$domain) == 1 && nzchar(var_obj$domain)
      }
      
      if (use_tuple) {
        # Sparse variable: use tuple indexing varname[(dim1, dim2, dim3)]
        paste0(x$name, "[(", subscripts, ")]")
      } else {
        # Cartesian product: use comma indexing varname[dim1, dim2, dim3]
        paste0(x$name, "[", subscripts, "]")
      }
    } else {
      x$name
    }
  }
}

#' @export
#' @method as_jump constant
as_jump.constant <- function(x, ...) {
  format(x$value, scientific = FALSE)
}

#' @export
#' @method as_jump symbol
as_jump.symbol <- function(x, model = NULL, in_filter = FALSE, use_haskey = FALSE, in_arithmetic = FALSE, var_names = NULL, ...) {
  # If in a filter with use_haskey and this is a parameter with data, generate haskey() call
  if (in_filter && use_haskey && !is.null(x$dims) && length(x$dims) > 0) {
    # Check if this is a parameter with data
    param_name <- x$name
    is_parameter <- !is.null(model) && !is.null(model$parameters) && param_name %in% names(model$parameters)
    
    if (is_parameter) {
      param_obj <- model$parameters[[param_name]]
      has_data <- FALSE
      
      # Check if parameter has data
      if (!is.null(param_obj$data) && nrow(param_obj$data) > 0) {
        has_data <- TRUE
      } else if (!is.null(model$base_path)) {
        data_file <- file.path(model$base_path, "parameters", param_name, "data.csv")
        has_data <- file.exists(data_file) && file.size(data_file) > 100
      }
      
      if (has_data) {
        # Generate haskey(param, (indices))
        index_names <- sapply(x$dims, function(d) {
          if (inherits(d, "symbol") && !is.null(d$name)) {
            d$name
          } else {
            as_jump(d, model = model, in_filter = in_filter, use_haskey = use_haskey, in_arithmetic = in_arithmetic, var_names = var_names, ...)
          }
        })
        return(paste0("haskey(", x$name, ", (", paste(index_names, collapse = ","), "))"))
      }
    }
  }
  
  # Regular symbol handling: include indexing if present
  if (!is.null(x$dims) && length(x$dims) > 0) {
    # x$dims is a list of symbol AST nodes
    index_names <- sapply(x$dims, function(d) {
      if (inherits(d, "symbol") && !is.null(d$name)) {
        d$name
      } else {
        # For non-symbol dimensions (expressions, shifts, etc.), evaluate them
        as_jump(d, model = model, in_filter = in_filter, use_haskey = use_haskey, in_arithmetic = in_arithmetic, var_names = var_names, ...)
      }
    })
    
    # Check if this symbol is a parameter (not a variable) - parameters need get() for sparse data
    # BUT: computed parameters (with formulas) should use direct indexing
    is_parameter <- !is.null(model) && !is.null(model$parameters) && x$name %in% names(model$parameters)
    is_variable <- !is.null(model) && !is.null(model$variables) && x$name %in% names(model$variables)
    
    # Check if parameter is computed (has formula) - if so, treat like variable (direct indexing)
    is_computed_param <- FALSE
    if (is_parameter && !is.null(model$parameters[[x$name]]$formula)) {
      is_computed_param <- TRUE
    }
    
    if (is_parameter && !is_variable && !is_computed_param) {
      # This is a data parameter - use get() with default to handle sparse data
      is_single_dim <- length(x$dims) == 1
      if (is_single_dim) {
        tuple_index <- index_names  # Scalar key
      } else {
        tuple_index <- paste0("(", paste(index_names, collapse = ","), ")")  # Tuple key
      }
      
      # Check if parameter has a parametric default (ast_formula) vs constant default
      param_obj <- model$parameters[[x$name]]
      has_parametric_default <- !is.null(param_obj$defVal) && inherits(param_obj$defVal, "ast_formula")
      
      if (has_parametric_default) {
        # Default is an expression like DiscountRate[r] - evaluate it inline
        # Build var_names mapping for the default expression
        default_var_names <- if (!is.null(var_names)) var_names else list()
        default_expr <- as_jump(param_obj$defVal$expr, model = model, var_names = default_var_names, ...)
        default_val <- default_expr
      } else {
        # Constant default - use the Def variable
        default_val <- paste0(x$name, "Def")
      }
      
      return(paste0("get(", x$name, ", ", tuple_index, ", ", default_val, ")"))
    } else {
      # Variable, computed parameter, or other indexed symbol - use direct indexing
      paste0(x$name, "[", paste(index_names, collapse = ","), "]")
    }
  } else {
    # Simple symbol (no indexing)
    # If used in arithmetic context and it's an iterator variable, use parse(Int, ...)
    # Check if this is an iterator variable by looking at var_names
    # Check both KEYS (for set names) and VALUES (for iterator variables like r, y)
    is_iterator <- !is.null(var_names) && (x$name %in% names(var_names) || x$name %in% var_names)
    if (in_arithmetic && is_iterator) {
      # This is an iterator variable being used in arithmetic
      # Parse as integer to get numeric value (equivalent to GAMS .val or R as.numeric)
      return(paste0("parse(Int, ", x$name, ")"))
    }
    x$name
  }
}

#' @export
#' @method as_jump shift
as_jump.shift <- function(x, model = NULL, var_names = NULL, shifts_tracker = NULL, ...) {
  stopifnot(inherits(x, "shift"))
  
  # Get the set name from the symbol using var_names (local scope) or index_aliases (global)
  symbol <- x$symbol
  offset <- x$offset
  
  # Determine the set name (check var_names first for local scope, then index_aliases)
  set_name <- NULL
  
  # First check var_names for local iterator variable mappings (e.g., in sums)
  if (!is.null(var_names) && symbol %in% names(var_names)) {
    set_name <- var_names[[symbol]]
  }
  
  # Fall back to index_aliases for global scope
  if (is.null(set_name) && !is.null(model) && !is.null(model$index_aliases)) {
    # index_aliases is a named vector: c(REGION="r", TIMESLICE="l", YEAR="y", ...)
    # Find the set name where the value matches our symbol
    matching_sets <- names(model$index_aliases)[model$index_aliases == symbol]
    if (length(matching_sets) > 0) {
      set_name <- matching_sets[1]
    }
  }
  
  # If we couldn't find the set name, use the symbol itself in uppercase
  if (is.null(set_name)) {
    set_name <- toupper(symbol)
  }
  
  # Record this shift in tracker (if provided)
  if (!is.null(shifts_tracker)) {
    key <- paste0(set_name, "_", offset)
    if (!key %in% names(shifts_tracker)) {
      shifts_tracker[[key]] <- list(set_name = set_name, offset = offset)
    }
  }
  
  # Generate shift dictionary name
  if (offset < 0) {
    dict_name <- paste0(tolower(set_name), "_lag", abs(offset))
  } else {
    dict_name <- paste0(tolower(set_name), "_lead", offset)
  }
  
  # Return dictionary lookup syntax with get() to handle missing keys
  # Use the symbol itself as fallback (for boundary conditions)
  paste0("get(", dict_name, ", ", symbol, ", ", symbol, ")")
}

#' @export
#' @method as_jump unary
as_jump.unary <- function(x, ...) {
  stopifnot(inherits(x, "unary"))
  op <- x$op
  rhs_expr <- as_jump(x$rhs, ...)
  
  if (op == "-") {
    # For negation, parentheses may be needed for complex expressions
    # But simple variables/numbers don't need them: -x, -5
    # Add parens if rhs contains operators
    if (grepl("[+*/^<>=!&|\\-]", rhs_expr)) {
      paste0("-(", rhs_expr, ")")
    } else {
      paste0("-", rhs_expr)
    }
  } else if (op == "not") {
    # Logical NOT: always wrap in parentheses for clarity and correctness
    # !(a == b) not !a == b
    paste0("!(", rhs_expr, ")")
  } else {
    stop("Unrecognized unary operator for JuMP export: ", op)
  }
}

#' @export
#' @method as_jump call
as_jump.call <- function(x, model = NULL, in_filter = FALSE, ...) {
  # Convert regular function calls (ast_call)
  # Similar logic to as_jump.func but for class "call"
  
  func_name <- x$name
  
  # Convert arguments
  args_str <- sapply(x$args, function(arg) {
    as_jump(arg, model = model, in_filter = in_filter, ...)
  })
  
  # Generate function call
  paste0(func_name, "(", paste(args_str, collapse = ", "), ")")
}

#' @export
#' @method as_jump func
as_jump.func <- function(x, model = NULL, in_filter = FALSE, ...) {
  # Special handling for sum/prod with conditional indexing
  if (tolower(x$name) %in% c("sum", "prod") && !is.null(x$index)) {
    return(as_jump_sum_prod(x, model = model, in_filter = in_filter, ...))
  }
  
  # GAMS-specific function conversions
  if (tolower(x$name) == "val") {
    # y.val -> parse(Int, string(y)) or just use ordinal value
    # For OSeMOSYS, .val is typically used on YEAR which is numeric
    # So we can convert y to its numeric value
    val <- as_jump(x$value, model = model, in_filter = in_filter, ...)
    # In most cases, the value is already numeric from the set definition
    # But if it's a string, parse it
    return(paste0("parse(Int, string(", val, "))"))
  }
  
  if (tolower(x$name) == "ord") {
    # ord(y) - ordinal position in set
    # Handle x$value being a list with one element
    val_obj <- if (is.list(x$value) && length(x$value) == 1 && inherits(x$value[[1]], "ast")) {
      x$value[[1]]
    } else if (inherits(x$value, "ast")) {
      x$value
    } else {
      stop("ord() requires a single symbol, got: ", paste(class(x$value), collapse="/"))
    }
    val <- as_jump(val_obj, model = model, in_filter = in_filter, ...)
    set_name <- resolve_alias_to_set(val, model)
    
    if (in_filter) {
      # In JuMP constraint filter: can't use anonymous functions
      # Return a marker for pattern-based replacement
      return(paste0("__ORD__", val, "__IN__", set_name, "__"))
    } else {
      # Regular Julia code: use findfirst
      return(paste0("findfirst(x -> x == ", val, ", ", set_name, ")"))
    }
  }
  
  if (tolower(x$name) == "card") {
    # card(SET) -> length(SET)
    val_obj <- if (is.list(x$value) && length(x$value) == 1 && inherits(x$value[[1]], "ast")) {
      x$value[[1]]
    } else if (inherits(x$value, "ast")) {
      x$value
    } else {
      stop("card() requires a single symbol, got: ", paste(class(x$value), collapse="/"))
    }
    val <- as_jump(val_obj, model = model, in_filter = in_filter, ...)
    set_name <- resolve_alias_to_set(val, model)
    
    if (in_filter) {
      # In filter context, return marker for pattern replacement
      return(paste0("__CARD__", set_name, "__"))
    } else {
      return(paste0("length(", set_name, ")"))
    }
  }
  
  if (tolower(x$name) %in% c("smax", "smin")) {
    # smax(iterator, expr) / smin(iterator, expr)
    # Two distinct use cases:
    # 1. Boundary check: smax(ldld, ...) where iterator is doubled version of dimension
    #    → Converts to: ld != last(DAYTYPE)
    # 2. Value computation: smax(yy, y.val) where iterator != value symbol
    #    → Converts to: parse(Int, string(last(YEAR)))
    
    if (is.null(x$index)) {
      stop("smax/smin requires an index")
    }
    
    idx_symbol <- as_jump(x$index, model = model, in_filter = in_filter, ...)
    base_set <- resolve_alias_to_set(idx_symbol, model)
    
    if (in_filter) {
      # Distinguish between boundary check and value computation
      # Check if value is val(symbol) and extract that symbol
      value_symbol <- NULL
      if (!is.null(x$value) && inherits(x$value, "func") && 
          tolower(x$value$name) == "val" && 
          !is.null(x$value$value) && inherits(x$value$value, "symbol")) {
        value_symbol <- x$value$value$name
      }
      
      # If index is doubled/repeated AND matches value symbol, it's a boundary check
      # Examples:
      #   smax(ldld, anything) → ld != last(DAYTYPE) [boundary]
      #   smax(yy, y.val) → parse(Int, string(last(YEAR))) [value computation]
      
      is_doubled <- nchar(idx_symbol) >= 4 && nchar(idx_symbol) %% 2 == 0 &&
                   substr(idx_symbol, 1, nchar(idx_symbol)/2) == substr(idx_symbol, nchar(idx_symbol)/2 + 1, nchar(idx_symbol))
      
      is_repeated_2char <- nchar(idx_symbol) == 2 && substr(idx_symbol, 1, 1) == substr(idx_symbol, 2, 2)
      
      # Extract base from iterator
      constraint_var <- if (is_doubled) {
        substr(idx_symbol, 1, nchar(idx_symbol)/2)
      } else if (is_repeated_2char) {
        substr(idx_symbol, 1, 1)
      } else {
        NULL
      }
      
      # Check if this iterator matches the value symbol
      # If iterator is yy and value is y, they match → value computation
      # If iterator is ldld and we're checking ld bounds → boundary check
      # Heuristic: 2-char iterators (yy, tt) are value computations, longer ones (ldld) are boundary checks
      if (!is.null(constraint_var) && !is.null(value_symbol) && constraint_var == value_symbol) {
        # Iterator matches value
        if (is_repeated_2char) {
          # Short 2-char iterator (yy, tt, etc.) → value computation
          if (tolower(x$name) == "smax") {
            return(paste0("parse(Int, string(last(", base_set, ")))"))
          } else {
            return(paste0("parse(Int, string(first(", base_set, ")))"))
          }
        } else {
          # Longer doubled iterator (ldld, lsls) → boundary check
          # smax(ldld, ld.val) → "can ld be incremented?" → ld != last(DAYTYPE)
          if (tolower(x$name) == "smax") {
            return(paste0(constraint_var, " != last(", base_set, ")"))
          } else {
            return(paste0(constraint_var, " != first(", base_set, ")"))
          }
        }
      } else if (!is.null(constraint_var)) {
        # Doubled iterator, no value match: fallback boundary check
        if (tolower(x$name) == "smax") {
          return(paste0(constraint_var, " != last(", base_set, ")"))
        } else {
          return(paste0(constraint_var, " != first(", base_set, ")"))
        }
      } else {
        # Fallback: value computation
        val <- as_jump(x$value, model = model, in_filter = in_filter, ...)
        if (tolower(x$name) == "smax") {
          return(paste0("parse(Int, string(last(", base_set, ")))"))
        } else {
          return(paste0("parse(Int, string(first(", base_set, ")))"))
        }
      }
    } else {
      # Not in filter: use standard aggregation
      val <- as_jump(x$value, model = model, in_filter = in_filter, ...)
      func <- if (tolower(x$name) == "smax") "maximum" else "minimum"
      return(paste0(func, "(", val, " for ", idx_symbol, " in ", base_set, ")"))
    }
  }
  
  # Generic function handling
  val <- if (inherits(x$value, "ast") || inherits(x$value, "multimod")) {
    # Single AST object
    as_jump(x$value, model = model, ...)
  } else if (is.list(x$value) && !is.data.frame(x$value)) {
    # Check if it's a list of AST objects
    if (length(x$value) > 0 && all(sapply(x$value, inherits, "ast"))) {
      # List of AST objects (multi-argument function)
      sapply(x$value, as_jump, model = model, ...)
    } else {
      stop("Unsupported value type for func '", x$name, "': list with ", length(x$value), 
           " elements, but elements are not all AST objects. ",
           "Element classes: ", paste(sapply(x$value, function(v) paste(class(v), collapse="/")), collapse=", "))
    }
  } else {
    stop("Unsupported value type for func '", x$name, "': ", paste(class(x$value), collapse=", "), 
         ". Expected ast object or list of ast objects.")
  }
  val_str <- paste(val, collapse = ", ")

  if (!is.null(x$index)) {
    idx <- as_jump(x$index, model = model, ...)
    return(paste0(x$name, "(", idx, ", ", val_str, ")"))
  } else {
    return(paste0(x$name, "(", val_str, ")"))
  }
}

#' Resolve an alias name to its base set name
#' @param alias_name Character string of the alias (e.g., "r", "y", "yy")
#' @param model Model object containing aliases
#' @return Base set name (e.g., "REGION", "YEAR") or the original name if not found
#' @keywords internal
resolve_alias_to_set <- function(alias_name, model) {
  if (is.null(model) || is.null(model$aliases)) return(alias_name)
  
  for (alias_group in model$aliases) {
    if (alias_name %in% alias_group) {
      # First element is the base set name
      return(alias_group[1])
    }
  }
  
  # Not found in aliases, return as-is (might already be a set name)
  return(alias_name)
}

#' Convert sum/prod with conditional indexing to JuMP syntax
#' @param x A func AST node representing sum or prod
#' @param ... Additional arguments passed to as_jump
#' @return JuMP sum/prod expression with proper filtering
#' @keywords internal
as_jump_sum_prod <- function(x, model = NULL, ...) {
  # Handle sum/prod with when-based conditional indexing
  # GMPL: sum{c in comm: (tech,group,c) in mTechGroupComm}(expr)
  # JuMP: sum(expr for c in COMM if (tech, group, c) in mTechGroupComm)
  
  if (!inherits(x$index, "when")) {
    # Fallback: simple index without condition
    # Need to generate proper "for var1 in SET1, var2 in SET2" syntax
    val <- as_jump(x$value, model = model, ...)
    
    # Build iterator specifications
    if (inherits(x$index, "dims")) {
      # Multiple iterators
      iter_specs <- sapply(x$index, function(d) {
        alias_name <- if (inherits(d, "symbol")) d$name 
        else if (inherits(d, "set")) d$name
        else as.character(d)
        
        # Apply index alias if available (for display)
        display_name <- alias_name
        if (!is.null(model) && !is.null(model$index_aliases) && alias_name %in% names(model$index_aliases)) {
          display_name <- model$index_aliases[[alias_name]]
        }
        
        # Resolve to base set for iteration
        base_set <- resolve_alias_to_set(alias_name, model)
        
        paste0(display_name, " in ", base_set)
      })
      
      iter_str <- paste(iter_specs, collapse = ", ")
    } else {
      # Single iterator
      alias_name <- if (inherits(x$index, "symbol")) x$index$name 
      else if (inherits(x$index, "set")) x$index$name
      else as.character(x$index)
      
      display_name <- alias_name
      if (!is.null(model) && !is.null(model$index_aliases) && alias_name %in% names(model$index_aliases)) {
        display_name <- model$index_aliases[[alias_name]]
      }
      
      base_set <- resolve_alias_to_set(alias_name, model)
      iter_str <- paste0(display_name, " in ", base_set)
    }
    
    return(paste0(x$name, "(", val, " for ", iter_str, ")"))
  }
  
  idx_when <- x$index
  iterator <- idx_when$then  # The set/symbol being iterated (e.g., comm or commp)
  condition <- idx_when$condition  # The filter condition (e.g., mTechGroupComm)
  
  # Process the value expression (which might be a when object for domain checks)
  val_expr <- as_jump(x$value, model = model, ...)
  
  # Get iterator name - can be single or tuple of iterators
  if (inherits(iterator, "symbol") || inherits(iterator, "set")) {
    # Single iterator
    iter_set_name <- iterator$name
    
    # Use short alias if available (for display)
    iter_name <- iter_set_name
    if (!is.null(model) && !is.null(model$index_aliases) && iter_set_name %in% names(model$index_aliases)) {
      iter_name <- model$index_aliases[[iter_set_name]]
    }
    
    # Resolve alias to base set for iteration
    iter_base_set <- resolve_alias_to_set(iter_set_name, model)
  } else if (inherits(iterator, "dims")) {
    # Multiple iterators as tuple: (year, timeslice) or (region, year)
    # Extract names from dims
    iter_set_names <- sapply(iterator, function(d) {
      if (inherits(d, "symbol")) d$name
      else if (inherits(d, "set")) d$name
      else as.character(d)
    })
    
    # Apply index aliases
    iter_names <- sapply(iter_set_names, function(sn) {
      if (!is.null(model) && !is.null(model$index_aliases) && sn %in% names(model$index_aliases)) {
        model$index_aliases[[sn]]
      } else {
        sn
      }
    })
    
    # Check if mapping dimensions match iterator dimensions
    condition_dims <- if (inherits(condition, "mapping") && !is.null(condition$dims)) {
      length(condition$dims)
    } else {
      0
    }
    
    if (condition_dims == length(iter_names)) {
      # Tuple dimensions match mapping dimensions: iterate directly over mapping
      # e.g., for (r, y) in mvTotalCost
      iter_name <- paste0("(", paste(iter_names, collapse = ", "), ")")
      iter_base_set <- condition$name
    } else {
      # Tuple dimensions < mapping dimensions: nested iteration with filter
      # e.g., for y in year for s in timeslice if (..., y, s) in mSupAva
      # Convert to nested for loops (handled later)
      iter_name <- NULL  # Signal that we need nested loops
      iter_base_set <- NULL
    }
  } else {
    iter_name <- as.character(iterator)
    iter_base_set <- iter_name
  }
  
  # Build the filter condition from mapping
  if (inherits(iterator, "dims") && is.null(iter_name)) {
    # Nested iteration case: tuple dimensions < mapping dimensions
    # Generate nested for loops with filter
    # e.g., for y in year for s in timeslice if (sup, comm, region, y, s) in mSupAva
    
    # Build nested for loops with aliases
    nested_loops <- paste(sapply(seq_along(iterator), function(i) {
      d <- iterator[[i]]
      d_set_name <- if (inherits(d, "symbol")) d$name else if (inherits(d, "set")) d$name else as.character(d)
      
      # Use short alias if available (for display)
      d_name <- d_set_name
      if (!is.null(model) && !is.null(model$index_aliases) && d_set_name %in% names(model$index_aliases)) {
        d_name <- model$index_aliases[[d_set_name]]
      }
      
      # Resolve alias to base set
      base_set <- resolve_alias_to_set(d_set_name, model)
      
      paste0("for ", d_name, " in ", base_set)
    }), collapse = " ")
    
    # Build filter with all dimensions from mapping (using aliases)
    if (inherits(condition, "mapping") && !is.null(condition$dims) && length(condition$dims) > 0) {
      dim_names <- sapply(condition$dims, function(d) {
        set_name <- if (inherits(d, "symbol")) d$name 
        else if (inherits(d, "set")) d$name
        else return(as.character(d))
        
        # Use alias if available
        if (!is.null(model) && !is.null(model$index_aliases) && set_name %in% names(model$index_aliases)) {
          return(model$index_aliases[[set_name]])
        }
        set_name
      })
      tuple_str <- paste0("(", paste(dim_names, collapse = ", "), ")")
      filter_str <- paste0(" if ", tuple_str, " in ", condition$name)
    } else {
      filter_str <- ""
    }
    
    return(paste0(x$name, "(", val_expr, " ", nested_loops, filter_str, ")"))
    
  } else if (inherits(iterator, "dims")) {
    # For tuple iterators matching mapping dimensions, no separate filter needed
    filter_str <- ""
  } else if (inherits(condition, "mapping") && !is.null(condition$dims) && length(condition$dims) > 0) {
    # Single iterator with mapping condition: add filter (using aliases)
    dim_names <- sapply(condition$dims, function(d) {
      set_name <- if (inherits(d, "symbol")) d$name 
      else if (inherits(d, "set")) d$name
      else return(as.character(d))
      
      # Use alias if available
      if (!is.null(model) && !is.null(model$index_aliases) && set_name %in% names(model$index_aliases)) {
        return(model$index_aliases[[set_name]])
      }
      set_name
    })
    tuple_str <- paste0("(", paste(dim_names, collapse = ", "), ")")
    filter_str <- paste0(" if ", tuple_str, " in ", condition$name)
  } else if (inherits(condition, "expression")) {
    # Boolean expression filter - set in_sum_filter=TRUE to force get() for parameters
    filter_str <- paste0(" if ", as_jump(condition, model = model, in_sum_filter = TRUE, ...))
  } else {
    # No filter or simple condition
    filter_str <- ""
  }
  
  # JuMP: sum(val for iter_name in base_set if condition)
  # Use iter_name as the variable, iter_base_set as the collection
  # Add init=1 for prod() to handle empty collections
  if (x$name == "prod") {
    paste0(x$name, "(", val_expr, " for ", iter_name, " in ", iter_base_set, filter_str, "; init = 1)")
  } else {
    paste0(x$name, "(", val_expr, " for ", iter_name, " in ", iter_base_set, filter_str, ")")
  }
}

#' Convert filter expression with haskey checks for parameters (for sum filters)
#' @keywords internal
convert_filter_with_haskey <- function(expr, model, ...) {
  # In sum filters, we can't use haskey() checks directly
  # So we just use get() for all parameters (fallback to safe mode)
  # Set in_sum_filter=TRUE to force get() even if data exists
  as_jump(expr, model = model, use_haskey = FALSE, in_sum_filter = TRUE, ...)
}

#' Flatten a chain of multiplicative expressions into individual factors
#' @keywords internal
collect_product_factors <- function(node) {
  if (is.null(node)) return(list())
  if (inherits(node, "expression") && identical(node$op, "*")) {
    c(collect_product_factors(node$lhs), collect_product_factors(node$rhs))
  } else {
    list(node)
  }
}

#' Build a nested product AST from a list of factor nodes
#' @keywords internal
build_product_ast <- function(factors) {
  stopifnot(length(factors) > 0)
  result <- factors[[1]]
  if (length(factors) == 1) {
    return(result)
  }
  for (i in 2:length(factors)) {
    result <- ast_expression("*", result, factors[[i]])
  }
  result
}

#' Inject multiplicative factors into the body of a sum node
#' @keywords internal
distribute_sum_product <- function(expr_node) {
  factors <- collect_product_factors(expr_node)
  if (length(factors) == 0) return(NULL)
  sum_idx <- which(vapply(factors, function(f) inherits(f, "sum"), logical(1)))
  # Only handle the single-sum case to avoid creating multiple nested sums
  if (length(sum_idx) != 1) return(NULL)
  sum_node <- factors[[sum_idx]]
  other_factors <- factors[-sum_idx]
  if (length(other_factors) == 0) {
    return(sum_node)
  }
  new_value <- build_product_ast(c(list(sum_node$value), other_factors))
  sum_copy <- sum_node
  sum_copy$value <- new_value
  sum_copy
}

#' Extract all parameter nodes from expression tree
#' @keywords internal
extract_parameters_from_expr <- function(node) {
  params <- list()
  
  if (inherits(node, "parameter")) {
    return(list(node))
  } else if (inherits(node, "expression")) {
    if (!is.null(node$lhs)) params <- c(params, extract_parameters_from_expr(node$lhs))
    if (!is.null(node$rhs)) params <- c(params, extract_parameters_from_expr(node$rhs))
  } else if (inherits(node, "call")) {
    for (arg in node$args) {
      params <- c(params, extract_parameters_from_expr(arg))
    }
  }
  
  return(params)
}

#' @export
#' @method as_jump sum
as_jump.sum <- function(x, model = NULL, ...) {
  if (inherits(x$index, "when")) {
    # Index with condition: sum{i in set: condition}
    # JuMP: sum(value for i in set if condition)
    iter_vars <- x$index$then
    condition_expr <- x$index$condition
    
    # Extract iterator variable name(s) and set
    if (inherits(iter_vars, "symbol")) {
      iter_name <- iter_vars$name
      # Resolve alias to base set
      iter_set <- iter_name
      if (!is.null(model) && !is.null(model$aliases)) {
        for (alias_pair in model$aliases) {
          if (length(alias_pair) == 2 && alias_pair[2] == iter_name) {
            iter_set <- alias_pair[1]
            break
          }
        }
      }
    } else if (inherits(iter_vars, "set")) {
      iter_name <- iter_vars$name
      iter_set <- iter_vars$name
    } else if (inherits(iter_vars, "dims") || is.list(iter_vars)) {
      # Multiple iterators - need tuple
      iter_names <- sapply(iter_vars, function(v) if (inherits(v, c("symbol", "set"))) v$name else as.character(v))
      iter_name <- paste0("(", paste(iter_names, collapse = ", "), ")")
      iter_set <- "???"  # Not clear what set for multiple iterators
    } else {
      iter_name <- as_jump(iter_vars, model = model, ...)
      iter_set <- iter_name
    }
    
    # Condition becomes the filter - manually construct membership test
    if (inherits(condition_expr, "mapping")) {
      # Mapping: convert dims to tuple and create membership test
      if (!is.null(condition_expr$dims) && length(condition_expr$dims) > 0) {
        subscripts <- as_jump(condition_expr$dims, model = model, ...)
        # Wrap in parentheses for tuple membership test
        filter_str <- paste0(" if (", subscripts, ") in ", condition_expr$name)
      } else {
        filter_str <- ""
      }
    } else if (inherits(condition_expr, "expression")) {
      # Complex expression - recursively convert
      # Special handling for use_haskey mode in filters
      use_haskey <- isTRUE(list(...)$use_haskey)
      if (use_haskey && condition_expr$op %in% c("GT", "GE", "LT", "LE", "EQ", "NE", "AND", "OR")) {
        # Check if any operand is a parameter - if so, we need haskey checks
        condition_str <- convert_filter_with_haskey(condition_expr, model, ...)
      } else {
        # Set in_sum_filter=TRUE to force get() for parameters
        condition_str <- as_jump(condition_expr, model = model, in_sum_filter = TRUE, ...)
      }
      filter_str <- paste0(" if (", condition_str, ")")
    } else {
      filter_str <- ""
    }
    
    # Convert value expression
    val_str <- as_jump(x$value, model = model, ...)
    
    paste0("sum(", val_str, " for ", iter_name, " in ", iter_set, filter_str, ")")
  } else {
    # Simple sum without condition
    # x$index can be either:
    # 1. A dims object directly (e.g., dims with r=REGION, y=YEAR)
    # 2. A structured index with $then and $index fields
    
    if (inherits(x$index, "dims")) {
      # Direct dims object - extract iterator variables and their sets
      idx_pairs <- character(0)
      sum_var_names <- character(0)
      iter_aliases <- names(x$index)
      for (i in seq_along(x$index)) {
        alias_name <- if (!is.null(iter_aliases) && length(iter_aliases) >= i) {
          iter_aliases[[i]]
        } else {
          NULL
        }
        var_obj <- x$index[[i]]
        set_name <- NULL
        if (inherits(var_obj, c("symbol", "set"))) {
          set_name <- var_obj$name
        } else if (is.character(var_obj) && length(var_obj) == 1) {
          set_name <- var_obj
        }
        if (is.null(set_name) || !nzchar(set_name)) {
          next
        }
        if (is.null(alias_name) || !nzchar(alias_name)) {
          if (!is.null(model) && !is.null(model$index_aliases) && set_name %in% names(model$index_aliases)) {
            alias_name <- model$index_aliases[[set_name]]
          } else {
            alias_name <- tolower(substr(set_name, 1, 1))
          }
        }
        idx_pairs <- c(idx_pairs, paste(alias_name, "in", set_name))
        sum_var_names[set_name] <- alias_name
      }
      idx_txt <- paste(idx_pairs, collapse = ", ")
    } else {
      # Structured index
      idxs <- as_jump(x$index$then, model = model, ...)
      idx  <- as_jump(x$index$index, model = model, ...)
      idx_txt <- paste(idxs, collapse = ", ")
      idx_txt <- paste(idx_txt, "in", idx)
      # Build var_names mapping for single iterator case
      # Map Set Name -> Iterator Name (e.g., REGION -> r)
      sum_var_names <- character(0)
      if (is.character(idxs) && length(idxs) == 1 && is.character(idx)) {
        sum_var_names[idx] <- idxs  # Changed: was sum_var_names[idxs] <- idx (backwards)
      }
    }
    
    # Merge var_names from outer scope (passed in ...) with sum's own iterators
    # Sum iterators take precedence (shadow outer variables)
    dots <- list(...)
    outer_var_names <- if ("var_names" %in% names(dots)) dots$var_names else NULL
    merged_var_names <- c(outer_var_names, sum_var_names)  # sum_var_names shadows outer
    
    # Remove var_names from dots to avoid duplication
    dots$var_names <- NULL
    
    # Convert value expression, passing merged var_names for arithmetic context
    val <- do.call(as_jump, c(list(x$value, model = model, var_names = merged_var_names), dots))
    
    paste0("sum(", val, " for ", idx_txt, ")")
  }
}

#' @export
#' @method as_jump prod
as_jump.prod <- function(x, model = NULL, ...) {
  idxs <- as_jump(x$index$then, model = model, ...)
  idx  <- as_jump(x$index$index, model = model, ...)
  val  <- as_jump(x$value, model = model, ...)

  idx_txt <- paste(idxs, collapse = ", ")
  paste0("prod(", val, " for ", idx_txt, " in ", idx, "; init = 1)")
}

#' @export
#' @method as_jump setmin
as_jump.setmin <- function(x, model = NULL, ...) {
  # Convert GMPL min{i in SET} expr to Julia minimum(SET)
  # or minimum(expr for i in SET) if expression depends on iterator
  
  # Check if value expression references the index variable
  if (inherits(x$index, "dims") && length(x$index) > 0) {
    # Extract iterator variable names and build var_names mapping
    iter_vars <- names(x$index)
    var_names_map <- character(0)
    for (var_name in iter_vars) {
      set_expr <- x$index[[var_name]]
      set_name <- if (inherits(set_expr, "symbol")) {
        set_expr$name
      } else if (inherits(set_expr, "set")) {
        set_expr$name
      } else {
        NULL
      }
      if (!is.null(set_name) && nzchar(set_name)) {
        var_names_map[var_name] <- set_name
      }
    }
    
    # Remove conflicting parameters from dots
    dots <- list(...)
    dots$var_names <- NULL
    dots$in_arithmetic <- NULL
    
    # Special case: if value is a call to min() or max() with single argument, unwrap it
    # GMPL often has redundant min{i in SET} min(i) which should just be minimum(i for i in SET)
    value_expr <- x$value
    if (inherits(value_expr, "call") && 
        value_expr$name %in% c("min", "max") && 
        length(value_expr$args) == 1) {
      # Unwrap: use the argument directly
      value_expr <- value_expr$args[[1]]
    }
    
    # Convert value expression with var_names and in_arithmetic context
    # This ensures symbols like 'yy' get converted to parse(Int, yy)
    value_str <- do.call(as_jump, c(list(value_expr, model = model, var_names = var_names_map, in_arithmetic = TRUE), dots))
    
    # Check if value expression uses any iterator
    uses_iter <- any(sapply(iter_vars, function(v) grepl(paste0("\\b", v, "\\b"), value_str)))
    
    if (uses_iter) {
      # Value depends on iterator: minimum(expr for i in SET)
      index_specs <- sapply(names(x$index), function(var_name) {
        set_expr <- x$index[[var_name]]
        set_name <- if (inherits(set_expr, "symbol")) {
          set_expr$name
        } else {
          as_jump(set_expr, model = model, ...)
        }
        paste(var_name, "in", set_name)
      })
      idx_txt <- paste(index_specs, collapse = ", ")
      paste0("minimum(", value_str, " for ", idx_txt, ")")
    } else {
      # Value doesn't depend on iterator: just minimum(SET)
      # Extract first set name
      set_expr <- x$index[[1]]
      set_name <- if (inherits(set_expr, "symbol")) {
        set_expr$name
      } else {
        as_jump(set_expr, model = model, ...)
      }
      paste0("minimum(", set_name, ")")
    }
  } else {
    # Fallback: simple minimum(value)
    val <- as_jump(x$value, model = model, ...)
    paste0("minimum(", val, ")")
  }
}

#' @export
#' @method as_jump setmax
as_jump.setmax <- function(x, model = NULL, ...) {
  # Convert GMPL max{i in SET} expr to Julia maximum(SET)
  # or maximum(expr for i in SET) if expression depends on iterator
  
  # Check if value expression references the index variable
  if (inherits(x$index, "dims") && length(x$index) > 0) {
    # Extract iterator variable names and build var_names mapping
    iter_vars <- names(x$index)
    var_names_map <- character(0)
    for (var_name in iter_vars) {
      set_expr <- x$index[[var_name]]
      set_name <- if (inherits(set_expr, "symbol")) {
        set_expr$name
      } else if (inherits(set_expr, "set")) {
        set_expr$name
      } else {
        NULL
      }
      if (!is.null(set_name) && nzchar(set_name)) {
        var_names_map[var_name] <- set_name
      }
    }
    
    # Remove conflicting parameters from dots
    dots <- list(...)
    dots$var_names <- NULL
    dots$in_arithmetic <- NULL
    
    # Special case: if value is a call to min() or max() with single argument, unwrap it
    # GMPL often has redundant max{i in SET} max(i) which should just be maximum(i for i in SET)
    value_expr <- x$value
    if (inherits(value_expr, "call") && 
        value_expr$name %in% c("min", "max") && 
        length(value_expr$args) == 1) {
      # Unwrap: use the argument directly
      value_expr <- value_expr$args[[1]]
    }
    
    # Convert value expression with var_names and in_arithmetic context
    # This ensures symbols like 'yy' get converted to parse(Int, yy)
    value_str <- do.call(as_jump, c(list(value_expr, model = model, var_names = var_names_map, in_arithmetic = TRUE), dots))
    
    # Check if value expression uses any iterator
    uses_iter <- any(sapply(iter_vars, function(v) grepl(paste0("\\b", v, "\\b"), value_str)))
    
    if (uses_iter) {
      # Value depends on iterator: maximum(expr for i in SET)
      index_specs <- sapply(names(x$index), function(var_name) {
        set_expr <- x$index[[var_name]]
        set_name <- if (inherits(set_expr, "symbol")) {
          set_expr$name
        } else {
          as_jump(set_expr, model = model, ...)
        }
        paste(var_name, "in", set_name)
      })
      idx_txt <- paste(index_specs, collapse = ", ")
      paste0("maximum(", value_str, " for ", idx_txt, ")")
    } else {
      # Value doesn't depend on iterator: just maximum(SET)
      # Extract first set name
      set_expr <- x$index[[1]]
      set_name <- if (inherits(set_expr, "symbol")) {
        set_expr$name
      } else {
        as_jump(set_expr, model = model, ...)
      }
      paste0("maximum(", set_name, ")")
    }
  } else {
    # Fallback: simple maximum(value)
    val <- as_jump(x$value, model = model, ...)
    paste0("maximum(", val, ")")
  }
}

#' @export
#' @method as_jump when
as_jump.when <- function(x, model = NULL, var_names = NULL, ...) {
  then <- as_jump(x$then, model = model, var_names = var_names, ...)
  otherwise <- if (!is.null(x$otherwise)) as_jump(x$otherwise, model = model, var_names = var_names, ...) else "0"
  
  # Check if condition is a mapping with dimensions (membership test)
  # or a parameter with dimensions (get check)
  if (inherits(x$condition, "mapping") && !is.null(x$condition$dims) && length(x$condition$dims) > 0) {
    # Mapping membership test: (tuple) in mapping
    # Use var_names if provided (from equation context), otherwise use index aliases
    dim_names <- sapply(x$condition$dims, function(d) {
      set_name <- if (inherits(d, "symbol")) d$name 
      else if (inherits(d, "set")) d$name
      else return(as.character(d))
      
      # First check var_names mapping (for consistency with equation iterators)
      if (!is.null(var_names) && set_name %in% names(var_names)) {
        return(var_names[[set_name]])
      }
      
      # Fallback to index alias if available
      if (!is.null(model) && !is.null(model$index_aliases) && set_name %in% names(model$index_aliases)) {
        return(model$index_aliases[[set_name]])
      }
      set_name
    })
    tuple_str <- paste0("(", paste(dim_names, collapse = ", "), ")")
    cond_str <- paste0(tuple_str, " in ", x$condition$name)
  } else if (inherits(x$condition, "parameter") && !is.null(x$condition$dims) && length(x$condition$dims) > 0) {
    # Parameter check: use get() pattern instead of haskey
    # Use var_names if provided (from equation context), otherwise use index aliases
    dim_names <- sapply(x$condition$dims, function(d) {
      set_name <- if (inherits(d, "symbol")) d$name 
      else if (inherits(d, "set")) d$name
      else return(as.character(d))
      
      # First check var_names mapping (for consistency with equation iterators)
      if (!is.null(var_names) && set_name %in% names(var_names)) {
        return(var_names[[set_name]])
      }
      
      # Fallback to index alias if available
      if (!is.null(model) && !is.null(model$index_aliases) && set_name %in% names(model$index_aliases)) {
        return(model$index_aliases[[set_name]])
      }
      set_name
    })
    tuple_str <- paste0("(", paste(dim_names, collapse = ", "), ")")
    # Use membership test for parameter existence check (treating as sparse set)
    cond_str <- paste0(tuple_str, " in ", x$condition$name)
  } else {
    # Generic condition (expression, etc.)
    cond_str <- as_jump(x$condition, model = model, var_names = var_names, ...)
  }
  
  # Use if-end syntax for JuMP compatibility (works inside sum comprehensions)
  # Add newlines for readability and proper parsing
  paste0("(if ", cond_str, "\n        ", then, "\n    else\n        ", otherwise, "\n    end)")
}

#' @export
#' @method as_jump where
as_jump.where <- function(x, ...) {
  as_jump(x$content, ...)
}

#' @export
#' @method as_jump expression
as_jump.expression <- function(x, use_index_aliases = TRUE, in_filter = FALSE, in_arithmetic = FALSE, ...) {
  dots <- list(...)
  # Distribute multiplication across sums so that weighting terms stay inside comprehensions
  if (identical(x$op, "*")) {
    distributed_sum <- distribute_sum_product(x)
    if (!is.null(distributed_sum)) {
      return(do.call(as_jump, c(list(distributed_sum,
                                    use_index_aliases = use_index_aliases,
                                    in_filter = in_filter,
                                    in_arithmetic = in_arithmetic),
                                dots)))
    }
  }

  # Determine if operands are in arithmetic context
  is_arithmetic_op <- x$op %in% c("+", "-", "*", "/", "**", "^", "div", "mod")
  is_comparison_op <- x$op %in% c("GT", "LT", "GE", "LE", "EQ", "NE", "==", "!=", "<>", "=", ">", "<", ">=", "<=")
  
  # For comparison and arithmetic operators, operands should not be in filter context
  # Comparison: already part of boolean expression
  # Arithmetic: need numeric values, not boolean haskey results
  operand_in_filter <- if (x$op %in% c("GT", "LT", "GE", "LE", "EQ", "NE", "==", "!=", "<>", "=", 
                                       "+", "-", "*", "/", "**", "^", "div", "mod")) {
    FALSE
  } else {
    in_filter
  }
  
  # Pass in_arithmetic=TRUE for arithmetic operands AND comparison operands (need numeric conversion)
  operand_in_arithmetic <- is_arithmetic_op || is_comparison_op || in_arithmetic
  
  lhs <- do.call(as_jump, c(list(x$lhs, use_index_aliases = use_index_aliases,
                                 in_filter = operand_in_filter,
                                 in_arithmetic = operand_in_arithmetic), dots))
  rhs <- do.call(as_jump, c(list(x$rhs, use_index_aliases = use_index_aliases,
                                 in_filter = operand_in_filter,
                                 in_arithmetic = operand_in_arithmetic), dots))
  op <- x$op

  # Convert operators to Julia syntax
  op <- switch(op,
               "==" = "==",
               "!=" = "!=",
               "<>" = "!=",
               "and" = "&&",
               "or" = "||",
               "**" = "^",  # Exponentiation
               "GT" = ">",  # GAMS greater than
               "LT" = "<",  # GAMS less than
               "GE" = ">=", # GAMS greater or equal
               "LE" = "<=", # GAMS less or equal
               "=" = "==",  # GAMS assignment in filter context becomes comparison
               op)
  
  # Special handling for ord/card patterns in filter context
  # Replace all occurrences of markers with appropriate Julia expressions
  if (in_filter) {
    # Pattern 1: ord(x) == card(SET) -> x == last(SET)
    # Pattern 2: ord(x) = card(SET) -> x == last(SET)
    ord_card_pattern <- "__ORD__(.+)__IN__(.+)__ (?:==|=) __CARD__\\2__"
    if (grepl(ord_card_pattern, paste(lhs, op, rhs))) {
      result <- paste(lhs, op, rhs)
      result <- gsub("__ORD__(.+)__IN__(.+)__ (?:==|=) __CARD__\\2__", "\\1 == last(\\2)", result, perl = TRUE)
      return(result)
    }
    
    # Pattern 3: ord(x) > 1 -> findfirst(==(x), SET) > 1
    # Pattern 4: ord(x) = 1 -> x == first(SET)
    # Pattern 5: ord(x) < card(SET) -> findfirst(==(x), SET) < length(SET)
    
    # Replace __ORD__var__IN__SET__ with findfirst(==(var), SET)
    lhs <- gsub("__ORD__(.+)__IN__(.+)__", "findfirst(==(\\1), \\2)", lhs)
    rhs <- gsub("__ORD__(.+)__IN__(.+)__", "findfirst(==(\\1), \\2)", rhs)
    
    # Replace __CARD__SET__ with length(SET)
    lhs <- gsub("__CARD__(.+)__", "length(\\1)", lhs)
    rhs <- gsub("__CARD__(.+)__", "length(\\1)", rhs)
  }

  # Add brackets if marked or needed
  if (isTRUE(x$lhs$brackets)) lhs <- paste0("(", lhs, ")")
  if (isTRUE(x$rhs$brackets)) rhs <- paste0("(", rhs, ")")

  paste(lhs, op, rhs)
}

#' @export
#' @method as_jump equation
as_jump.equation <- function(x, model = NULL, ...) {
  eqname <- x$name
  
  # Handle indexing and domain - do this BEFORE converting LHS/RHS
  if (!is.null(x$dims) && length(x$dims) > 0) {
    # Get domain dimension names (which may include aliases like 'dst', 'src')
    # These are the names specified in the equation's domain
    if (!is.null(x$domain) && !is.null(x$domain$dims) && length(x$domain$dims) > 0) {
      domain_dim_names <- sapply(x$domain$dims, function(d) {
        if (inherits(d, "symbol")) d$name 
        else if (inherits(d, "set")) d$name
        else if (is.character(d)) d
        else as.character(d)
      })
    } else {
      # No domain - use base set names from dims
      domain_dim_names <- sapply(x$dims, function(d) {
        if (inherits(d, "symbol")) d$name 
        else if (inherits(d, "set")) d$name
        else if (is.character(d)) d
        else as.character(d)
      })
    }
    
    # Apply index_aliases to domain names to get short iterator names for constraint header
    # Prioritize equation-specific dims_index_aliases over model-wide index_aliases
    iter_var_names <- character(length(domain_dim_names))
    used_names <- character(0)
    
    # Check if equation has dims_index_aliases (equation-specific iterator names)
    if (!is.null(x$dims_index_aliases) && length(x$dims_index_aliases) > 0) {
      # Use equation-specific dims_index_aliases directly
      # dims_index_aliases is a named character vector: c(REGION="r", REGION="rr", TIMESLICE="l", ...)
      # When dimensions repeat, access by position, not by name
      for (i in seq_along(domain_dim_names)) {
        # Access by position to handle duplicate dimension names correctly
        if (i <= length(x$dims_index_aliases)) {
          iter_var_names[i] <- x$dims_index_aliases[i]
        } else {
          # Fallback: use dim_name itself
          iter_var_names[i] <- domain_dim_names[i]
        }
      }
    } else {
      # Fallback to model-wide index_aliases with duplicate handling
      for (i in seq_along(domain_dim_names)) {
        dim_name <- domain_dim_names[i]
        
        # Determine the base set for this dimension
        base_set <- NULL
        candidate_name <- NULL
        
        if (!is.null(model$index_aliases) && dim_name %in% names(model$index_aliases)) {
          # dim_name is a base set name
          base_set <- dim_name
          candidate_name <- model$index_aliases[[dim_name]]
        } else if (!is.null(model$aliases)) {
          # dim_name might be an alias - find which set it belongs to
          for (alias_group in model$aliases) {
            if (dim_name %in% alias_group) {
              base_set <- alias_group[1]  # First element is the base set name
              candidate_name <- dim_name  # Use the alias as-is initially
              break
            }
          }
        }
        
        if (is.null(base_set)) {
          stop(sprintf(
            "Equation '%s': dimension '%s' is not a known set or alias.\n  Add it to index_aliases or define as alias in GAMS.",
            eqname, dim_name
          ))
        }
        
        # Check if candidate_name is already used (duplicate dimension)
        if (candidate_name %in% used_names) {
          # Find the alias group for this base set
          alias_group <- NULL
          if (!is.null(model$aliases)) {
            for (group in model$aliases) {
              if (base_set %in% group) {
                alias_group <- group
                break
              }
            }
          }
          
          if (is.null(alias_group) || length(alias_group) < 2) {
            stop(sprintf(
              "Equation '%s': dimension '%s' appears multiple times, but no additional aliases are defined.\n  Define additional aliases in GAMS: alias(%s, %s2, %s3, ...);",
              eqname, base_set, base_set, candidate_name, candidate_name
            ))
          }
          
          # Find the first unused alias in the group (excluding base set name)
          available_aliases <- setdiff(alias_group, c(base_set, used_names))
          if (length(available_aliases) == 0) {
            stop(sprintf(
              "Equation '%s': dimension '%s' appears too many times. Available aliases: %s\n  Define more aliases in GAMS.",
              eqname, base_set, paste(alias_group, collapse=", ")
            ))
          }
          
          candidate_name <- available_aliases[1]
        }
        
        iter_var_names[i] <- candidate_name
        used_names <- c(used_names, candidate_name)
      }
    }
    
    # Build name_map for symbol renaming in equation body
    # Priority: equation-specific dims_index_aliases > model-wide index_aliases
    if (!is.null(x$dims_index_aliases) && length(x$dims_index_aliases) > 0) {
      # Use equation-specific dims_index_aliases
      name_map <- as.list(x$dims_index_aliases)
    } else if (!is.null(model$index_aliases)) {
      # Fallback to model-wide index_aliases
      name_map <- as.list(model$index_aliases)
    } else {
      name_map <- list()
    }
    
    # Add explicit mappings for each dimension to its iterator name
    # This ensures symbols in the equation body use the correct iterator variables
    for (i in seq_along(domain_dim_names)) {
      dim_name <- domain_dim_names[i]
      iter_name <- iter_var_names[i]
      
      # Add to name_map if not already present
      if (!dim_name %in% names(name_map)) {
        name_map[[dim_name]] <- iter_name
      }
    }
    
    # Rename symbols in LHS and RHS to match iterator names
    # protected_names: names that should NOT be renamed (local iterators from nested sum/prod)
    rename_symbols_in_ast <- function(node, name_map, model, protected_names = character(0)) {
      if (is.null(node)) return(node)
      
      # If node has dims, rename the symbols in dims to use iterator names
      if (!is.null(node$dims) && length(node$dims) > 0) {
        dims_obj <- normalize_symbol_dims(node$dims)
        for (i in seq_along(dims_obj)) {
          d <- dims_obj[[i]]
          if (inherits(d, "symbol")) {
            set_name <- d$name

            # Don't rename if protected (local iterator)
            if (!set_name %in% protected_names && set_name %in% names(name_map)) {
              d$name <- name_map[[set_name]]
            }
          }
          dims_obj[[i]] <- d
        }
        node$dims <- dims_obj
      }
      
      # Recursively process child nodes for ALL node types
      if (inherits(node, "expression")) {
        node$lhs <- rename_symbols_in_ast(node$lhs, name_map, model, protected_names)
        node$rhs <- rename_symbols_in_ast(node$rhs, name_map, model, protected_names)
      } else if (inherits(node, "func")) {
        # For sum/prod with when index, collect iterator variables as protected names
        local_protected <- protected_names
        if (tolower(node$name) %in% c("sum", "prod") && !is.null(node$index) && inherits(node$index, "when")) {
          # Extract iterator set names from when$then
          iter_vars <- node$index$then
          
          if (inherits(iter_vars, "symbol") || inherits(iter_vars, "set")) {
            # Single iterator: add it and all its aliases to protected list
            set_name <- iter_vars$name
            local_protected <- c(local_protected, set_name)
            # Add all aliases too
            if (!is.null(model$aliases)) {
              for (alias_group in model$aliases) {
                if (set_name %in% alias_group) {
                  local_protected <- c(local_protected, alias_group)
                  break
                }
              }
            }
            
          } else if (inherits(iter_vars, "dims") || is.list(iter_vars)) {
            # Multiple iterators: add each and their aliases
            for (v in iter_vars) {
              set_name <- if (inherits(v, "symbol")) v$name
              else if (inherits(v, "set")) v$name
              else as.character(v)
              
              local_protected <- c(local_protected, set_name)
              # Add all aliases
              if (!is.null(model$aliases)) {
                for (alias_group in model$aliases) {
                  if (set_name %in% alias_group) {
                    local_protected <- c(local_protected, alias_group)
                    break
                  }
                }
              }
            }
          }
        }
        
        # Rename in the aggregation body with protected names
        node$value <- rename_symbols_in_ast(node$value, name_map, model, local_protected)
        
        # Rename in the index/condition (these reference outer scope, use original protected list)
        if (!is.null(node$index)) {
          node$index <- rename_symbols_in_ast(node$index, name_map, model, protected_names)
        }
      } else if (inherits(node, "sum") || inherits(node, "prod")) {
        # Handle sum/prod aggregation nodes - collect iterator variables as protected names
        local_protected <- protected_names
        if (!is.null(node$index) && inherits(node$index, "when")) {
          # Extract iterator set names from when$then
          iter_vars <- node$index$then
          
          if (inherits(iter_vars, "symbol") || inherits(iter_vars, "set")) {
            # Single iterator: add it and all its aliases to protected list
            set_name <- iter_vars$name
            local_protected <- c(local_protected, set_name)
            # Add all aliases
            if (!is.null(model$aliases)) {
              for (alias_group in model$aliases) {
                if (set_name %in% alias_group) {
                  local_protected <- c(local_protected, alias_group)
                  break
                }
              }
            }
            
          } else if (inherits(iter_vars, "dims") || is.list(iter_vars)) {
            # Multiple iterators: add each and their aliases
            for (v in iter_vars) {
              set_name <- if (inherits(v, "symbol")) v$name
              else if (inherits(v, "set")) v$name
              else as.character(v)
              
              local_protected <- c(local_protected, set_name)
              # Add all aliases
              if (!is.null(model$aliases)) {
                for (alias_group in model$aliases) {
                  if (set_name %in% alias_group) {
                    local_protected <- c(local_protected, alias_group)
                    break
                  }
                }
              }
            }
          }
          
          # Rename the when node specially:
          # - then part (iterator set) uses outer scope
          # - condition part (filter) uses local_protected to preserve iterator variable
          if (!is.null(node$index$then)) {
            node$index$then <- rename_symbols_in_ast(node$index$then, name_map, model, protected_names)
          }
          if (!is.null(node$index$condition)) {
            node$index$condition <- rename_symbols_in_ast(node$index$condition, name_map, model, local_protected)
          }
        } else if (!is.null(node$index)) {
          # No when node, just rename the index normally
          node$index <- rename_symbols_in_ast(node$index, name_map, model, protected_names)
        }
        
        # Rename in the value with protected names
        if (!is.null(node$value)) {
          node$value <- rename_symbols_in_ast(node$value, name_map, model, local_protected)
        }
      } else if (inherits(node, "when")) {
        if (!is.null(node$condition)) {
          node$condition <- rename_symbols_in_ast(node$condition, name_map, model, protected_names)
        }
        if (!is.null(node$then)) {
          node$then <- rename_symbols_in_ast(node$then, name_map, model, protected_names)
        }
      }
      # Note: variable, parameter, and mapping nodes are leaf nodes with dims already processed above
      # No further recursion needed for these - dims have been renamed
      
      node
    }
    
    # Rename symbols in LHS and RHS to use iterator variables, then convert to Julia
    # Pass a list of local iterators that should NOT be renamed (from nested sum/prod)
    lhs_renamed <- rename_symbols_in_ast(x$lhs, name_map, model, protected_names = character(0))
    rhs_renamed <- rename_symbols_in_ast(x$rhs, name_map, model, protected_names = character(0))
    
    # Create var_names mapping (iterator variable -> set name) for arithmetic operations
    # This is the inverse of name_map (set name -> iterator variable)
    if (length(name_map)) {
      var_names_for_arithmetic <- unlist(name_map, use.names = TRUE)
    } else {
      var_names_for_arithmetic <- character(0)
    }
    
    lhs <- as_jump(lhs_renamed, model = model, var_names = var_names_for_arithmetic, ...)
    rhs <- as_jump(rhs_renamed, model = model, var_names = var_names_for_arithmetic, ...)
  } else {
    # No dimensions - convert normally
    lhs <- as_jump(x$lhs, model = model, ...)
    rhs <- as_jump(x$rhs, model = model, ...)
  }

  rel <- switch(x$relation,
                "==" = "==",
                "<=" = "<=",
                ">=" = ">=",
                stop("Unsupported relation: ", x$relation))

  constraint_expr <- paste(lhs, rel, rhs)
  
  # Handle indexing and domain
  if (!is.null(x$dims) && length(x$dims) > 0) {
    # iter_var_names, dim_set_names, and name_map are already computed above
    
    if (!is.null(x$domain)) {
      # Check if domain is a mapping or a boolean expression
      if (inherits(x$domain, "mapping")) {
        # Domain is a mapping: use JuMP's inline tuple unpacking
        # @constraint(model, eqName[(vars...) in mapping], ...)
        domain_name <- x$domain$name
        
        paste0(
          "@constraint(\n    model,\n    ",
          eqname, "[(", paste(iter_var_names, collapse = ", "), ") in ", domain_name, "],\n    ",
          constraint_expr,
          "\n)"
        )
      } else {
        # Domain is a boolean expression: use as filter condition
        # @constraint(model, eqName[var1 in SET1, var2 in SET2; condition], ...)
        
        # Build iterator specifications using iter_var_names and domain_dim_names
        iter_specs <- sapply(seq_along(domain_dim_names), function(i) {
          var_name <- iter_var_names[i]
          dim_name <- domain_dim_names[i]
          
          # Resolve to base set for iteration
          base_set <- resolve_alias_to_set(dim_name, model)
          
          paste0(var_name, " in ", base_set)
        })
        
        # Rename symbols in domain to match iterators (reuse function and name_map from above)
        domain_renamed <- rename_symbols_in_ast(x$domain, name_map, model, protected_names = character(0))
        
        # Create var_names mapping (iterator variable -> set name) for arithmetic operations in domain
        # This is the inverse of name_map (set name -> iterator variable)
        # This allows arithmetic expressions like "y + param[r,s] - 1" to parse y as integer
        if (length(name_map)) {
          var_names_for_domain <- unlist(name_map, use.names = TRUE)
        } else {
          var_names_for_domain <- character(0)
        }
        
        # Convert domain expression to Julia boolean condition
        # Pass var_names=var_names_for_domain to enable parse(Int, ...) for iterator variables in arithmetic
        # Set in_filter=TRUE so parameters use get() with default 0 for sparse data
        
        # IMPORTANT: Domain conditions should ALWAYS evaluate the actual condition expression,
        # not just check key existence. The use_haskey mode is ONLY for constraint body expressions
        # to handle sparse data access, but constraint filter conditions must always evaluate
        # the comparison (e.g., AvailabilityFactor[r,t,y] < 1 must check the VALUE, not just existence).
        # 
        # Using haskey() for domain conditions causes wrong constraints to be generated:
        # - Constraints are generated for keys that exist even when condition is false
        # - Constraints are NOT generated for keys that don't exist even when default would satisfy condition
        #
        # Therefore, ALWAYS use use_haskey=FALSE for domain conditions regardless of global setting
        
        # Remove use_haskey from ... to avoid duplicate argument error
        extra_args <- list(...)
        extra_args$use_haskey <- NULL
        
        domain_condition <- do.call(as_jump, c(
          list(x = domain_renamed, model = model, var_names = var_names_for_domain, 
               use_index_aliases = FALSE, in_filter = TRUE, use_haskey = FALSE),
          extra_args
        ))
        
        paste0(
          "@constraint(\n    model,\n    ",
          eqname, "[", paste(iter_specs, collapse = ", "), "; ", domain_condition, "],\n    ",
          constraint_expr,
          "\n)"
        )
      }
    } else {
      # Simple indexed constraint without domain mapping or filter
      # Build iterator specifications using iter_var_names and domain_dim_names
      iter_specs <- sapply(seq_along(domain_dim_names), function(i) {
        var_name <- iter_var_names[i]
        dim_name <- domain_dim_names[i]
        
        # Resolve to base set for iteration
        base_set <- resolve_alias_to_set(dim_name, model)
        
        paste0(var_name, " in ", base_set)
      })
      
      paste0(
        "@constraint(\n    model,\n    ",
        eqname, "[", paste(iter_specs, collapse = ", "), "],\n    ",
        constraint_expr,
        "\n)"
      )
    }
  } else {
    # Scalar constraint
    paste0(
      "@constraint(\n    model,\n    ",
      eqname, ",\n    ",
      constraint_expr,
      "\n)"
    )
  }
}


