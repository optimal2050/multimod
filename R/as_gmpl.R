
#' @title Convert multimod object to GMPL/MathProg syntax
#' @description Render a `multimod` object as a GMPL code string.
#' @param x A `multimod` object.
#' @param ... Additional arguments (not used).
#' @returns A character string with valid GMPL syntax.
#' @export
as_gmpl <- function(x, ...) {
  UseMethod("as_gmpl", x)
}

#' @export
as_gmpl.default <- function(x, ...) {
  if (is.null(x)) return(x)
  if (length(x) == 1 && is.numeric(x)) {
    return(format(x, scientific = FALSE))
  } else if (length(x) == 1 && is.character(x)) {
    return(x)
  }
  warning("No as_gmpl method for object of class: ", class(x))
  x
}

ensure_dims_object <- function(dims_value) {
  if (is.null(dims_value) || inherits(dims_value, "dims")) {
    return(dims_value)
  }
  if (inherits(dims_value, "ast")) {
    return(ast_dims(dims_value))
  }
  if (is.list(dims_value)) {
    if (length(dims_value) == 0) {
      return(ast_dims())
    }
    # Strip names to prevent malformed structures
    names(dims_value) <- NULL
    return(do.call(ast_dims, dims_value))
  }
  if (length(dims_value) > 0) {
    dims_list <- as.list(dims_value)
    # Strip names to prevent malformed structures
    names(dims_list) <- NULL
    return(do.call(ast_dims, dims_list))
  }
  ast_dims()
}

dim_binding_name <- function(entry) {
  if (is.null(entry)) return("")
  if (inherits(entry, "shift") && !is.null(entry$symbol)) {
    return(as.character(entry$symbol)[1])
  }
  if (!is.null(entry$name)) {
    return(as.character(entry$name)[1])
  }
  if (!is.null(entry$symbol)) {
    return(as.character(entry$symbol)[1])
  }
  value <- tryCatch(as.character(entry), error = function(e) character(0))
  if (length(value) == 0) {
    ""
  } else {
    value[1]
  }
}

#' @export
as_gmpl.character <- function(x, ...) {
  if (is.character(x)) {
    return(x)
  }
  UseMethod("as_gmpl", x)
}

#' @export
#' @method as_gmpl set
as_gmpl.set <- function(x, declaration = FALSE, desc = declaration, model = NULL, ...) {
  stopifnot(inherits(x, "set"))
  if (declaration) {
    comment <- if (desc && !is.null(x$desc) && length(x$desc) > 0 && !is.na(x$desc) && nchar(x$desc) > 0) {
      paste0("  # ", x$desc)
    } else {
      ""
    }
    paste0("set ", x$name, ";", comment)
  } else {
    x$name
  }
}

#' @export
#' @method as_gmpl mapping
as_gmpl.mapping <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "mapping"))
  dims_obj <- ensure_dims_object(x$dims)
  if (declaration) {
    # Get dimension count from the mapping
    n_dims <- if (!is.null(dims_obj)) length(dims_obj) else 0

    comment <- if (desc && !is.null(x$desc) && length(x$desc) > 0 && !is.na(x$desc) && nchar(x$desc) > 0) {
      paste0("  # ", x$desc)
    } else {
      ""
    }
    paste0("set ", x$name, " dimen ", n_dims, ";", comment)
  } else {
    # Check if we're in a binding condition (sum/prod iterator condition)
    in_binding <- getOption("multimod.in_binding_condition", FALSE)

    if (in_binding && !is.null(dims_obj) && length(dims_obj) > 0) {
      # Convert to membership test: (dims) in mapping
      index_aliases <- getOption("multimod.index_aliases", NULL)
      iter_vars <- getOption("multimod.iterator_vars", NULL)

      dim_list <- sapply(dims_obj, function(d) {
        set_name <- dim_binding_name(d)
        # Priority: iterator vars > dummy vars > original name
        if (!is.null(iter_vars) && set_name %in% names(iter_vars)) {
          iter_vars[[set_name]]
        } else if (!is.null(index_aliases) && set_name %in% names(index_aliases)) {
          index_aliases[[set_name]]
        } else {
          set_name
        }
      })
      paste0("(", paste(dim_list, collapse = ","), ") in ", x$name)
    } else {
      x$name
    }
  }
}

#' @export
#' @method as_gmpl dims
as_gmpl.dims <- function(x, use_index_aliases = TRUE, brackets = "[", ...) {
  if (length(x) == 0) return("")

  # Check if we're in an equation context with dummy variables
  index_aliases <- if (use_index_aliases) getOption("multimod.index_aliases", NULL) else NULL

  # Also check for iterator variables from sum/prod
  iter_vars <- getOption("multimod.iterator_vars", NULL)

  dim_names <- sapply(x, function(d) {
    # If dimension is a shift, convert it to full arithmetic expression
    if (inherits(d, "shift")) {
      return(as_gmpl(d, ...))
    }
    
    set_name <- dim_binding_name(d)
    # Priority: iterator vars > dummy vars > original name
    a <- try({(!is.null(iter_vars) && set_name %in% names(iter_vars))})
    if (inherits(a, "try-error")) browser()
    if (!is.null(iter_vars) && set_name %in% names(iter_vars)) {
      iter_vars[[set_name]]
    } else if (!is.null(index_aliases) && set_name %in% names(index_aliases)) {
      index_aliases[[set_name]]
    } else {
      set_name
    }
  })

  out <- paste(dim_names, collapse = ",")

  if (is.null(brackets) || all(brackets == "")) return(out)
  brackets <- brackets_pair(brackets)
  paste0(brackets[1], out, brackets[2])
}

#' @export
#' @method as_gmpl parameter
as_gmpl.parameter <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "parameter"))
  dims_obj <- ensure_dims_object(x$dims)
  active_dims_obj <- ensure_dims_object(x$active_dims)
  if (declaration) {
    # Use active_dims if available (for folded parameters), else full dims
    dims_to_use <- if (!is.null(active_dims_obj) && length(active_dims_obj) > 0) {
      active_dims_obj
    } else {
      dims_obj
    }

    # Format indexing with curly braces
    dims_str <- if (!is.null(dims_to_use) && inherits(dims_to_use, "dims") && length(dims_to_use) > 0) {
      # Get dimension names
      dim_names <- sapply(dims_to_use, dim_binding_name)
      paste0("{", paste(dim_names, collapse = ","), "}")
    } else {
      ""
    }
    comment <- if (desc && !is.null(x$desc) && length(x$desc) > 0 && !is.na(x$desc) && nchar(x$desc) > 0) {
      paste0("  # ", x$desc)
    } else {
      ""
    }

    # Check for computed parameter formula (stored as AST or ast_formula)
    formula_ast <- if (!is.null(x$formula) && inherits(x$formula, "ast_formula")) {
      x$formula$expr  # Extract wrapped expression
    } else {
      x$formula
    }

    if (!is.null(formula_ast) && inherits(formula_ast, "ast")) {
      # Computed parameter: param Name{r in REGION, s in STORAGE} := expression;
      # Use preserved dims_index_aliases if available (from read_gmpl), otherwise generate

      if (!is.null(dims_to_use) && inherits(dims_to_use, "dims") && length(dims_to_use) > 0) {
        # Get dimension names
        dim_names <- sapply(dims_to_use, dim_binding_name)

        # Use preserved iterator variables if available
        if (!is.null(x$dims_index_aliases) && length(x$dims_index_aliases) == length(dim_names)) {
          iter_vars <- x$dims_index_aliases
        } else {
          # Fallback: generate iterator variables (use first letter of set name)
          iter_vars <- sapply(dim_names, function(dname) {
            tolower(substr(dname, 1, 1))
          })
          names(iter_vars) <- dim_names
        }

        # Build indexing: {r in REGION, s in STORAGE, y in YEAR}
        indexing_parts <- paste(iter_vars, "in", dim_names)
        dims_str <- paste0("{", paste(indexing_parts, collapse = ", "), "}")

        # Set up iterator variable context for formula conversion
        old_iter_vars <- getOption("multimod.iterator_vars", list())
        new_iter_vars <- old_iter_vars
        for (i in seq_along(dim_names)) {
          new_iter_vars[[dim_names[i]]] <- iter_vars[i]
        }
        options(multimod.iterator_vars = new_iter_vars)
        on.exit(options(multimod.iterator_vars = old_iter_vars), add = TRUE)

        # Convert formula with iterator context
        formula_str <- as_gmpl(formula_ast, ...)
      } else {
        # Scalar computed parameter (no dimensions)
        dims_str <- ""
        formula_str <- as_gmpl(formula_ast, ...)
      }

      paste0("param ", x$name, dims_str, " :=\n\t", formula_str, ";", comment)
    } else if (!is.null(x$defVal)) {
      # Handle ast_formula in defVal
      defval_ast <- if (inherits(x$defVal, "ast_formula")) {
        x$defVal$expr  # Extract wrapped expression
      } else {
        x$defVal
      }

      # Check if defVal has actual value (not NULL, not empty, not NA for scalars)
      has_defval <- !is.null(defval_ast) &&
                    (inherits(defval_ast, "ast") ||
                     (length(defval_ast) > 0 && !all(is.na(defval_ast))))

      if (has_defval) {
      # Parameter with default value: param Name{indexing}, [symbolic] default value;
      # If default is an expression (AST or contains subscripts), need iterator variables
      has_subscript <- inherits(defval_ast, "ast") ||
                       (is.character(defval_ast) && grepl("\\[", defval_ast))

      # Determine dims_str for this parameter
      param_dims_str <- if (has_subscript && !is.null(dims_to_use) && length(dims_to_use) > 0) {
        # Need iterator variables for expression defaults
        dim_names <- sapply(dims_to_use, dim_binding_name)

        # Use preserved iterator variables if available
        if (!is.null(x$dims_index_aliases) && length(x$dims_index_aliases) == length(dim_names)) {
          iter_vars <- x$dims_index_aliases
        } else {
          # Fallback: generate iterator variables
          iter_vars <- sapply(dim_names, function(dname) {
            tolower(substr(dname, 1, 1))
          })
          names(iter_vars) <- dim_names
        }

        # Create indexing string with iterator variables
        index_specs <- paste(iter_vars, "in", names(iter_vars), collapse = ", ")
        paste0("{", index_specs, "}")
      } else {
        dims_str  # Use plain dims_str without iterator variables
      }

      symbolic_str <- if (isTRUE(x$symbolic)) "symbolic " else ""
      default_str <- if (inherits(defval_ast, "ast")) {
        # AST expression - convert to GMPL
        # Set up iterator variable context if needed
        if (has_subscript && !is.null(dims_to_use) && length(dims_to_use) > 0) {
          dim_names <- sapply(dims_to_use, dim_binding_name)

          # Use preserved iterator variables if available
          if (!is.null(x$dims_index_aliases) && length(x$dims_index_aliases) == length(dim_names)) {
            iter_vars <- x$dims_index_aliases
          } else {
            # Fallback: generate iterator variables
            iter_vars <- sapply(dim_names, function(dname) {
              tolower(substr(dname, 1, 1))
            })
            names(iter_vars) <- dim_names
          }

          # Set up iterator variable context
          old_iter_vars <- getOption("multimod.iterator_vars", list())
          new_iter_vars <- old_iter_vars
          for (i in seq_along(dim_names)) {
            new_iter_vars[[dim_names[i]]] <- iter_vars[i]
          }
          options(multimod.iterator_vars = new_iter_vars)
          on.exit(options(multimod.iterator_vars = old_iter_vars), add = TRUE)
        }

        as_gmpl(defval_ast, ...)
      } else if (is.character(defval_ast)) {
        # Only quote if symbolic (string literal), otherwise it's an expression
        if (isTRUE(x$symbolic)) {
          paste0("'", defval_ast, "'")
        } else {
          defval_ast  # Expression - no quotes
        }
      } else if (is.infinite(defval_ast)) {
        # Replace Inf with configurable value
        INF <- getOption("multimod.gmpl_inf", 1e20)
        format(INF, scientific = FALSE)
      } else {
        format(defval_ast, scientific = FALSE)
      }
      paste0("param ", x$name, param_dims_str, ", ", symbolic_str, "default ", default_str, ";", comment)
      } else {
        # Simple parameter declaration (no default value)
        paste0("param ", x$name, dims_str, ";", comment)
      }
    } else {
      # Simple parameter declaration
      paste0("param ", x$name, dims_str, ";", comment)
    }
  } else {
    # Check if we're in a binding condition
    in_binding <- getOption("multimod.in_binding_condition", FALSE)

    if (in_binding && !is.null(dims_obj) && length(dims_obj) > 0) {
      # Convert to membership test: (dims) in parameter
      index_aliases <- getOption("multimod.index_aliases", NULL)
      iter_vars <- getOption("multimod.iterator_vars", NULL)

      dim_list <- sapply(dims_obj, function(d) {
        set_name <- dim_binding_name(d)
        # Priority: iterator vars > dummy vars > original name
        if (!is.null(iter_vars) && set_name %in% names(iter_vars)) {
          iter_vars[[set_name]]
        } else if (!is.null(index_aliases) && set_name %in% names(index_aliases)) {
          index_aliases[[set_name]]
        } else {
          set_name
        }
      })
      paste0("(", paste(dim_list, collapse = ","), ") in ", x$name)
    } else {
      # In equation context, use dummy variables
      if (!is.null(dims_obj) && length(dims_obj) > 0) {
        dims_str <- as_gmpl(dims_obj, use_index_aliases = TRUE, brackets = "[", ...)
        paste0(x$name, dims_str)
      } else {
        x$name
      }
    }
  }
}

#' @export
#' @method as_gmpl variable
as_gmpl.variable <- function(x, declaration = FALSE, desc = declaration, ...) {
  stopifnot(inherits(x, "variable"))
  dims_obj <- ensure_dims_object(x$dims)
  if (declaration) {
    # Format indexing with curly braces and iterator variables (r in REGION, etc.)
    dims_str <- if (!is.null(dims_obj) && inherits(dims_obj, "dims") && length(dims_obj) > 0) {
      # Get dimension names
      dim_names <- sapply(dims_obj, dim_binding_name)

      # Use preserved iterator variables if available (from read_gmpl)
      if (!is.null(x$dims_index_aliases) && length(x$dims_index_aliases) == length(dim_names)) {
        iter_vars <- x$dims_index_aliases
      } else {
        # Generate iterator variables (use first letter of set name, ensure uniqueness)
        iter_vars <- character(length(dim_names))
        used_vars <- character(0)

        for (i in seq_along(dim_names)) {
          dname <- dim_names[i]
          base_var <- tolower(substr(dname, 1, 1))

          # Ensure uniqueness by appending numbers if needed
          candidate <- base_var
          counter <- 1
          while (candidate %in% used_vars) {
            candidate <- paste0(base_var, counter)
            counter <- counter + 1
          }

          iter_vars[i] <- candidate
          used_vars <- c(used_vars, candidate)
        }
        names(iter_vars) <- dim_names
      }

      # Build indexing: {r in REGION, s in STORAGE, y in YEAR}
      indexing_parts <- paste(iter_vars, "in", dim_names)
      paste0("{", paste(indexing_parts, collapse = ", "), "}")
    } else {
      ""
    }
    # Add bounds if specified - free by default, only write finite bounds
    bounds <- if (!is.null(x$bounds) && is.list(x$bounds)) {
      parts <- character(0)
      # Only add finite bounds (omit Inf/-Inf)
      if (!is.null(x$bounds$lo) && is.finite(x$bounds$lo)) {
        parts <- c(parts, paste0(">= ", x$bounds$lo))
      }
      if (!is.null(x$bounds$up) && is.finite(x$bounds$up)) {
        parts <- c(parts, paste0("<= ", x$bounds$up))
      }
      if (length(parts) > 0) {
        paste0(" ", paste(parts, collapse = ", "))
      } else {
        ""  # Free variable (no bounds written)
      }
    } else if (!is.null(x$vtype)) {
      # Use vtype from GAMS variable declaration
      # "positive" variables get >= 0, "binary" gets >= 0, <= 1
      vtype <- tolower(x$vtype)
      if (vtype == "positive") {
        " >= 0"
      } else if (vtype == "binary") {
        " >= 0, <= 1"
      } else {
        ""  # Free variable (no bounds)
      }
    } else if (!is.null(x$domain)) {
      # Fallback: Use domain type from GAMS variable declaration
      # "positive" variables get >= 0, unrestricted/free variables have no bounds
      domain <- tolower(x$domain)
      if (domain == "positive") {
        " >= 0"
      } else {
        ""  # Free variable (no bounds)
      }
    } else {
      ""  # Free variable by default (no bounds)
    }
    comment <- if (desc && !is.null(x$desc) && length(x$desc) > 0 && !is.na(x$desc) && nchar(x$desc) > 0) {
      paste0("  # ", x$desc)
    } else {
      ""
    }
    paste0("var ", x$name, dims_str, bounds, ";", comment)
  } else {
    # Check if we're in a binding condition
    in_binding <- getOption("multimod.in_binding_condition", FALSE)

    if (in_binding && !is.null(dims_obj) && length(dims_obj) > 0) {
      # Convert to membership test: (dims) in variable
      index_aliases <- getOption("multimod.index_aliases", NULL)
      iter_vars <- getOption("multimod.iterator_vars", NULL)

      dim_list <- sapply(dims_obj, function(d) {
        set_name <- dim_binding_name(d)
        # Priority: iterator vars > dummy vars > original name
        if (!is.null(iter_vars) && set_name %in% names(iter_vars)) {
          iter_vars[[set_name]]
        } else if (!is.null(index_aliases) && set_name %in% names(index_aliases)) {
          index_aliases[[set_name]]
        } else {
          set_name
        }
      })
      paste0("(", paste(dim_list, collapse = ","), ") in ", x$name)
    } else {
      # In equation context, use dummy variables
      if (!is.null(dims_obj) && length(dims_obj) > 0) {
        dims_str <- as_gmpl(dims_obj, use_index_aliases = TRUE, brackets = "[", ...)
        paste0(x$name, dims_str)
      } else {
        x$name
      }
    }
  }
}

#' @export
#' @method as_gmpl constant
as_gmpl.constant <- function(x, ...) {
  format(x$value, scientific = FALSE)
}

#' @export
#' @method as_gmpl symbol
as_gmpl.symbol <- function(x, ...) {
  # If symbol has dims (indexing), include them
  dims_obj <- ensure_dims_object(x$dims)
  if (!is.null(dims_obj) && length(dims_obj) > 0) {
    # x$dims is a list of symbol AST nodes
    index_names <- sapply(dims_obj, function(d) {
      if (inherits(d, "symbol") && !is.null(d$name)) {
        d$name
      } else {
        as_gmpl(d, ...)
      }
    })
    paste0(x$name, "[", paste(index_names, collapse = ","), "]")
  } else {
    x$name
  }
}

#' @export
#' @method as_gmpl shift
as_gmpl.shift <- function(x, ...) {
  # Convert shift to GMPL arithmetic: y-1, y+1, ls-1, etc.
  symbol_name <- x$symbol
  offset <- x$offset

  if (offset > 0) {
    paste0(symbol_name, "+", offset)
  } else {
    # offset is negative, so just concatenate (includes minus sign)
    paste0(symbol_name, offset)
  }
}

#' @export
#' @method as_gmpl sum
as_gmpl.sum <- function(x, ...) {
  # GMPL uses curly braces for indexing: sum{i in I: condition} expr

  if (inherits(x$value, "when") && !is.null(x$value$condition)) {
    # Check if value is a 'when' node (condition from GMPL sum{...} syntax)
    # In GMPL, conditions go in the index part, not as if-then-else
    # Extract condition and actual value
    condition <- x$value$condition
    actual_value <- x$value$then

    # Get index names and sets from x$index (dims object)
    if (inherits(x$index, "dims") && length(x$index) > 0) {
      # Build index spec: "var in SET, var2 in SET2"
      index_specs <- sapply(names(x$index), function(var_name) {
        set_expr <- x$index[[var_name]]
        set_name <- dim_binding_name(set_expr)
        paste(var_name, "in", set_name)
      })

      # Add condition
      cond_str <- as_gmpl(condition, ...)
      idx <- paste0(paste(index_specs, collapse = ", "), ": ", cond_str)
    } else {
      # Fallback
      idx <- as_gmpl(x$index, brackets = "{}", ...)
      cond_str <- as_gmpl(condition, ...)
      idx <- paste0(idx, ": ", cond_str)
    }

    val <- as_gmpl(actual_value, ...)
  } else {
    # No condition, standard sum - still need "var in SET" format
    if (inherits(x$index, "dims") && length(x$index) > 0) {
      # Build index spec: "var in SET, var2 in SET2"
      index_specs <- sapply(names(x$index), function(var_name) {
        set_expr <- x$index[[var_name]]
        set_name <- dim_binding_name(set_expr)
        paste(var_name, "in", set_name)
      })
      idx <- paste(index_specs, collapse = ", ")
    } else {
      # Fallback for unexpected structure
      idx <- as_gmpl(x$index, brackets = "", ...)
    }
    val <- as_gmpl(x$value, ...)
  }

  # If the value is an addition or subtraction (lower precedence than multiplication),
  # wrap it in parentheses to ensure correct evaluation order
  # Check both direct value and when$then (for conditional sums)
  value_expr <- x$value
  if (inherits(value_expr, "when") && inherits(value_expr$then, "expression")) {
    value_expr <- value_expr$then
  }
  if (inherits(value_expr, "expression") && value_expr$op %in% c("+", "-")) {
    val <- paste0("(", val, ")")
  }

  paste0("sum{", idx, "} ", val)
}

#' @export
#' @method as_gmpl prod
as_gmpl.prod <- function(x, ...) {
  # GMPL uses curly braces for indexing: prod{i in I} expr
  idx <- as_gmpl(x$index, brackets = "{}", ...)
  val <- as_gmpl(x$value, ...)

  # If the value is an addition or subtraction, wrap in parentheses
  # Check both direct value and when$then (for conditional products)
  value_expr <- x$value
  if (inherits(value_expr, "when") && inherits(value_expr$then, "expression")) {
    value_expr <- value_expr$then
  }
  if (inherits(value_expr, "expression") && value_expr$op %in% c("+", "-")) {
    val <- paste0("(", val, ")")
  }

  paste0("prod{", idx, "}(", val,")")
}

#' @export
#' @method as_gmpl setmin
as_gmpl.setmin <- function(x, ...) {
  # GMPL uses curly braces for indexed min: min{i in I} expr
  # Build index spec: "var in SET, var2 in SET2"
  if (inherits(x$index, "dims") && length(x$index) > 0) {
    index_specs <- sapply(names(x$index), function(var_name) {
      set_expr <- x$index[[var_name]]
      set_name <- dim_binding_name(set_expr)
      paste(var_name, "in", set_name)
    })
    idx <- paste(index_specs, collapse = ", ")
  } else {
    # Fallback for unexpected structure
    idx <- as_gmpl(x$index, brackets = "", ...)
  }

  val <- as_gmpl(x$value, ...)

  # If the value is an addition or subtraction, wrap in parentheses
  # Check both direct value and when$then (for conditional min)
  value_expr <- x$value
  if (inherits(value_expr, "when") && inherits(value_expr$then, "expression")) {
    value_expr <- value_expr$then
  }
  if (inherits(value_expr, "expression") && value_expr$op %in% c("+", "-")) {
    val <- paste0("(", val, ")")
  }

  paste0("min{", idx, "} ", val)
}

#' @export
#' @method as_gmpl setmax
as_gmpl.setmax <- function(x, ...) {
  # GMPL uses curly braces for indexed max: max{i in I} expr
  # Build index spec: "var in SET, var2 in SET2"
  if (inherits(x$index, "dims") && length(x$index) > 0) {
    index_specs <- sapply(names(x$index), function(var_name) {
      set_expr <- x$index[[var_name]]
      set_name <- dim_binding_name(set_expr)
      paste(var_name, "in", set_name)
    })
    idx <- paste(index_specs, collapse = ", ")
  } else {
    # Fallback for unexpected structure
    idx <- as_gmpl(x$index, brackets = "", ...)
  }

  val <- as_gmpl(x$value, ...)

  # If the value is an addition or subtraction, wrap in parentheses
  # Check both direct value and when$then (for conditional max)
  value_expr <- x$value
  if (inherits(value_expr, "when") && inherits(value_expr$then, "expression")) {
    value_expr <- value_expr$then
  }
  if (inherits(value_expr, "expression") && value_expr$op %in% c("+", "-")) {
    val <- paste0("(", val, ")")
  }

  paste0("max{", idx, "} ", val)
}

#' @export
#' @method as_gmpl call
#' @rdname as_gmpl
as_gmpl.call <- function(x, ...) {
  # Handle generic function calls: name(arg1, arg2, ...)
  # Example: min(y), max(a,b), abs(x)
  stopifnot(!is.null(x$name))

  # Convert arguments
  if (!is.null(x$args) && length(x$args) > 0) {
    args_str <- sapply(x$args, as_gmpl, ...)
    args_text <- paste(args_str, collapse = ", ")
    paste0(x$name, "(", args_text, ")")
  } else {
    # No arguments
    paste0(x$name, "()")
  }
}

#' @export
#' @method as_gmpl func
#' @rdname as_gmpl
as_gmpl.func <- function(x, ...) {
  # Special handling for sum/prod with GAMS-style conditional indexing
  if (tolower(x$name) %in% c("sum", "prod") && !is.null(x$index)) {
    # Convert GAMS sum(iterator$condition, expr) to GMPL sum{iterator in Set: condition}(expr)
    return(as_gmpl_sum_prod(x, ...))
  }

  # Generic function handling
  val <- if (inherits(x$value, c("ast", "multimod"))) {
    as_gmpl(x$value, ...)
  } else if (is.list(x$value) && !is.data.frame(x$value)) {
    sapply(x$value, as_gmpl, ...)
  } else {
    stop("Unsupported value type for func: ", class(x$value))
  }
  val_str <- paste(val, collapse = ", ")

  if (!is.null(x$index)) {
    idx <- as_gmpl(x$index, ...)
    return(paste0(x$name, "(", idx, ", ", val_str, ")"))
  } else {
    return(paste0(x$name, "(", val_str, ")"))
  }
}

#' Convert GAMS-style sum/prod to GMPL binding syntax
#' @param x A func AST node representing sum or prod
#' @param ... Additional arguments passed to as_gmpl
#' @return GMPL sum/prod expression with proper binding
#' @keywords internal
as_gmpl_sum_prod <- function(x, ...) {
  # Extract model from ... if present
  dots <- list(...)
  model <- dots$model

  # Extract iterator and condition from when node
  # GAMS: sum(comm$mTechGroupComm[...], expr)
  # GMPL: sum{c in comm: (...) in mTechGroupComm}(expr)

  if (!inherits(x$index, "when")) {
    # Fallback to regular handling if not a conditional index
    idx <- as_gmpl(x$index, brackets = "{}", ...)
    val <- as_gmpl(x$value, ...)
    return(paste0(x$name, "{", idx, "}(", val, ")"))
  }

  idx_when <- x$index
  iterator <- idx_when$then  # The set/symbol(s) being iterated (e.g., comm or [year, slice])
  condition <- idx_when$condition  # The filter condition (e.g., mTechGroupComm[...])

  # Check if iterator is a tuple (dims object) or single symbol
  is_tuple <- inherits(iterator, "dims")

  if (is_tuple) {
    # Multiple iterators: sum((year, slice)$condition, expr)
    # In GMPL, multi-iterator with condition must be NESTED sums:
    # sum{y in year}(sum{s in slice: condition}(expr))

    iter_sets <- sapply(iterator, dim_binding_name)

    # Resolve aliases to base sets for iteration
    # In GMPL, we iterate over base sets, not aliases
    iter_base_sets <- iter_sets
    if (!is.null(model) && !is.null(model$aliases)) {
      for (i in seq_along(iter_sets)) {
        set_name <- iter_sets[i]
        # Check if this is an alias
        for (alias_pair in model$aliases) {
          if (length(alias_pair) == 2 && alias_pair[2] == set_name) {
            # This is an alias, use the base set
            iter_base_sets[i] <- alias_pair[1]
            break
          }
        }
      }
    }

    # Generate dummy variables for each iterator
    index_aliases <- getOption("multimod.index_aliases", NULL)
    existing_iter_vars <- getOption("multimod.iterator_vars", list())
    all_symbols <- if (!is.null(model)) build_symbols_list(model) else NULL
    all_set_names <- if (!is.null(model) && !is.null(model$sets)) names(model$sets) else character(0)

    iter_index_aliases <- character(length(iter_sets))
    for (i in seq_along(iter_sets)) {
      set_name <- iter_sets[i]

      # Generate dummy var, avoiding ALL used names including equation-level index_aliases
      used_names <- c(
        unlist(index_aliases, use.names = FALSE),  # Equation-level dummies
        unlist(existing_iter_vars, use.names = FALSE),  # Other iterator dummies
        iter_index_aliases[seq_len(i-1)]  # Already generated vars for this tuple
      )

      iter_index_aliases[i] <- generate_index_alias(
        set_name = set_name,
        used_names = used_names,
        all_set_names = all_set_names,
        all_symbols = all_symbols
      )
    }

    # Set up iterator variable replacement context FOR ALL iterators
    old_iter_vars <- getOption("multimod.iterator_vars", list())
    iter_vars <- old_iter_vars
    for (i in seq_along(iter_sets)) {
      iter_vars[[iter_sets[i]]] <- iter_index_aliases[i]
    }
    options(multimod.iterator_vars = iter_vars)
    on.exit(options(multimod.iterator_vars = old_iter_vars), add = TRUE)

    # Build NESTED sums: sum{a in A}(sum{b in B: condition}(expr))
    # Only the INNERMOST iterator gets the condition
    result <- as_gmpl(x$value, ...)

    # Process from innermost to outermost (reverse order)
    for (i in rev(seq_along(iter_sets))) {
      # Use BASE set for iteration, not alias
      binding <- paste(iter_index_aliases[i], "in", iter_base_sets[i])

      if (i == length(iter_sets)) {
        # Innermost: add condition
        options(multimod.in_binding_condition = TRUE)
        cond_str <- as_gmpl(condition, ...)
        options(multimod.in_binding_condition = FALSE)

        result <- paste0(x$name, "{", binding, ": ", cond_str, "}(", result, ")")
      } else {
        # Outer loops: no condition
        result <- paste0(x$name, "{", binding, "}(", result, ")")
      }
    }

    return(result)

  } else {
    # Single iterator - existing logic
    iter_name <- if (inherits(iterator, "symbol")) {
      iterator$name
    } else if (inherits(iterator, "set")) {
      iterator$name
    } else {
      as_gmpl(iterator, ...)
    }

    # Generate dummy variable for iterator
    index_aliases <- getOption("multimod.index_aliases", NULL)
    existing_iter_vars <- getOption("multimod.iterator_vars", list())

    # Get the dummy var for this iterator set (accounting for aliases)
    if (!is.null(index_aliases) && iter_name %in% names(index_aliases)) {
      # Use the equation-level dummy variable for this set
      candidate_dummy <- index_aliases[[iter_name]]
    } else {
      # Iterator set not in equation's index_aliases - generate using model-level logic
      # This handles cases where sum iterates over a set/alias not in the equation signature

      # Get all symbols from model for conflict checking
      all_symbols <- if (!is.null(model)) {
        build_symbols_list(model)
      } else {
        NULL
      }

      # Collect all existing dummy vars to check for conflicts
      used_names <- c(
        unlist(index_aliases, use.names = FALSE),
        unlist(existing_iter_vars, use.names = FALSE)
      )

      # Get all set names for alias detection
      all_set_names <- if (!is.null(model) && !is.null(model$sets)) {
        names(model$sets)
      } else {
        character(0)
      }

      # Generate dummy var for this iterator using the same logic as generate_index_aliases
      candidate_dummy <- generate_index_alias(
        set_name = iter_name,
        used_names = used_names,
        all_set_names = all_set_names,
        all_symbols = all_symbols
      )
    }

    # Check if this dummy var collides with existing iterator vars or equation-level dummy vars
    used_index_aliases <- c(unlist(existing_iter_vars, use.names = FALSE), unlist(index_aliases, use.names = FALSE))

    if (candidate_dummy %in% used_index_aliases && !(iter_name %in% names(existing_iter_vars))) {
      # Collision detected - append number to make unique
      counter <- 2
      while (TRUE) {
        test_var <- paste0(candidate_dummy, counter)
        if (!(test_var %in% used_index_aliases)) {
          dummy_var <- test_var
          break
        }
        counter <- counter + 1
        if (counter > 10) {
          # Fallback to using more letters from set name
          dummy_var <- substr(iter_name, 1, min(4, nchar(iter_name)))
          break
        }
      }
    } else {
      dummy_var <- candidate_dummy
    }

    # Resolve alias to base set for iteration
    # In GMPL, we iterate over base sets, not aliases
    iter_base_set <- iter_name
    if (!is.null(model) && !is.null(model$aliases)) {
      for (alias_pair in model$aliases) {
        if (length(alias_pair) == 2 && alias_pair[2] == iter_name) {
          # This is an alias, use the base set
          iter_base_set <- alias_pair[1]
          break
        }
      }
    }

    # Build single-iterator binding: c in comm (using base set)
    bindings <- paste(dummy_var, "in", iter_base_set)

    # Set up iterator variable replacement BEFORE processing condition
    # Store the mapping of iterator set name to dummy variable
    old_iter_vars <- getOption("multimod.iterator_vars", list())
    iter_vars <- old_iter_vars
    iter_vars[[iter_name]] <- dummy_var
    options(multimod.iterator_vars = iter_vars)
    on.exit(options(multimod.iterator_vars = old_iter_vars), add = TRUE)
  }

  # Build condition part (common for both single and tuple iterators)
  # Set flag to indicate we're in a binding condition context
  # This tells as_gmpl methods to convert mappings/params/vars to membership tests
  old_in_binding <- getOption("multimod.in_binding_condition", FALSE)
  options(multimod.in_binding_condition = TRUE)

  # Convert condition - as_gmpl will handle membership tests automatically now
  cond_str <- as_gmpl(condition, ...)

  # Reset binding flag immediately after processing condition
  # The value should NOT have membership test conversion
  options(multimod.in_binding_condition = old_in_binding)

  # Build the binding: {bindings: condition}
  # bindings is either "c in comm" or "y in year, s in slice"
  binding <- paste0(bindings, ": ", cond_str)

  # Handle value - may contain nested when for additional filtering
  # Check if value is an AST node (expression, variable, etc.) first
  # AST nodes are also lists, so must check inherits() before is.list()
  val_str <- if (inherits(x$value, "ast")) {
    # Single AST node - convert directly
    as_gmpl(x$value, ...)
  } else if (is.list(x$value) && length(x$value) > 1) {
    # Multiple value elements - first is likely mapping, rest are expressions
    # Skip the first element if it's a mapping (that's the iterator set definition)
    val_items <- x$value
    start_idx <- 1
    if (inherits(val_items[[1]], "mapping") || inherits(val_items[[1]], "set")) {
      start_idx <- 2
    }

    if (start_idx <= length(val_items)) {
      val_exprs <- lapply(val_items[start_idx:length(val_items)], function(v) {
        if (is.null(v)) {
          return(NULL)
        }

        # Check if this is a when expression (could be direct or wrapped in expression)
        when_obj <- NULL
        if (inherits(v, "when")) {
          when_obj <- v
        } else if (inherits(v, "expression")) {
          # Check if expression contains when in lhs or rhs
          if (inherits(v$lhs, "when")) when_obj <- v$lhs
          else if (inherits(v$rhs, "when")) when_obj <- v$rhs
        }

        if (!is.null(when_obj)) {
          # Nested condition - create nested sum with FORIF placeholder
          inner_expr <- as_gmpl(when_obj$then, ...)
          inner_cond <- when_obj$condition
          if (inherits(inner_cond, "variable") || inherits(inner_cond, "parameter") || inherits(inner_cond, "mapping")) {
            # Build membership test for inner condition
            inner_dims <- ensure_dims_object(inner_cond$dims)
            if (!is.null(inner_dims) && length(inner_dims) > 0) {
              dim_list <- sapply(inner_dims, function(d) {
                set_name <- dim_binding_name(d)
                # Use iter vars or dummy vars
                iter_vars <- getOption("multimod.iterator_vars", NULL)
                if (!is.null(iter_vars) && set_name %in% names(iter_vars)) {
                  iter_vars[[set_name]]
                } else if (!is.null(index_aliases) && set_name %in% names(index_aliases)) {
                  index_aliases[[set_name]]
                } else if (set_name == iter_name) {
                  dummy_var
                } else {
                  set_name
                }
              })
              inner_cond_str <- paste0("(", paste(dim_list, collapse = ","), ") in ", inner_cond$name)
              # Wrap in nested sum with FORIF
              paste0("sum{FORIF: ", inner_cond_str, "}(", inner_expr, ")")
            } else {
              inner_cond_str <- inner_cond$name
              paste0("sum{FORIF: ", inner_cond_str, "}(", inner_expr, ")")
            }
          } else {
            paste0("if ", as_gmpl(inner_cond, ...), " then ", inner_expr)
          }
        } else {
          as_gmpl(v, ...)
        }
      })
      # Filter out NULLs
      val_exprs <- Filter(Negate(is.null), val_exprs)
      if (length(val_exprs) > 0) {
        paste(val_exprs, collapse = " ")
      } else {
        ""
      }
    } else {
      ""
    }
  } else if (is.list(x$value) && length(x$value) == 1) {
    as_gmpl(x$value[[1]], ...)
  } else {
    as_gmpl(x$value, ...)
  }

  paste0(x$name, "{", binding, "}(", val_str, ")")
}

#' @export
#' @method as_gmpl when
as_gmpl.when <- function(x, ...) {
  # Check if this is a GAMS $ operator that should become a conditional sum
  # This occurs when the condition is a mapping/variable/parameter with dimensions
  # (i.e., a membership test rather than a boolean expression)

  cond_dims <- ensure_dims_object(x$condition$dims)
  is_membership_filter <- inherits(x$condition, c("mapping", "variable", "parameter")) &&
                         !is.null(cond_dims) &&
                         length(cond_dims) > 0

  if (is_membership_filter) {
    # Convert to sum with FORIF pattern: sum{_IF_: tuple in mapping}(expression)
    # This is GMPL's way to handle GAMS $(condition) for optional terms

    then_expr <- as_gmpl(x$then, ...)

    # Build the membership test condition
    cond_name <- x$condition$name

    # Get dimension variables for the tuple
    index_aliases <- getOption("multimod.index_aliases", NULL)
    iter_vars <- getOption("multimod.iterator_vars", NULL)

    dim_list <- sapply(cond_dims, function(d) {
      set_name <- dim_binding_name(d)

      # Use iter vars or dummy vars if available
      if (!is.null(iter_vars) && set_name %in% names(iter_vars)) {
        iter_vars[[set_name]]
      } else if (!is.null(index_aliases) && set_name %in% names(index_aliases)) {
        index_aliases[[set_name]]
      } else {
        set_name
      }
    })

    tuple <- paste0("(", paste(dim_list, collapse = ","), ")")
    condition_str <- paste0(tuple, " in ", cond_name)

    # Return FORIF sum pattern
    paste0("sum{_IF_: ", condition_str, "}(", then_expr, ")")

  } else {
    # Regular if-then-else for boolean conditions
    then_expr <- as_gmpl(x$then, ...)
    cond <- as_gmpl(x$condition, ...)
    else_part <- if (!is.null(x$otherwise)) {
      paste0(" else ", as_gmpl(x$otherwise, ...))
    } else {
      ""
    }
    paste0("if ", cond, " then ", then_expr, else_part)
  }
}

#' @export
#' @method as_gmpl where
as_gmpl.where <- function(x, ...) {
  # GMPL uses ':' for conditional filtering in sets
  # Example: {i in I: condition}
  content <- as_gmpl(x$content, ...)
  condition <- as_gmpl(x$condition, ...)
  paste0(content, ": ", condition)
}

#' Get operator precedence level (higher = tighter binding)
#' @keywords internal
get_op_precedence <- function(op) {
  precedence <- list(
    "or" = 1,
    "and" = 2,
    "<" = 3, ">" = 3, "<=" = 3, ">=" = 3, "=" = 3, "==" = 3, "!=" = 3, "<>" = 3,
    "+" = 4, "-" = 4,
    "*" = 5, "/" = 5,
    "^" = 6, "**" = 6
  )
  if (op %in% names(precedence)) {
    return(precedence[[op]])
  }
  return(10)  # Unknown operators get highest precedence
}

#' @export
#' @method as_gmpl expression
as_gmpl.expression <- function(x, parent_op = NULL, parent_precedence = 0, ...) {
  stopifnot(inherits(x, "expression"))

  op_value <- as.character(x$op)[1]
  if (length(x$op) > 1 && getOption("multimod.debug_ops", FALSE)) {
    warning(sprintf("Expression operator has length %d, truncating to '%s'", length(x$op), op_value))
  }

  gmpl_ops <- c(
    "+", "-", "*", "/", "^",
    "<", ">", "=", "<=", ">=", "==", "!=", "<>",
    "and", "or", "not"
  )

  # For non-boolean operators, disable binding condition flag
  # We only want membership tests for standalone mappings in boolean context
  # Not for mappings used in arithmetic/comparison expressions
  if (!(op_value %in% c("and", "or"))) {
    old_in_binding <- getOption("multimod.in_binding_condition", FALSE)
    options(multimod.in_binding_condition = FALSE)
    on.exit(options(multimod.in_binding_condition = old_in_binding), add = TRUE)
  }

  # Get precedence of current operator
  current_precedence <- get_op_precedence(op_value)

  # Convert child expressions with parent context
  lhs <- if (inherits(x$lhs, "expression")) {
    as_gmpl(x$lhs, parent_op = op_value, parent_precedence = current_precedence, ...)
  } else {
    as_gmpl(x$lhs, ...)
  }

  rhs <- if (inherits(x$rhs, "expression")) {
    as_gmpl(x$rhs, parent_op = op_value, parent_precedence = current_precedence, ...)
  } else {
    as_gmpl(x$rhs, ...)
  }

  # Wrap sides in parentheses if the $brackets flag is TRUE
  # Exception: Don't wrap if the expression starts with an aggregation function
  # because "max{i in I} expr" can't have opening paren immediately after "max{i in I}"
  # as GLPK would interpret "max{i in I} (expr)" as max{i in I} with value=(expr)
  if (isTRUE(x$lhs$brackets) && !inherits(x$lhs, c("sum", "prod", "setmin", "setmax"))) {
    lhs <- paste0("(", lhs, ")")
  }
  if (isTRUE(x$rhs$brackets) && !inherits(x$rhs, c("sum", "prod", "setmin", "setmax"))) {
    rhs <- paste0("(", rhs, ")")
  }

  # Apply precedence-based parenthesization ONLY if $brackets is not explicitly FALSE
  # The AST from read_gmpl has the correct structure - respect it!

  # Left side: parenthesize if child has lower precedence AND brackets not explicitly set
  if (inherits(x$lhs, "expression") && !isFALSE(x$lhs$brackets)) {
    lhs_precedence <- get_op_precedence(x$lhs$op)
    if (lhs_precedence < current_precedence) {
      lhs <- paste0("(", lhs, ")")
    }
  }

  # Right side: parenthesize if child has lower or EQUAL precedence AND brackets not explicitly set
  if (inherits(x$rhs, "expression") && !isFALSE(x$rhs$brackets)) {
    rhs_precedence <- get_op_precedence(x$rhs$op)
    # For non-associative ops (-, /, ^), parenthesize equal precedence on right
    if (op_value %in% c("-", "/", "^")) {
      if (rhs_precedence <= current_precedence) {
        rhs <- paste0("(", rhs, ")")
      }
    } else {
      # For associative ops (+, *, and, or), only parenthesize lower precedence
      if (rhs_precedence < current_precedence) {
        rhs <- paste0("(", rhs, ")")
      }
    }
  }

  # For division, if RHS is an indexed access, wrap in parens to preserve semantics
  # Example: a / (b[x]) not a / b[x] which could be (a/b)[x]
  rhs_dims <- ensure_dims_object(x$rhs$dims)
  if (identical(op_value, "/") && inherits(x$rhs, c("symbol", "parameter", "variable")) &&
      !is.null(rhs_dims) && length(rhs_dims) > 0 && !grepl("^\\(", rhs)) {
    rhs <- paste0("(", rhs, ")")
  }

  # Operator normalization
  op <- op_value
  if (op == "**") op <- "^"  # GMPL uses ^ not **
  if (op == "==") op <- "="
  if (op == "!=") op <- "<>"

  if (!op %in% gmpl_ops) {
    stop("Unsupported operator for GMPL export: ", op)
  }

  paste(lhs, op, rhs)
}

#' @export
#' @method as_gmpl unary
as_gmpl.unary <- function(x, ...) {
  stopifnot(inherits(x, "unary"))
  op <- x$op
  # Note: unary nodes store operand in $rhs, not $expr
  operand <- if (!is.null(x$rhs)) x$rhs else x$expr  # fallback for old code
  if (op == "-") {
    paste0("-", as_gmpl(operand, ...))
  } else if (op == "not") {
    paste0("not ", as_gmpl(operand, ...))
  } else {
    stop("Unrecognized unary operator for GMPL export: ", op)
  }
}

#' @export
#' @method as_gmpl equation
as_gmpl.equation <- function(x, model = NULL, is_objective = FALSE, obj_info = NULL, ...) {
  eqname <- x$name
  dims_obj <- ensure_dims_object(x$dims)
  domain_dims_obj <- if (!is.null(x$domain) && !is.null(x$domain$dims)) {
    ensure_dims_object(x$domain$dims)
  } else {
    NULL
  }

  # Generate index tuple with dummy variables
  # We need the equation dims and optionally the domain mapping
  index_tuple <- ""
  if ((!is.null(domain_dims_obj) && length(domain_dims_obj) > 0) ||
      (!is.null(dims_obj) && length(dims_obj) > 0)) {
    # Extract set names from dims
    # Priority: use domain$dims if available (contains aliases as used in equation body)
    # Otherwise fall back to equation$dims (base set names)
    dims_source <- if (!is.null(domain_dims_obj) && length(domain_dims_obj) > 0) {
      domain_dims_obj
    } else {
      dims_obj
    }

    set_names <- sapply(dims_source, function(d) {
      if (inherits(d, "symbol")) d$name
      else if (inherits(d, "set")) d$name
      else as.character(d)
    })

    # Use equation-specific index variables if available (from original GMPL)
    if (!is.null(x$dims_index_aliases) && length(x$dims_index_aliases) == length(set_names)) {
      # Use the original iterator variables from the parsed equation
      dims_index_aliases <- x$dims_index_aliases
    } else {
      # Use model's global index_aliases if available
      if (!is.null(model) && !is.null(model$index_aliases)) {
        # Map each set name to its global alias
        dims_index_aliases <- sapply(set_names, function(sname) {
          if (sname %in% names(model$index_aliases)) {
            model$index_aliases[[sname]]
          } else {
            # Fallback to first letter if not in global aliases
            tolower(substr(sname, 1, 1))
          }
        })
        names(dims_index_aliases) <- set_names
      } else {
        # Fall back to generating new ones
        # This should not normally happen - equations should have dims_index_aliases
        stop("Equation '", eqname, "' is missing dims_index_aliases. ",
             "This should have been preserved from original GMPL parsing or set during equation creation. ",
             "Equation dims: ", paste(set_names, collapse = ", "), ". ",
             "Cannot auto-generate index aliases - they must match the variable references in equation body.")
      }
    }

    # Create the tuple using ONLY the equation dims (not aliases)
    var_list <- paste(dims_index_aliases, collapse = ", ")

    # Build full index_aliases mapping including all aliases
    # This ensures that if the equation uses 'dst' (alias of region), it maps to the same 'r'
    # BUT: If both base set and alias appear in the same equation dims, they need DIFFERENT dummy vars
    index_aliases <- dims_index_aliases
    if (!is.null(model$aliases) && length(model$aliases) > 0) {
      for (alias_pair in model$aliases) {
        if (length(alias_pair) == 2) {
          base_set <- alias_pair[1]
          alias_set <- alias_pair[2]

          # Check if BOTH base and alias are in the equation dims
          both_in_dims <- (base_set %in% set_names) && (alias_set %in% set_names)

          # Only unify the dummy vars if they don't BOTH appear in dims
          if (!both_in_dims) {
            # If the base set is in our index_aliases, add the alias with same dummy var
            if (base_set %in% names(index_aliases)) {
              index_aliases[[alias_set]] <- index_aliases[[base_set]]
            }
            # Also check reverse - if alias is in dims, map base set too
            if (alias_set %in% names(index_aliases)) {
              index_aliases[[base_set]] <- index_aliases[[alias_set]]
            }
          }
        }
      }
    }

    # Now var_list has the correct tuple, index_aliases has all mappings

    # Check if there's a domain mapping or condition
    if (!is.null(x$domain) && inherits(x$domain, "symbol")) {
      domain_name <- x$domain$name
      index_tuple <- paste0("{(", var_list, ") in ", domain_name, "}")
    } else if (!is.null(x$domain) && inherits(x$domain, "mapping")) {
      domain_name <- x$domain$name
      index_tuple <- paste0("{(", var_list, ") in ", domain_name, "}")
    } else if (!is.null(x$domain) && inherits(x$domain, "expression")) {
      # Domain is a condition/filter expression
      # Use Cartesian product with condition
      # Use dims_index_aliases (only equation dims) not index_aliases (which includes all aliases)
      set_pairs <- paste(paste(dims_index_aliases, "in", set_names), collapse = ", ")
      domain_cond <- as_gmpl(x$domain, model = model, ...)
      index_tuple <- paste0("{", set_pairs, ": ", domain_cond, "}")
    } else {
      # No domain mapping, just Cartesian product
      # {t in tech, r in region, c in comm, ...}
      # Use dims_index_aliases (only equation dims) not index_aliases (which includes all aliases)
      set_pairs <- paste(paste(dims_index_aliases, "in", set_names), collapse = ", ")
      index_tuple <- paste0("{", set_pairs, "}")
    }

    # Store dummy vars in environment for use in expression conversion
    # This allows expressions to use the dummy variable names
    old_index_aliases <- getOption("multimod.index_aliases", NULL)
    options(multimod.index_aliases = index_aliases)
    on.exit(options(multimod.index_aliases = old_index_aliases), add = TRUE)
  }

  # For objectives, only need to convert RHS expression
  if (is_objective && !is.null(obj_info)) {
    rhs <- as_gmpl(x$rhs, model = model, ...)
    # Format: minimize cost: expression;
    # Validate and standardize sense
    direction <- obj_info$sense
    if (!direction %in% c("minimize", "maximize")) {
      stop("Invalid objective sense '", direction, "'. Must be 'minimize' or 'maximize'.")
    }
    variable_name <- obj_info$variable
    return(paste0(direction, " ", variable_name, ": ", rhs, ";"))
  }

  # For regular constraints
  lhs <- as_gmpl(x$lhs, model = model, ...)
  rhs <- as_gmpl(x$rhs, model = model, ...)

  # GMPL doesn't use special relation symbols, just regular operators
  rel <- switch(x$relation,
                "==" = "=",
                "<=" = "<=",
                ">=" = ">=",
                stop("Unsupported relation: ", x$relation)
  )

  # Build the constraint string
  constraint_str <- paste0("s.t. ", eqname, index_tuple, ": ",
         lhs, " ", rel, " ", rhs, ";")

  # Post-process: resolve aliases if requested
  resolve_aliases <- getOption("multimod.resolve_aliases", TRUE)
  if (resolve_aliases && !is.null(model) && !is.null(model$aliases)) {
    replacements_made <- list()

    for (alias_group in model$aliases) {
      if (length(alias_group) > 1) {
        base_set <- alias_group[1]
        aliases <- alias_group[-1]

        for (alias in aliases) {
          # Only replace after 'in' keyword to avoid replacing iterator variables
          # Pattern: \bin\s+<alias>\b -> in <base_set>
          pattern <- paste0("\\bin\\s+", alias, "\\b")
          replacement <- paste0("in ", base_set)

          # Check if pattern exists before replacing
          matches <- grepl(pattern, constraint_str, perl = TRUE)
          if (any(matches)) {
            constraint_str <- gsub(pattern, replacement, constraint_str, perl = TRUE)
            replacements_made[[alias]] <- base_set
          }
        }
      }
    }

    # Add comment if replacements were made
    if (length(replacements_made) > 0) {
      replacements_str <- paste(
        sapply(names(replacements_made), function(a) {
          paste0(a, "->", replacements_made[[a]])
        }),
        collapse = ", "
      )
      constraint_str <- paste0("# Aliases resolved: ", replacements_str, "\n", constraint_str)
    }
  }

  return(constraint_str)
}


