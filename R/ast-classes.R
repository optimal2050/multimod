# classes ####

#' Create a new multimod AST (Abstract Syntax Tree) node
#'
#' This is a generic constructor for all `ast` node types,
#' used internally by the `*()` functions to define structured equations.
#'
#' @param node_type The type of AST node (e.g., `"expression"`, `"variable"`,
#' `"constant"`, `"parameter"`, `"symbol"`).
#' @param ... Additional named fields passed as components of the AST node.
#'
#' @return A structured S3 object of class `ast`, with a subclass
#' matching `node_type`.
#' @export
new_ast <- function(node_type, ..., inherits_class = NULL) {
  stopifnot(is.character(node_type))
  stopifnot(length(node_type) == 1)
  stopifnot(is.null(inherits_class) || !is.na(inherits_class) || is.character(inherits_class))
  structure(list(...),
            class = c(node_type, inherits_class, "ast"))
            # class = c(paste0("", node_type), inherits_class, "ast"))
}

#' Create an AST node representing a set
#'
#' Constructs a set object used as an index domain for other symbols
#' (e.g., mappings, parameters, variables, equations).
#'
#' @param name Character. Name of the set (e.g., "b").
#'
#' @return An object of class `ast` and subclass `set`.
#'
#' @export
#' @rdname new_ast
#' @examples
#' ast_set("b")                     # standalone set
ast_set <- function(name) {
  stopifnot(is.character(name), length(name) == 1)
  # if (!is.null(domain)) stopifnot(inherits(domain, "ast") && domain$type == "set")
  new_ast("set", name = name)
}
# @param domain Optional. Another AST set node representing the parent set
# (e.g., "a" in b(a)), indicating that this set is a subset of the parent set.
# ast_set("b", domain = ast_set("a"))  # subset declaration b ⊆ a

# Create an AST node for a list of index symbols
#
# Constructs an `index` object, which represents a list of index symbols
# used in a indexed function or expression. Each index is typically represented
# as a set reference (`set`) or as a symbolic identifier (`symbol`).
# Similar to `dims`, but specifically for indexing.
#
# @param sets A list of AST nodes (e.g., set or symbol), one for each index
#
# @return An `index` object (subclass of `ast`)
# @export
# index <- function(sets) {
#   stopifnot(is.list(sets))
#   lapply(sets, function(x) {
#     stopifnot(inherits(x, "ast"))
#   })
#   new_ast("index", symbols = symbols)
# }

#' Create an AST node for dimensions (dims) of a symbol
#'
#' Constructs an `dims` object, which represents the declared dimensions
#' of a variable, parameter, or equation in a structured symbolic model. Each
#' dimension is typically represented as a set reference (`set`) or as a
#' symbolic identifier (`symbol`). This is similar to `index`, but
#' specifically for dimensions.
#'
#' @param ... One or more dimension expressions, typically created using
#'   [ast_set()] or [ast_symbol()]. Expected AST objects or character strings,
#'   comma-separated.
#'   If character strings are provided, they will be converted to `symbol` objects.
#'
#' @return A list of class `dims` and `ast`, representing the dimension nodes.
#'
#' @export
#' @examples
#' ast_dims(list(ast_set("tech"), ast_set("region")))
#' ast_dims(ast_symbol("t"))
ast_dims <- function(...) {
  # browser()
  args <- list(...)
  if (!is.null(args$symbols)) {
    symb_list <- args$symbols
    args$symbols <- NULL
  } else {
    symb_list <- NULL
  }
  dims <- args

  is_simple_identifier <- function(value) {
    is.character(value) && length(value) == 1 && !is.na(value) &&
      grepl("^[A-Za-z][A-Za-z0-9_]*$", value)
  }

  if (length(dims) == 0 || (length(dims) == 1 && is_empty(dims[[1]]))) {
    # empty
    return(new_ast("dims"))
  }

  # Preserve names before any transformations
  dims_names <- names(dims)

  flatten_dim_entries <- function(items) {
    out <- list()
    for (item in items) {
      if (inherits(item, "ast")) {
        out[[length(out) + 1]] <- item
        next
      }
      if (is.list(item) && !inherits(item, "ast")) {
        # Recursively flatten plain lists (e.g., build_stage = list("NODE", "TECH"))
        sub_items <- flatten_dim_entries(item)
        if (length(sub_items)) {
          out <- c(out, sub_items)
        }
        next
      }
      if (is.atomic(item) && length(item) > 1) {
        # Multiple identifiers packaged together; expand into scalars
        out <- c(out, as.list(item))
        next
      }
      out[[length(out) + 1]] <- item
    }
    out
  }

  dims <- flatten_dim_entries(dims)

  # check if ... is an unnamed list of objects
  if (length(dims) == 1 && !inherits(dims, "ast") &&
      #is.null(names(dims)) &&
      (is.list(dims[[1]]) || is.vector(dims[[1]]))
      ) {
    # Unwrap single-element list but preserve names if they exist
    if (!is.null(dims_names) && !is.na(dims_names[1]) && dims_names[1] != "") {
      # Keep as list to preserve name
      # dims stays as is
    } else {
      dims <- dims[[1]]
      dims_names <- names(dims)  # Get names from unwrapped list
    }
  }

  dims <- lapply(dims, function(x) {
    # browser()
    if (inherits(x, "ast")) {
      return(x)
    }
    if (is_simple_identifier(x)) {
      return(ast_symbol(x))
    }
    
    # Parse as expression with language context (handles set names with hyphens, etc.)
    ast_parse_expr(x, symbols = symb_list)
  })

  # Restore names after lapply where the original length still matches.
  if (!is.null(dims_names) && length(dims_names) == length(dims)) {
    names(dims) <- dims_names
  }

  # if (all(cls %in% c("set", "symbol"))) {
  #   # pass
  # } else if (all(cls %in% "character")) {
  #   # Convert character to set
  #   dims <- lapply(dims, ast_set)
  # } else {
  #   stop("Invalid input for `dims`: ",
  #        "must be set, symbol, or characters.")
  # }
  if (!is.list(dims)) browser()
  out <- do.call(new_ast, c("dims", dims))
  return(out)
}

#' Create a mapping AST node
#'
#' Constructs a mapping node representing a mapping or set of indices.
#'
#' @param name A character string representing the mapping name.
#' @param dims An `dims` object representing the dimensions of the mapping.
#'
#' @return An `mapping` S3 object (subclass of `ast`).
#' @export
ast_mapping <- function(name, dims = ast_dims(), ...) {
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  # stopifnot(is.character(dims))
  stopifnot(length(dims) > 0)
  # if (!is.null(domain)) {
  #   stopifnot(inherits(domain, "ast") && domain$type == "mapping")
  # }
  new_ast("mapping", name = name, dims = dims, ...)
}
# @param domain An optional `mapping` object representing the parent mapping,
# indicating that this mapping is a subset of the parent mapping.



#' Create a variable AST node
#'
#' @param name Name of the variable.
#' @param dims Optional vector of dimension `dims` objects.
#'
#' @return An `variable` S3 object (subclass of `ast`).
#' @export
ast_variable <- function(name, dims = ast_dims(), vtype = NULL, bounds = NULL, ...) {
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  # Only include non-NULL optional arguments
  args <- list(name = name, dims = dims, ...)
  if (!is.null(vtype)) args$vtype <- vtype
  if (!is.null(bounds)) args$bounds <- bounds
  do.call(new_ast, c(list("variable"), args))
}

#' Create a parameter AST node
#'
#' @param name Name of the parameter.
#' @param dims Optional vector of dimension `dims` objects.
#'
#' @return An `parameter` S3 object (subclass of `ast`).
#' @export
ast_parameter <- function(name, dims = ast_dims(), ...) {
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  new_ast("parameter", name = name, dims = dims, ...)
}

#' Create a symbol AST node (for unclassified identifiers)
#'
#' @param name A character string representing the symbol.
#'
#' @return An `symbol` S3 object (subclass of `ast`).
#' @export
ast_symbol <- function(name, ...) {
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  new_ast("symbol", name = name, ...)
}

#' Create an index shift AST node for ordered set references
#'
#' Constructs a shifted index reference for ordered sets (e.g., YEAR, SEASON).
#' Used for GAMS lag/lead operators like `y-1` (previous year) or `y+1` (next year).
#' Negative offset represents backward shift (lag), positive represents forward shift (lead).
#'
#' @param symbol A character string representing the index symbol (e.g., "y", "ls", "ld").
#' @param offset An integer offset. Negative for backward shift (e.g., -1 for y-1), positive for forward shift (e.g., +1 for y+1).
#'
#' @return A `shift` S3 object (subclass of `ast`).
#' @export
#' @examples
#' ast_shift("y", -1)  # y-1 (previous year)
#' ast_shift("y", 1)   # y+1 (next year)
#' ast_shift("ls", -1) # ls-1 (previous season)
ast_shift <- function(symbol, offset, ...) {
  stopifnot(is.character(symbol), length(symbol) == 1)
  stopifnot(is.numeric(offset), length(offset) == 1)
  stopifnot(offset == round(offset))  # must be integer
  stopifnot(offset != 0)  # zero offset doesn't make sense
  new_ast("shift", symbol = symbol, offset = as.integer(offset), ...)
}

#' Create a constant AST node
#'
#' @param value A numeric or character constant.
#'
#' @return An `constant` S3 object (subclass of `ast`).
#' @export
ast_constant <- function(value, ...) {
  stopifnot(is.numeric(value) || is.character(value))
  new_ast("constant", value = value, ...)
}

#' Construct a conditional expression node for multimod AST
#'
#' This function constructs a conditional expression node of type `"when"`,
#' representing GAMS-style conditional terms using the `$` operator,
#' `if ... else ...` statements in Julia/JuMP,  `if ... in ...` in Python/Pyomo,
#' etc.
#'
#' @param condition The condition to check, must be an AST node.
#' @param then The expression to evaluate if the condition is true
#' (usually the left-hand side).
#' @param otherwise Optional expression to evaluate if the condition is false.
#' This part is not available in GAMS `$` statements, but is useful for other
#' languages.
#'
#' @return An object of class `ast` and subclass `when`.
#' @export
#'
#' @examples
#' ast_when(
#'   condition = ast_symbol("i_active(i)"),
#'   then = ast_variable("x", c("i"))
#' )
ast_when <- function(condition, then, otherwise = NULL, ...) {
  # browser()
  stopifnot(inherits(then, "ast") || is.null(then))
  stopifnot(inherits(condition, "ast"))

  new_ast("when", condition = condition, then = then,
          otherwise = otherwise, ...)
}

#' Create a summation AST node
#'
#' Constructs an abstract syntax tree (AST) node representing a summation over an
#' index. The index is typically a `set`, `dims` or `when` object if
#' filtering is applied to the index.
#'
#' @param index An AST node representing the index variable(s) (e.g., `ast_dims("t")`).
#' @param value An AST node representing the expression to be summed.
#' @param domain An optional AST node representing a domain condition.
#'   This can be a mapping, a logical condition (e.g., from a `$`-filter), or a parameter.
#'   Use `NULL` if there is no restriction.
#'
#' @return An object of class `ast` and `sum`.
#' @export
ast_sum <- function(index = ast_dims(), value, domain = NULL) {
  stopifnot(inherits(index, "ast"))
  stopifnot(inherits(value, "ast"))
  if (!is.null(domain)) stopifnot(inherits(domain, "ast"))
  new_ast("sum", index = index, value = value, domain = domain)
}


#' Create a product AST node
#'
#' Constructs an abstract syntax tree (AST) node representing a product over an
#' index. The index is typically a `set`, `dims` or `when` object if
#' filtering is applied to the index.
#'
#' @inheritParams ast_sum
#' @return An object of class `ast` and `prod`.
#' @export
ast_prod <- function(index, value, domain = NULL) {
  stopifnot(inherits(index, "ast"))
  stopifnot(inherits(value, "ast"))
  if (!is.null(domain)) stopifnot(inherits(domain, "ast"))
  new_ast("prod", index = index, value = value, domain = domain)
}

#' Create a function AST node
#'
#' Constructs an abstract syntax tree (AST) node representing a function.
#' @param name A character string representing the function name.
#' @param value An AST node representing the function body or expression.
#' @param index An AST node (typically dims or when) defining the index set.
#' @return An `ast` object of class `function`.
#' @export
ast_func <- function(name, value, index = NULL, ...) {
  # browser()
  stopifnot(is.character(name), length(name) == 1)

  # index must be NULL or a single ast (like ast_dims or ast_when)
  if (!is.null(index) && !inherits(index, "ast")) {
    stop("`index` must be NULL or an ast object (e.g., ast_dims or ast_when).")
  }

  # value can be a single ast or a list of asts (for multi-arg functions)
  if (!(inherits(value, "ast") || (is.list(value) && all(sapply(value, inherits, "ast"))))) {
    stop("`value` must be an ast object or a list of ast objects.")
  }

  new_ast("func", name = name, index = index, value = value, ...)
}



# ast_func <- function(name, index = NULL, ...) {
#   stopifnot(is.character(name), length(name) == 1)
#   stopifnot(is.null(index) || inherits(index, "ast"))
#   stopifnot(inherits(value, "ast"))
#   new_ast("func", name = name, index = index, value = value)
# }

# ast_func_indexed <- function(fun, index, value) {
#   stopifnot(is.character(fun), length(fun) == 1)
#   stopifnot(inherits(index, "ast"))
#   stopifnot(inherits(value, "ast"))
#   structure(
#     list(fun = fun, index = index, value = value),
#     class = c("func_indexed", "ast")
#   )
# }



#' Create an expression AST node
#'
#' Constructs a binary operation node representing an expression such as addition, multiplication, etc.
#'
#' @param op A character string representing the operator (e.g., `+`, `*`, `/`).
#' @param lhs The left-hand side AST node.
#' @param rhs The right-hand side AST node.
#'
#' @return An `expression` S3 object (subclass of `ast`).
#' @export
ast_expression <- function(op, lhs, rhs, brackets = NULL) {
  stopifnot(is.character(op), !is.null(lhs), !is.null(rhs))
  stopifnot(inherits(lhs, "ast"), inherits(rhs, "ast"))
  stopifnot(length(op) == 1)
  if (length(brackets) > 1) browser()
  stopifnot(length(brackets) <= 1)
  new_ast("expression", op = op, lhs = lhs, rhs = rhs, brackets = brackets)
}

#' Create a unary expression AST node
#'
#' Represents a unary operator such as "-" or "not" applied to a single argument.
#'
#' @param op Character string. The unary operator, e.g., "-" or "not".
#' @param rhs The operand (a `ast` object).
#'
#' @return An object of class `ast` and `unary`.
#' @export
#' @examples
#' ast <- ast_unary("-", ast_symbol("x"))
#' str(ast)
#' ast_unary("not", ast_symbol("x"))
ast_unary <- function(op, rhs) {
  stopifnot(is.character(op), length(op) == 1)
  stopifnot(inherits(rhs, "ast"))
  # Check if the operator is valid
  valid_operators <- c("-", "not", "+", "!")
  if (!op %in% valid_operators) {
    stop("Invalid unary operator: ", op,
         "\nValid operators: ", paste(valid_operators, collapse = ", "))
  }
  # Create the unary AST node
  new_ast("unary", op = op, rhs = rhs)
  # new_unary(op, rhs)
}


#' Create an equation AST node
#'
#' Constructs an equation node representing a mathematical equation.
#' This node includes a left-hand side (LHS), right-hand side (RHS),
#' relation operator (e.g., equality or inequality), and an optional domain
#' (e.g., mapping, expression or logical condition).
#'
#' @param lhs The left-hand side of the equation (an AST node).
#' @param rhs The right-hand side of the equation (an AST node).
#' @param relation A character string representing the relation type.
#'  One of `"=="`, `"<="`, or `">="`.
#'  @param name Optional character string. The name of the equation.
#'  @param domain Optional AST node representing the domain condition.
#'  @param desc Optional character string. A description or label for the equation.
#'
#'  @return An `equation` S3 object.
#'  @export
ast_equation <- function(lhs, rhs, relation = "==",
                         name = NULL, domain = NULL, desc = NULL) {
  stopifnot(relation %in% c("==", "<=", ">=", "<", ">"))
  stopifnot(inherits(lhs, "ast"))
  stopifnot(inherits(rhs, "ast"))
  new_ast(
    "equation",
    lhs = lhs,
    rhs = rhs,
    relation = relation,
    name = name,
    domain = domain,
    desc = desc
  )
}

#' Create a "where" AST node
#'
#' Constructs a "where" node representing a reference to a specific
#' location in the abstract syntax tree (AST).
#'
#' @param name A character string representing the name of the reference,
#' matching the name of the symbol replacing AST node or a branch of the AST.
#' @param content The AST content
#' @param hash Hash value for the node
#' @param ... Additional attributes
#'
#' @returns An AST where node
#' @export
ast_where <- function(name, content, hash = node_hash(content), ...) {
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  stopifnot(is.null(content) || inherits(content, "ast"))
  new_ast("where", name = name, content = content, hash = hash, ...)
}

# coercion functions ####
ast_func_to_sum <- function(x) {
  # Convert a function node to a sum node
  stopifnot(inherits(x, c("ast", "func")))
  stopifnot(x$name == "sum")
  ast_sum(index = x$index, value = x$value)
}


ast_func_to_prod <- function(x) {
  # Convert a function node to a product node
  stopifnot(inherits(x, c("ast", "func")))
  stopifnot(x$name == "prod")
  ast_prod(index = x$index, value = x$value)
}

# functions ####

#' Get the type of an AST or multimod node
#'
#' This function retrieves the class of an AST node or multimod object that
#' represents a specific type of node in the abstract syntax tree.
#'
#' @param x An `ast` or `multimod` object. In case of other classes, it will
#' return `NULL`.
#'
#' @return A character string representing the type of the node
#' (e.g., `"expression"`, `"variable"`, `"parameter"`, `"sum"`, etc.).
#' @export
node_type <- function(x) {
  if (!inherits(x, c("ast", "multimod"))) {
    return(NULL)
  }
  class(x)[1]
}

#' Generate a stable hash for an AST or multimod object (excluding internal hash fields)
#'
#' @param node An `ast` object.
#' @param algo Hashing algorithm. If NULL, uses `options("multimod.hash_algo")`.
#'
#' @return A character hash (e.g., "42fca0dd").
#' @export
#' @examples
#' ast <- ast_expression("+", ast_variable("x"), ast_constant(5))
#' hash <- node_hash(ast)
node_hash <- function(node, algo = NULL) {

  stopifnot(inherits(node, c("ast", "multimod")))

  if (is.null(algo)) {
    algo <- getOption("multimod.hash_algo", default = "crc32")
  }

  # Recursive copy of AST, dropping any `$hash` fields
  strip_hash_field <- function(obj) {
    if (inherits(obj, "ast")) {
      obj$hash <- NULL
      obj <- lapply(obj, strip_hash_field)
      class(obj) <- class(node)
    } else if (is.list(obj)) {
      obj <- lapply(obj, strip_hash_field)
    }
    obj
  }

  ast_clean <- strip_hash_field(node)

  # Generate digest
  digest::digest(ast_clean, algo = algo)
}



#' Create a function call AST node
#'
#' Constructs an AST node representing a function call with arguments.
#' This is similar to ast_func but specifically for function calls in expressions.
#'
#' @param name Character string, function name
#' @param args List of AST nodes representing arguments
#' @return An object of class `ast` and `call`
#' @export
ast_call <- function(name, args = list()) {
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.list(args))
  stopifnot(all(sapply(args, inherits, "ast")))
  new_ast("call", name = name, args = args)
}

#' Create a set minimum AST node
#'
#' Constructs an AST node representing a minimum over a set index.
#' This is distinct from numeric min() functions.
#'
#' @param index An AST node representing the index set
#' @param value An AST node representing the value expression
#' @return An object of class `ast` and `setmin`
#' @export
ast_setmin <- function(index, value) {
  stopifnot(inherits(index, "ast"))
  stopifnot(inherits(value, "ast"))
  new_ast("setmin", index = index, value = value)
}

#' Create a set maximum AST node
#'
#' Constructs an AST node representing a maximum over a set index.
#' This is distinct from numeric max() functions.
#'
#' @param index An AST node representing the index set
#' @param value An AST node representing the value expression
#' @return An object of class `ast` and `setmax`
#' @export
ast_setmax <- function(index, value) {
  stopifnot(inherits(index, "ast"))
  stopifnot(inherits(value, "ast"))
  new_ast("setmax", index = index, value = value)
}

# ========================================================================== #
# Helper utilities for AST parser selection

#' Resolve the parsing language for AST helpers
#'
#' @param language Explicit language override (e.g., "gams", "gmpl").
#' @param symbols Symbol table or similar object that may carry a `language` field/attribute.
#' @param context Optional object (model structure, builder) that exposes `$language` or a
#'   `language` attribute.
#' @keywords internal
resolve_ast_language <- function(language = NULL, symbols = NULL, context = NULL) {
  if (!is.null(language)) {
    candidate <- language
  } else {
    candidate <- first_non_null(
      extract_language(context),
      extract_language(symbols)
    )
  }

  if (is.null(candidate)) {
    stop("Parser language is not defined. Pass `language`, or attach it to `symbols`/`context` via $language or metadata$source_language.")
  }

  lang <- as.character(candidate)[1]
  if (is.na(lang) || !nzchar(lang)) {
    stop("Parser language is empty. Provide an explicit language identifier (e.g., 'gams', 'gmpl').")
  }

  tolower(lang)
}

# Return first non-null element
first_non_null <- function(...) {
  args <- list(...)
  for (arg in args) {
    if (!is.null(arg) && length(arg) > 0) {
      return(arg)
    }
  }
  NULL
}

extract_language <- function(obj) {
  if (is.null(obj)) return(NULL)

  if (!is.null(obj$language)) {
    return(obj$language)
  }

  if (!is.null(obj$metadata)) {
    metadata_lang <- obj$metadata$source_language %||% obj$metadata$language
    if (!is.null(metadata_lang)) {
      return(metadata_lang)
    }
  }

  attr(obj, "language", exact = TRUE)
}

normalize_ast_language <- function(language) {
  if (is.null(language)) return("gams")
  lang <- tolower(language)
  if (lang %in% c("gmpl", "glpk", "mathprog")) {
    return("gmpl")
  }
  lang
}

get_ast_parser <- function(language) {
  lang <- normalize_ast_language(language)
  if (lang == "gams") {
    return(parse_gams_expr)
  } else if (lang == "gmpl") {
    return(parse_gmpl_expr)
  } else if (lang == "linopy") {
    stop("linopy AST parsing is not part of the package; ",
         "the experimental reader lives in drafts/R/read_linopy.R")
  }
  stop("No AST parser registered for language: ", language)
}

#' Language-aware AST expression parser
#'
#' @keywords internal
ast_parse_expr <- function(expr, symbols = NULL, language = NULL, context = NULL, ...) {
  if (inherits(expr, "ast")) return(expr)

  lang <- resolve_ast_language(language, symbols, context)
  parser <- get_ast_parser(lang)

  if (is.null(symbols)) {
    symbols <- list()
  }

  parser(expr, symbols = symbols, ...)
}

