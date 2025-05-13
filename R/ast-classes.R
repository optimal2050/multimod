#' Create a new multimod AST (Abstract Syntax Tree) node
#'
#' This is a generic constructor for all `multimod_ast` node types,
#' used internally by the `ast_*()` functions to define structured equations.
#'
#' @param type The type of AST node (e.g., `"expression"`, `"variable"`, `"constant"`, `"parameter"`, `"symbol"`).
#' @param ... Additional named fields passed as components of the AST node.
#'
#' @return A structured S3 object of class `multimod_ast`, with a subclass matching `type`.
#' @export
new_ast <- function(type, ..., inherits_class = NULL) {
  stopifnot(is.character(type))
  stopifnot(length(type) == 1)
  stopifnot(is.null(inherits_class) || !is.na(inherits_class) || is.character(inherits_class))
  structure(list(type = type, ...),
            class = c(paste0("ast_", type), inherits_class, "multimod_ast"))
}

new_multimod_ast <- new_ast

#' Create an AST node representing a set
#'
#' Constructs a set object used as an index domain for other symbols
#' (e.g., mappings, parameters, variables, equations).
#'
#' @param name Character. Name of the set (e.g., "b").
#'
#' @return An object of class `multimod_ast` and `ast_set`
#'
#' @export
#' @rdname new_multimod_ast
#' @examples
#' ast_set("b")                     # standalone set
#' ast_set("b", domain = ast_set("a"))  # subset declaration b ⊆ a
ast_set <- function(name) {
  stopifnot(is.character(name), length(name) == 1)
  # if (!is.null(domain)) stopifnot(inherits(domain, "multimod_ast") && domain$type == "set")
  new_multimod_ast("set", name = name)
}
# @param domain Optional. Another AST set node representing the parent set
# (e.g., "a" in b(a)), indicating that this set is a subset of the parent set.



# Create an AST node for a list of index symbols
#
# Constructs an `ast_index` object, which represents a list of index symbols
# used in a indexed function or expression. Each index is typically represented
# as a set reference (`ast_set`) or as a symbolic identifier (`ast_symbol`).
# Similar to `ast_dims`, but specifically for indexing.
#
# @param sets A list of AST nodes (e.g., ast_set or ast_symbol), one for each index
#
# @return An `ast_index` object (subclass of `multimod_ast`)
# @export
# ast_index <- function(sets) {
#   stopifnot(is.list(sets))
#   lapply(sets, function(x) {
#     stopifnot(inherits(x, "multimod_ast"))
#   })
#   new_multimod_ast("index", symbols = symbols)
# }

#' Create an AST node for dimensions (dims) of a symbol
#'
#' Constructs an `ast_dims` object, which represents the declared dimensions
#' of a variable, parameter, or equation in a structured symbolic model. Each
#' dimension is typically represented as a set reference (`ast_set`) or as a
#' symbolic identifier (`ast_symbol`). This is similar to `ast_index`, but
#' specifically for dimensions.
#'
#' @param ... One or more dimension expressions, typically created using
#'   [ast_set()] or [ast_symbol()]. Expected AST objects or character strings.
#'   If character strings are provided, they will be converted to `ast_symbol` objects.
#'
#' @return A list of class `ast_dims` and `multimod_ast`, representing the dimension nodes.
#'
#' @export
#'
#' @examples
#' ast_dims(list(ast_set("tech"), ast_set("region")))
#' ast_dims(ast_symbol("t"))
ast_dims <- function(dims = list()) {
  # browser()
  # args <- list(...)
  # if (is_empty(args) && is_empty(dims)) {
  #   stop("Either `...` or `dims` must be provided.")
  # } else if (!is_empty(dims)) {
  if (inherits(dims, c("ast_set", "ast_symbol"))) {
    # Convert to list if a single object is provided
    dims <- list(dims)
  } else if (is.character(dims)) {
    # Convert character to ast_set
    dims <- lapply(dims, ast_set)
  } else if (is.list(dims)) {
    # Check if all elements are ast_set or ast_symbol
    stopifnot(all(sapply(dims, function(x) inherits(x, c("ast_set", "ast_symbol")))))
  } else {
    stop("Invalid input for `dims`: must be a list of ast_set or ast_symbol.")
  }
  # stopifnot(is.list(dims))
    # stopifnot(length(dims) > 0)
    # stopifnot(is_empty(args))
  # } else {
  #   stopifnot(length(args) > 0)
  #   # stopifnot(all(sapply(args, function(x) inherits(x, "multimod_ast"))))
  #   dims <- args
  # }
  # if (is.null(dims$inherits_class)) {
  #   inherits_class <- "ast_dims"
  # } else {
  #   inherits_class <- c(as.character(dims$inherits_class), "ast_dims")
  #   dims$inherits_class <- NULL
  # }
  # class_check <- sapply(dims, inherits, c("ast_set", "ast_symbol"))
  # if (any(!class_check)) {
  #   browser()
  #   stop("All elements in `dims` must be of class 'ast_set'.")
  # }
  new_multimod_ast("dims", dims = dims)
}

as_dims <- function(x, ...) {
  UseMethod("as_dims", x)
}

as_dims.default <- function(x, ...) {
  stop("No as_dims method for object of class: ", class(x))
}

as_dims.character <- function(x, ...) {
  # Convert character to ast_symbol
  ast_dims(dims = lapply(x, ast_set))
}

as_dims.list <- function(x, ...) {

  dims <- lapply(x, function(y) {
    if (inherits(y, "ast_set")) {
      return(y)
    } else if (is.character(y)) {
      return(ast_set(y))
    } else if (inherits(y, "ast_symbol")) {
      return(ast_set(y$name))
    } else {
      stop("Invalid element in list: must be ast_set or character.")
    }
  })

  return(ast_dims(dims = dims))
}

#' Create a mapping AST node
#'
#' Constructs a mapping node representing a mapping or set of indices.
#'
#' @param name A character string representing the mapping name.
#' @param dims An `ast_dims` object representing the dimensions of the mapping.
#'
#' @return An `ast_mapping` S3 object (subclass of `multimod_ast`).
#' @export
ast_mapping <- function(name, dims = ast_dims(), domain = NULL, ...) {
  stopifnot(is.character(name))
  # stopifnot(is.character(dims))
  stopifnot(length(dims) > 0)
  # if (!is.null(domain)) {
  #   stopifnot(inherits(domain, "multimod_ast") && domain$type == "mapping")
  # }
  new_multimod_ast("mapping", name = name, dims = dims, domain = domain, ...)
}
# @param domain An optional `ast_mapping` object representing the parent mapping,
# indicating that this mapping is a subset of the parent mapping.



#' Create a variable AST node
#'
#' @param name Name of the variable.
#' @param dims Optional vector of dimension `ast_dims` objects.
#'
#' @return An `ast_variable` S3 object (subclass of `multimod_ast`).
#' @export
ast_variable <- function(name, dims = ast_dims()) {
  stopifnot(is.character(name))
  new_multimod_ast("variable", name = name, dims = dims)
}

#' Create a parameter AST node
#'
#' @param name Name of the parameter.
#' @param dims Optional vector of dimension `ast_dims` objects.
#'
#' @return An `ast_parameter` S3 object (subclass of `multimod_ast`).
#' @export
ast_parameter <- function(name, dims = ast_dims()) {
  stopifnot(is.character(name))
  new_multimod_ast("parameter", name = name, dims = dims)
}

#' Create a symbol AST node (for unclassified identifiers)
#'
#' @param name A character string representing the symbol.
#'
#' @return An `ast_symbol` S3 object (subclass of `multimod_ast`).
#' @export
ast_symbol <- function(name) {
  stopifnot(is.character(name))
  new_multimod_ast("symbol", name = name)
}

#' Create a constant AST node
#'
#' @param value A numeric or character constant.
#'
#' @return An `ast_constant` S3 object (subclass of `multimod_ast`).
#' @export
ast_constant <- function(value) {
  stopifnot(is.numeric(value) || is.character(value))
  new_multimod_ast("constant", value = value)
}

#' Create an expression AST node
#'
#' Constructs a binary operation node representing an expression such as addition, multiplication, etc.
#'
#' @param op A character string representing the operator (e.g., `+`, `*`, `/`).
#' @param left The left-hand side AST node.
#' @param right The right-hand side AST node.
#'
#' @return An `ast_expression` S3 object (subclass of `multimod_ast`).
#' @export
ast_expression <- function(op, left, right) {
  stopifnot(is.character(op), !is.null(left), !is.null(right))
  stopifnot(inherits(left, "multimod_ast"), inherits(right, "multimod_ast"))
  new_multimod_ast("expression", op = op, left = left, right = right)
}

#' Create a unary expression AST node
#'
#' Represents a unary operator such as "-" or "not" applied to a single argument.
#'
#' @param op Character string. The unary operator, e.g., "-" or "not".
#' @param rhs The operand (a `multimod_ast` object).
#'
#' @return An object of class `multimod_ast` and `ast_unary`.
#' @export
#' @examples
#' ast <- ast_unary("-", ast_symbol("x"))
#' str(ast)
#' ast_unary("not", ast_symbol("x"))
ast_unary <- function(op, rhs) {
  stopifnot(is.character(op), length(op) == 1)
  stopifnot(inherits(rhs, "multimod_ast"))
  # Check if the operator is valid
  valid_operators <- c("-", "not", "+", "!")
  if (!op %in% valid_operators) {
    stop("Invalid unary operator: ", op,
         "\nValid operators: ", paste(valid_operators, collapse = ", "))
  }
  # Create the unary AST node
  new_ast("unary", op = op, rhs = rhs)
  # new_ast_unary(op, rhs)
}

#' Construct a conditional (dollar) expression node for multimod AST
#'
#' This function constructs a conditional expression node of type `"condition"`,
#' representing GAMS-style conditional terms using the `$` operator.
#'
#' @param condition The condition to check (right-hand side of `$`). Must be an AST node.
#' @param then The expression to evaluate if the condition is true (usually the left-hand side).
#'
#' @return An object of class `multimod_ast` and subclass `ast_condition`
#' @export
#'
#' @examples
#' ast_condition(
#'   condition = ast_symbol("i_active(i)"),
#'   then = ast_variable("x", c("i"))
#' )
ast_condition <- function(condition, then) {
  stopifnot(inherits(then, "multimod_ast") || is.null(then))
  stopifnot(inherits(condition, "multimod_ast"))

  new_multimod_ast("condition", then = then, condition = condition)
}

#' Create a summation AST node
#'
#' Constructs an abstract syntax tree (AST) node representing a summation over an index,
#' optionally restricted by a domain condition (such as a mapping, logical condition, or parameter).
#'
#' @param index Character. The index variable (e.g., `"t"`).
#' @param domain An optional AST node (class `multimod_ast`) representing a domain condition.
#'   This can be a mapping, a logical condition (e.g., from a `$`-filter), or a parameter.
#'   Use `NULL` if there is no restriction.
#' @param value An AST node representing the expression to be summed.
#'
#' @return An object of class `multimod_ast` and `ast_sum`.
#' @export
ast_sum <- function(index = ast_dims(), domain = NULL, value) {
  stopifnot(inherits(index, "ast_dims"))
  stopifnot(inherits(domain, "multimod_ast") || is.null(domain))
  stopifnot(inherits(value, "multimod_ast"))
  if (!is.null(domain)) stopifnot(inherits(domain, "multimod_ast"))
  new_multimod_ast("sum", index = index, domain = domain, value = value)
}

#' Create a product AST node
#'
#' Constructs an abstract syntax tree (AST) node representing a product over an index,
#' optionally restricted by a domain condition (such as a mapping, logical condition, or parameter).
#'
#' @inheritParams ast_sum
#' @return An object of class `multimod_ast` and `ast_prod`.
#' @export
ast_prod <- function(index, domain = NULL, value) {
  stopifnot(inherits(index, "multimod_ast"))
  if (!is.null(domain)) stopifnot(inherits(domain, "multimod_ast"))
  stopifnot(inherits(value, "multimod_ast"))
  new_multimod_ast("prod", index = index, domain = domain, value = value)
}



