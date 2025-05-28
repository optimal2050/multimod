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
  dims <- list(...)
  if (length(dims) == 0 || (length(dims) == 1 && is_empty(dims[[1]]))) {
    # empty
    return(new_ast("dims"))
  }
  # check if ... is an unnamed list of objects
  if (length(dims) == 1 && is.null(names(dims)) &&
      !inherits(dims, "ast") &&
      (is.list(dims[[1]]) || is.vector(dims[[1]]))
      ) {
    dims <- dims[[1]]
  }

  cls <- sapply(dims, function(x) class(x)[1])

  if (all(cls %in% c("set", "symbol"))) {
    # pass
  } else if (all(cls %in% "character")) {
    # Convert character to set
    dims <- lapply(dims, ast_set)
  } else {
    stop("Invalid input for `dims`: ",
         "must be set, symbol, or characters.")
  }
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
ast_variable <- function(name, dims = ast_dims()) {
  stopifnot(is.character(name))
  new_ast("variable", name = name, dims = dims)
}

#' Create a parameter AST node
#'
#' @param name Name of the parameter.
#' @param dims Optional vector of dimension `dims` objects.
#'
#' @return An `parameter` S3 object (subclass of `ast`).
#' @export
ast_parameter <- function(name, dims = ast_dims()) {
  stopifnot(is.character(name))
  new_ast("parameter", name = name, dims = dims)
}

#' Create a symbol AST node (for unclassified identifiers)
#'
#' @param name A character string representing the symbol.
#'
#' @return An `symbol` S3 object (subclass of `ast`).
#' @export
ast_symbol <- function(name, ...) {
  stopifnot(is.character(name))
  new_ast("symbol", name = name, ...)
}

#' Create a constant AST node
#'
#' @param value A numeric or character constant.
#'
#' @return An `constant` S3 object (subclass of `ast`).
#' @export
ast_constant <- function(value, ...) {
  stopifnot(is.numeric(value) || is.character(value))
  new_ast("constant", value = value)
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

  new_ast("when", condition = condition, then = then, otherwise = otherwise)
}

#' Create a summation AST node
#'
#' Constructs an abstract syntax tree (AST) node representing a summation over an
#' index. The index is typically a `set`, `dims` or `when` object if
#' filtering is applied to the index.
#'
#' @param index Character. The index variable (e.g., `"t"`).
#' @param value An AST node representing the expression to be summed.
#'
#' @return An object of class `ast` and `sum`.
#' @export
ast_sum <- function(index = ast_dims(), value) {
  stopifnot(inherits(index, "ast"))
  # stopifnot(inherits(domain, "ast") || is.null(domain))
  stopifnot(inherits(value, "ast"))
  # if (!is.null(domain)) stopifnot(inherits(domain, "ast"))
  new_ast("sum", index = index, value = value)
}
# @param domain An optional AST node (class `ast`) representing a domain condition.
#   This can be a mapping, a logical condition (e.g., from a `$`-filter), or a parameter.
#   Use `NULL` if there is no restriction.


#' Create a product AST node
#'
#' Constructs an abstract syntax tree (AST) node representing a product over an
#' index. The index is typically a `set`, `dims` or `when` object if
#' filtering is applied to the index.
#'
#' @inheritParams ast_sum
#' @return An object of class `ast` and `prod`.
#' @export
ast_prod <- function(index, value) {
  stopifnot(inherits(index, "ast"))
  # if (!is.null(domain)) stopifnot(inherits(domain, "ast"))
  stopifnot(inherits(value, "ast"))
  new_ast("prod", index = index, value = value)
}

#' Create a function AST node
#'
#' Constructs an abstract syntax tree (AST) node representing a function.
#' @param name A character string representing the function name.
#' @param value An AST node representing the function body or expression.
#' @return An `ast` object of class `function`.
#' @export
ast_func <- function(name, value) {
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(inherits(value, "ast"))
  new_ast("func", name = name, value = value)
}

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
#' @param ...
#'
#' @returns
#' @export
ast_where <- function(name, content, hash = node_hash(content), ...) {
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  stopifnot(is.null(content) || inherits(content, "ast"))
  new_ast("where", name = name, content = content, hash = hash, ...)
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


