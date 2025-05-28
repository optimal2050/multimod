#' Create a new set object
#'
#' @param name Character. Name of the set.
#' @param desc Character. Description of the set (optional).
#' @param subset_of Character vector. Name of the parent set (optional).
#' @param data Optional values (e.g., character vector or data.frame).
#' @return An object of class `set`
#' @export
new_set <- function(name, desc = NULL, subset_of = NULL,
                             data = NULL) {
  stopifnot(is.character(name), length(name) == 1)

  structure(
    list(
      name = name,
      desc = desc,
      subset_of = subset_of,
      data = data
    ),
    class = c("set", "multimod", "ast")
  )
}

#' Create a multimod mapping object
#'
#' @param name character, name of the mapping
#' @param desc character, description of the mapping
#' @param subset_of character, name of the parent mapping
#' @param dims character vector, names of the declared dimensions
#' @param data data frame, data for the mapping
#' @return An object of class `mapping`
#' @export
new_mapping <- function(name,
                                 desc = NULL,
                                 subset_of = NULL,
                                 dims = NULL,
                                 active_dims = NULL,
                                 data = NULL,
                                 auto_fold = FALSE) {
  stopifnot(is.character(name), length(name) == 1)
  # browser()
  structure(
    list(
      name = name,
      desc = desc,
      dims = ast_dims(dims),
      active_dims = ast_dims(active_dims),
      data = data
    ),
    class = c("mapping", "multimod", "ast")
  )
}

#' Create a multimod parameter
#'
#' @param name character, name of the parameter
#' @param dims character vector, names of the declared dimensions
#' @param data data frame, data for the parameter
#' @param active_dims character vector, names of the active dimensions
#' @param auto_fold logical, whether to automatically fold dimensions
#'
#' @returns a parameter object
#' @export
#'
#' @examples
new_parameter <- function(
    name,
    desc = NULL,
    dims,
    active_dims = NULL,
    data = NULL,
    auto_fold = FALSE) {
  if (is.null(active_dims)) {
    if (auto_fold) {
      # dims_folded <- fold_param_dims(data, dims)
      # active_dims <- dims_folded$active_dims
    } else {
      active_dims <- setdiff(names(data), "value")
    }
  }

  structure(
    list(
      name = name,
      desc = desc,
      dims = ast_dims(dims),
      active_dims = ast_dims(active_dims),
      data = data
    ),
    class = c("parameter", "multimod", "ast")
  )
}

#' Create a multimod variable
#'
#' @param name character, name of the variable
#' @param dims character vector, names of the declared dimensions
#' @param data data frame, data for the variable
#' @param active_dims character vector, names of the active dimensions
#' @param domain character, domain of the variable (e.g., "continuous", "integer", "binary")
#' @param auto_fold logical, whether to automatically fold dimensions
#'
#' @returns a variable object
#' @export
new_variable <- function(
    name,
    desc = NULL,
    dims,
    active_dims = NULL,
    domain = NULL,
    data = NULL, # mapping parameter/set
    # domain = "continuous",
    auto_fold = FALSE) {
  # stopifnot(domain %in% c("continuous", "integer", "binary"))

  if (is.null(active_dims)) {
    if (auto_fold) {
      # dims_folded <- fold_param_dims(data, dims)
      # active_dims <- dims_folded$active_dims
    } else {
      active_dims <- dims
    }
  }
  # message(name)
  structure(
    list(
      name = name,
      desc = desc,
      dims = ast_dims(dims),
      active_dims = ast_dims(active_dims),
      # domain = ast_mapping(active_dims),
      domain = domain,
      data = as.data.frame(data)
    ),
    class = c("variable", "multimod", "ast")
  )
}

#' Create a multimod equation object
#'
#' Constructs an equation object used in the `multimod` modeling framework.
#' This object represents a single equation, including left-hand side (LHS),
#' right-hand side (RHS), relation operator (e.g., equality or inequality), and an optional domain.
#'
#' @param name Character string. The name of the equation.
#' @param desc Optional character string. A description or label for the equation.
#' @param dims Character vector. Names of the dimensions over which the equation is declared.
#' @param lhs An AST (abstract syntax tree) representing the left-hand side of the equation.
#' @param rhs An AST representing the right-hand side of the equation.
#' @param relation Character string. The relation type: one of `"=="`, `"<="`, or `">="`.
#' @param domain Optional AST or symbol representing the domain/mapping condition for the equation.
#'
#' @returns An object of class `equation`, containing the parsed equation structure.
#'
#' @export
#'
#' @examples
#' lhs <- ast_variable("vTechOut", dims = c("tech", "region"))
#' rhs <- ast_expression("*", param("pTechEff", dims = c("tech")), ast_variable("vTechInp", dims = c("tech", "region")))
#' eq <- new_equation(
#'   name = "eqTechEff",
#'   desc = "Technology output efficiency",
#'   dims = c("tech", "region"),
#'   lhs = lhs,
#'   rhs = rhs,
#'   relation = "=="
#' )
#' print(eq)
new_equation <- function(
    name,
    desc = NULL,
    dims,
    lhs,
    rhs,
    relation = "==",
    domain = NULL) {
  # browser()
  stopifnot(relation %in% c("==", "<=", ">="))
  # if (!inherits(rhs, "expression")) browser()
  stopifnot(inherits(lhs, "ast"), inherits(rhs, "ast"))

  # if (name == "eqTechSng2Grp") browser()

  structure(
    list(
      name = name,
      desc = desc,
      dims = ast_dims(dims),
      domain = domain,
      lhs = lhs,
      relation = relation,
      rhs = rhs
    ),
    class = c("equation", "multimod", "ast")
  )
}

new_model_structure <- function(
    name = NULL,
    desc = NULL,
    sets = list(),
    mappings = list(),
    aliases = list(),
    parameters = list(),
    variables = list(),
    equations = list(),
    source = NULL,
    language = NULL
) {
  structure(
    list(
      name       = name,
      desc       = desc,
      sets       = sets,
      mappings   = mappings,
      aliases    = aliases,
      parameters = parameters,
      variables  = variables,
      equations  = equations,
      source     = source, # File path or string
      # source     = tryCatch(normalizePath(source, winslash = "/"),
      #                       error = function(e) {NULL}),
      language   = language # e.g., "GAMS", "Pyomo", "JuMP"
    ),
    class = "model_structure"
  )
}

#' Create a multimod model object
#'
#' @param sets named list of sets
#' @param mappings named list of mappings
#' @param parameters named list of parameter objects
#' @param variables named list of variable objects
#' @param equations named list of equation objects
#' @param desc
#'
#' @returns
#' @export
#'
#' @examples
new_model <- function(
    name = NULL,
    desc = NULL,
    sets = list(),
    aliases = list(),
    mappings = list(),
    parameters = list(),
    variables = list(),
    equations = list()
) {
  structure(
    list(
      name = name, # Name of the model (optional)
      desc = desc, # Description of the model (optional)
      sets = sets, # Named character vectors (optional descriptions)
      aliases = aliases, # Named list of character vectors (optional descriptions)
      mappings = mappings, # Named list of mapping sets
      parameters = parameters, # Named list of parameter
      variables = variables, # Named list of variable
      equations = equations # Named list of equation
    ),
    class = c("model", "multimod")
  )
}



