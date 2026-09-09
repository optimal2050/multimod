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

  obj <- structure(
    list(
      name = name,
      desc = desc,
      subset_of = subset_of,
      data = data
    ),
    class = c("set", "multimod", "ast")
  )

  validate(obj, context = sprintf("set '%s'", name))
  obj
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
                                 auto_fold = FALSE,
                                 symbols = NULL) {
  stopifnot(is.character(name), length(name) == 1)
  # browser()
  obj <- structure(
    list(
      name = name,
      desc = desc,
      dims = ast_dims(dims, symbols = symbols),
      active_dims = ast_dims(active_dims, symbols = symbols),
      data = data
    ),
    class = c("mapping", "multimod", "ast")
  )

  validate(obj, context = sprintf("mapping '%s'", name))
  obj
}

#' Create a multimod parameter
#'
#' @param name character, name of the parameter
#' @param dims character vector, names of the declared dimensions
#' @param data data frame, data for the parameter
#' @param active_dims character vector, names of the active dimensions
#' @param comment character, comment line from source file (e.g., *@ domain hint)
#' @param auto_fold logical, whether to automatically fold dimensions
#'
#' @returns a parameter object
#' @export
new_parameter <- function(
    name,
    desc = NULL,
    dims,
    active_dims = NULL,
    data = NULL,
    defVal = NULL,
    defInt = NULL,
    symbolic = FALSE,
    formula = NULL,
    comment = NULL,
    auto_fold = FALSE,
    symbols = NULL) {
  if (is.null(active_dims)) {
    if (auto_fold) {
      # dims_folded <- fold_param_dims(data, dims)
      # active_dims <- dims_folded$active_dims
    } else {
      active_dims <- setdiff(names(data), "value")
    }
  }

  obj <- structure(
    list(
      name = name,
      desc = desc,
      dims = ast_dims(dims, symbols = symbols),
      active_dims = ast_dims(active_dims, symbols = symbols),
      data = data,
      defVal = defVal,
      defInt = defInt,
      symbolic = symbolic,
      formula = formula,
      comment = comment
    ),
    class = c("parameter", "multimod", "ast")
  )

  validate(obj, context = sprintf("parameter '%s'", name))
  obj
}

#' Create a multimod variable
#'
#' @param name character, name of the variable
#' @param dims character vector, names of the declared dimensions
#' @param data data frame, data for the variable
#' @param active_dims character vector, names of the active dimensions
#' @param domain character, domain mapping name for sparse indexing (NULL = Cartesian, character(0) = unused)
#' @param comment character, comment line from source file (e.g., *@ domain hint)
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
    vtype = NULL,
    bounds = NULL,
    comment = NULL,
    data = NULL, # mapping parameter/set
    # domain = "continuous",
    auto_fold = FALSE,
    symbols = NULL) {
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
  obj <- structure(
    list(
      name = name,
      desc = desc,
      dims = ast_dims(dims, symbols = symbols),
      active_dims = ast_dims(active_dims, symbols = symbols),
      # domain = ast_mapping(active_dims),
      domain = domain,
      vtype = vtype,
      bounds = bounds,
      comment = comment,
      data = as.data.frame(data)
    ),
    class = c("variable", "multimod", "ast")
  )

  validate(obj, context = sprintf("variable '%s'", name))
  obj
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
#' @param comment character, comment line from source file (e.g., *@ domain hint)
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
    domain = NULL,
    comment = NULL,
    dims_index_aliases = NULL,
    symbols = NULL) {
  # browser()
  stopifnot(relation %in% c("==", "<=", ">="))
  # if (!inherits(rhs, "expression")) browser()
  stopifnot(inherits(lhs, "ast"), inherits(rhs, "ast"))

  # if (name == "eqTechSng2Grp") browser()

  obj <- structure(
    list(
      name = name,
      desc = desc,
      dims = ast_dims(dims, symbols = symbols),
      domain = domain,
      comment = comment,
      lhs = lhs,
      relation = relation,
      rhs = rhs,
      dims_index_aliases = dims_index_aliases  # Store equation-specific iterator vars
    ),
    class = c("equation", "multimod", "ast")
  )

  validate(obj, context = sprintf("equation '%s'", name))
  obj
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
    objectives = list(),  # List of objective metadata (can have multiple)
    models = list(),      # List of model definitions: list(name = c(equation_names))
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
      objectives = objectives,  # Plural - can have multiple objectives
      models     = models,      # Track model definitions and solve statements
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
#' @param desc model description (character)
#' @param metadata Named list with auxiliary information (e.g., language, source_file, data_source).
#'
#' @returns A model object
#' @export
new_model <- function(
    name = NULL,
    desc = NULL,
    sets = list(),
    aliases = list(),
    mappings = list(),
    parameters = list(),
    variables = list(),
    equations = list(),
    inMemory = TRUE,
    base_path = NULL,
    metadata = list(),
    ...
) {
  metadata <- metadata %||% list()
    if (!is.null(metadata$language) && is.null(metadata$source_language)) {
      metadata$source_language <- metadata$language
    }
    metadata$language <- NULL

  model <- structure(
    list(
      name = name, # Name of the model (optional)
      desc = desc, # Description of the model (optional)
      sets = sets, # Named character vectors (optional descriptions)
      aliases = aliases, # Named list of character vectors (optional descriptions)
      mappings = mappings, # Named list of mapping sets
      parameters = parameters, # Named list of parameter
      variables = variables, # Named list of variable
      equations = equations, # Named list of equation
      inMemory = inMemory, # Default: all data in memory
      base_path = base_path, # Base path for relative data paths
      metadata = metadata,
      ...
    ),
    class = c("model", "multimod")
  )

  if (!is.null(metadata$source_language)) {
    attr(model, "language") <- metadata$source_language
  }

  model
}



