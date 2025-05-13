#' Convert an object to a multimod structure
#'
#' This is a generic function that converts an object to a multimod-compatible format.
#'
#' @param x An object to convert.
#' @param ... Additional arguments passed to methods.
#'
#' @return A multimod object.
#' @export
as_multimod <- function(x, ...) {
  UseMethod("as_multimod")
}

#' @export
as_multimod.default <- function(x, ...) {
  stop("No as_multimod method for object of class: ", class(x))
}



#' @export
as_multimod.model_structure <- function(x, ...) {
  stopifnot(inherits(x, "model_structure"))

  args <- list(...)

  name <- args$name %||% x$name
  desc <- args$desc %||% x$desc

  # Build symbol list for expression parsing
  symbols <- build_symbols_list(x)

  # Convert sets and aliases
  sets <- lapply(x$sets, function(s) {
    new_multimod_set(name = s$name, desc = s$desc)
  })
  aliases <- x$aliases # assumed to be list of character pairs

  # Convert mappings
  mappings <- lapply(x$mappings, function(m) {
    new_multimod_mapping(
      name = m$name,
      desc = m$desc,
      dims = m$dims,
      data = if (!is.null(m$data)) m$data else NULL # optional
    )
  })

  # Convert parameters
  parameters <- lapply(x$parameters, function(p) {
    new_multimod_parameter(
      name = p$name,
      desc = p$desc,
      dims = p$dims,
      data = if (!is.null(p$data)) p$data else NULL # optional
    )
  })

  # Convert variables
  variables <- lapply(x$variables, function(v) {
    new_multimod_variable(
      name = v$name,
      desc = v$desc,
      dims = v$dims
    )
  })

  # Convert equations with error capture
  equations <- list()
  for (eq_name in names(x$equations)) {
    eqn_info <- x$equations[[eq_name]]
    eqn <- tryCatch(
      {
        coerce_gams_equation(eqn_info, symbols)
      },
      error = function(e) {
        stop(sprintf("Failed to convert equation '%s': %s", eq_name, e$message))
        # NULL
      }
    )
    if (!is.null(eqn)) {
      equations[[eq_name]] <- eqn
    }
  }

  new_multimod_model(
    name = name,
    desc = desc,
    sets = sets,
    aliases = aliases,
    mappings = mappings,
    parameters = parameters,
    variables = variables,
    equations = equations
  )
}
