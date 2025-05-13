
# internal functions ####

build_symbols_list <- function(model_info) {
  list(
    sets = names(model_info$sets), # !!! Add aliases
    mappings = names(model_info$mappings),
    parameters = names(model_info$parameters),
    variables = names(model_info$variables)
  )
}

detect_symbol_type <- function(
    name, symbols,
    known_funcs = c("log", "exp", "abs", "sqrt"),
    default = "symbol"
    ) {
  if (!is.null(symbols$variables) && name %in% symbols$variables) {
    return("variable")
  } else if (!is.null(symbols$parameters) && name %in% symbols$parameters) {
    return("parameter")
  } else if (!is.null(symbols$mappings) && name %in% symbols$mappings) {
    # browser()
    return("mapping")
  } else if (!is.null(symbols$sets) && name %in% symbols$sets) {
    return("set")
  } else if (name %in% known_funcs) {
    return("func")
  } else {
    return(default) # fallback (could also be "unknown")
  }
}

