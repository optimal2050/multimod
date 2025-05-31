
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
    known_func_indexed = c("sum", "prod", "smin", "smax", "sand", "sor"),
    default = "symbol",
    rename = FALSE
) {
  lname <- tolower(name)

  check_case <- function(x, type) {
    if (is.null(x)) return(FALSE)
    x_lower <- tolower(x)
    match_idx <- match(lname, x_lower, nomatch = 0)
    if (match_idx > 0) {
      if (name != x[match_idx]) {
        warning(sprintf("Case inconsistency detected for %s '%s' vs '%s'", type, name, x[match_idx]))
        if (rename) {
          message(sprintf("Renaming '%s' to '%s'", name, x[match_idx]))
          name <<- x[match_idx]
        }
      }
      return(TRUE)
    }
    return(FALSE)
  }

  if (check_case(symbols$variables, "variable")) {
    return("variable")
  } else if (check_case(symbols$parameters, "parameter")) {
    return("parameter")
  } else if (check_case(symbols$mappings, "mapping")) {
    return("mapping")
  } else if (check_case(symbols$sets, "set")) {
    return("set")
  } else if (lname %in% tolower(known_func_indexed)) {
    return("func_indexed")
  } else if (lname %in% tolower(known_funcs)) {
    return("func")
  } else {
    return(default)
  }
}


known_gams_functions <- list(
  abs = 1, acos = 1, asin = 1, atan = 1, cos = 1,
  exp = 1, log = 1, log10 = 1, sin = 1, sqrt = 1, tan = 1,
  ceil = 1, floor = 1, round = 1, sign = 1, mod = 2, power = 2,
  min = 2, max = 2,
  arctan = 1, arccos = 1, arcsin = 1, cosh = 1, sinh = 1, tanh = 1,
  acosh = 1, asinh = 1, atanh = 1, erf = 1, erfc = 1, gamma = 1,
  ln = 1, log2 = 1, trunc = 1
)


#' Infer variable type (vtype) from lower/upper bounds
#'
#' @param lo Lower bound (numeric or NULL)
#' @param up Upper bound (numeric or NULL)
#' @param integer Logical; is the variable declared integer?
#' @return A character string: "free", "positive", "binary", "integer", or NULL
#' @noRd
infer_vtype_from_bounds <- function(lo = NULL, up = NULL, integer = FALSE) {
  is_zero <- function(x) isTRUE(all.equal(x, 0))
  is_one <- function(x) isTRUE(all.equal(x, 1))
  is_pos_inf <- function(x) is.null(x) || isTRUE(x == Inf)

  if (is_zero(lo) && is_one(up)) {
    return("binary")
  } else if (is_zero(lo) && is_pos_inf(up)) {
    return("positive")
  } else if (integer) {
    return("integer")
  } else if (is.null(lo) && is.null(up)) {
    return("free")
  } else {
    return(NULL)  # ambiguous or custom-bounded
  }
}

