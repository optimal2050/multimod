#' Convert AST to R Code
#'
#' @description
#' Converts AST nodes to executable R code. This is the foundation for
#' creating R functions from AST expressions.
#'
#' @param x An AST node (expression, symbol, constant, etc.)
#' @param ... Additional arguments passed to methods
#'
#' @return A character string containing R code
#'
#' @export
as_r <- function(x, ...) {
  UseMethod("as_r")
}

#' @export
as_r.symbol <- function(x, ...) {
  if (!is.null(x$dims) && length(x$dims) > 0) {
    # Symbol with indices: Name[i,j,k]
    indices_code <- sapply(x$dims, as_r, ...)
    paste0(x$name, "[", paste(indices_code, collapse = ", "), "]")
  } else {
    x$name
  }
}

#' @export
as_r.ast_symbol <- function(x, ...) {
  if (!is.null(x$dims) && length(x$dims) > 0) {
    # Symbol with indices: Name[i,j,k]
    indices_code <- sapply(x$dims, as_r, ...)
    paste0(x$name, "[", paste(indices_code, collapse = ", "), "]")
  } else {
    x$name
  }
}

#' @export
as_r.parameter <- function(x, ...) {
  if (!is.null(x$dims) && length(x$dims) > 0) {
    # Parameter with indices: Name[i,j,k]
    indices_code <- sapply(x$dims, as_r, ...)
    paste0(x$name, "[", paste(indices_code, collapse = ", "), "]")
  } else {
    x$name
  }
}

#' @export
as_r.ast_parameter <- function(x, ...) {
  if (!is.null(x$dims) && length(x$dims) > 0) {
    # Parameter with indices: Name[i,j,k]
    indices_code <- sapply(x$dims, as_r, ...)
    paste0(x$name, "[", paste(indices_code, collapse = ", "), "]")
  } else {
    x$name
  }
}

#' @export
as_r.constant <- function(x, ...) {
  deparse(x$value)
}

#' @export
as_r.ast_constant <- function(x, ...) {
  deparse(x$value)
}

#' @export
as_r.expression <- function(x, ...) {
  lhs_code <- as_r(x$lhs, ...)
  rhs_code <- as_r(x$rhs, ...)
  
  # Handle indexing operator specially
  if (x$op == "[") {
    return(paste0(lhs_code, "[", rhs_code, "]"))
  }
  
  # Handle binary operators
  if (x$op %in% c("+", "-", "*", "/", "^", "==", "!=", "<", ">", "<=", ">=", "&&", "||")) {
    return(paste0("(", lhs_code, " ", x$op, " ", rhs_code, ")"))
  }
  
  # Default: space-separated
  paste0(lhs_code, " ", x$op, " ", rhs_code)
}

#' @export
as_r.ast_expression <- function(x, ...) {
  lhs_code <- as_r(x$lhs, ...)
  rhs_code <- as_r(x$rhs, ...)
  
  # Handle indexing operator specially
  if (x$op == "[") {
    return(paste0(lhs_code, "[", rhs_code, "]"))
  }
  
  # Handle binary operators
  if (x$op %in% c("+", "-", "*", "/", "^", "==", "!=", "<", ">", "<=", ">=", "&&", "||")) {
    return(paste0("(", lhs_code, " ", x$op, " ", rhs_code, ")"))
  }
  
  # Default: space-separated
  paste0(lhs_code, " ", x$op, " ", rhs_code)
}

#' @export
as_r.unary <- function(x, ...) {
  rhs_code <- as_r(x$rhs, ...)
  paste0(x$op, rhs_code)
}

#' @export
as_r.ast_unary <- function(x, ...) {
  rhs_code <- as_r(x$rhs, ...)
  paste0(x$op, rhs_code)
}

#' @export
as_r.call <- function(x, ...) {
  args_code <- sapply(x$args, as_r, ...)
  paste0(x$name, "(", paste(args_code, collapse = ", "), ")")
}

#' @export
as_r.ast_call <- function(x, ...) {
  args_code <- sapply(x$args, as_r, ...)
  paste0(x$name, "(", paste(args_code, collapse = ", "), ")")
}

#' @export
as_r.setmin <- function(x, ...) {
  # Handle both old (index/value) and new (set/index/expr) structures
  if (!is.null(x$set)) {
    # New structure
    set_name <- x$set
    if (!is.null(x$expr)) {
      expr_code <- as_r(x$expr, ...)
      return(paste0("min(sapply(", set_name, ", function(", x$index, ") ", expr_code, "))"))
    } else {
      return(paste0("min(", set_name, ")"))
    }
  } else if (!is.null(x$index) && !is.null(x$value)) {
    # Old structure: index is dims object, value is call object
    # Extract set name from dims
    if (inherits(x$index, "dims") && length(x$index) > 0) {
      index_name <- names(x$index)[1]
      set_name <- x$index[[1]]$name  # The set the index iterates over
    }
    # value is the call to min()
    if (inherits(x$value, "call") && x$value$name == "min") {
      if (length(x$value$args) > 0) {
        expr_code <- as_r(x$value$args[[1]], ...)
        return(paste0("min(sapply(", set_name, ", function(", index_name, ") ", expr_code, "))"))
      } else {
        return(paste0("min(", set_name, ")"))
      }
    }
  }
  # Fallback
  "min()"
}

#' @export
as_r.ast_setmin <- function(x, ...) {
  # Handle both old (index/value) and new (set/index/expr) structures
  if (!is.null(x$set)) {
    # New structure
    set_name <- x$set
    if (!is.null(x$expr)) {
      expr_code <- as_r(x$expr, ...)
      return(paste0("min(sapply(", set_name, ", function(", x$index, ") ", expr_code, "))"))
    } else {
      return(paste0("min(", set_name, ")"))
    }
  } else if (!is.null(x$index) && !is.null(x$value)) {
    # Old structure: index is dims object, value is call object
    # Extract set name from dims
    if (inherits(x$index, "dims") && length(x$index) > 0) {
      index_name <- names(x$index)[1]
      set_name <- x$index[[1]]$name  # The set the index iterates over
    }
    # value is the call to min()
    if (inherits(x$value, "call") && x$value$name == "min") {
      if (length(x$value$args) > 0) {
        expr_code <- as_r(x$value$args[[1]], ...)
        return(paste0("min(sapply(", set_name, ", function(", index_name, ") ", expr_code, "))"))
      } else {
        return(paste0("min(", set_name, ")"))
      }
    }
  }
  # Fallback
  "min()"
}

#' @export
as_r.setmax <- function(x, ...) {
  # Handle both old (index/value) and new (set/index/expr) structures
  if (!is.null(x$set)) {
    # New structure
    set_name <- x$set
    if (!is.null(x$expr)) {
      expr_code <- as_r(x$expr, ...)
      return(paste0("max(sapply(", set_name, ", function(", x$index, ") ", expr_code, "))"))
    } else {
      return(paste0("max(", set_name, ")"))
    }
  } else if (!is.null(x$index) && !is.null(x$value)) {
    # Old structure: index is dims object, value is call object
    if (inherits(x$index, "dims") && length(x$index) > 0) {
      index_name <- names(x$index)[1]
      set_name <- x$index[[1]]$name
    }
    if (inherits(x$value, "call") && x$value$name == "max") {
      if (length(x$value$args) > 0) {
        expr_code <- as_r(x$value$args[[1]], ...)
        return(paste0("max(sapply(", set_name, ", function(", index_name, ") ", expr_code, "))"))
      } else {
        return(paste0("max(", set_name, ")"))
      }
    }
  }
  "max()"
}

#' @export
as_r.ast_setmax <- function(x, ...) {
  # Handle both old (index/value) and new (set/index/expr) structures
  if (!is.null(x$set)) {
    # New structure
    set_name <- x$set
    if (!is.null(x$expr)) {
      expr_code <- as_r(x$expr, ...)
      return(paste0("max(sapply(", set_name, ", function(", x$index, ") ", expr_code, "))"))
    } else {
      return(paste0("max(", set_name, ")"))
    }
  } else if (!is.null(x$index) && !is.null(x$value)) {
    # Old structure: index is dims object, value is call object
    if (inherits(x$index, "dims") && length(x$index) > 0) {
      index_name <- names(x$index)[1]
      set_name <- x$index[[1]]$name
    }
    if (inherits(x$value, "call") && x$value$name == "max") {
      if (length(x$value$args) > 0) {
        expr_code <- as_r(x$value$args[[1]], ...)
        return(paste0("max(sapply(", set_name, ", function(", index_name, ") ", expr_code, "))"))
      } else {
        return(paste0("max(", set_name, ")"))
      }
    }
  }
  "max()"
}

#' @export
as_r.if <- function(x, ...) {
  cond_code <- as_r(x$condition, ...)
  then_code <- as_r(x$then_expr, ...)
  else_code <- if (!is.null(x$else_expr)) as_r(x$else_expr, ...) else NULL
  
  if (!is.null(else_code)) {
    paste0("if (", cond_code, ") { ", then_code, " } else { ", else_code, " }")
  } else {
    paste0("if (", cond_code, ") { ", then_code, " }")
  }
}

#' @export
as_r.ast_if <- function(x, ...) {
  cond_code <- as_r(x$condition, ...)
  then_code <- as_r(x$then_expr, ...)
  else_code <- if (!is.null(x$else_expr)) as_r(x$else_expr, ...) else NULL
  
  if (!is.null(else_code)) {
    paste0("if (", cond_code, ") { ", then_code, " } else { ", else_code, " }")
  } else {
    paste0("if (", cond_code, ") { ", then_code, " }")
  }
}

#' @export
as_r.character <- function(x, ...) {
  # String literal
  deparse(x)
}

#' @export
as_r.numeric <- function(x, ...) {
  # Numeric literal
  deparse(x)
}

#' @export
as_r.logical <- function(x, ...) {
  # Logical literal
  deparse(x)
}

#' Extract Arguments from AST
#'
#' @description
#' Recursively extracts all symbol names (potential arguments) from an AST node.
#' This is used to automatically determine function arguments when converting
#' AST to R functions.
#'
#' @param x An AST node
#' @param args Character vector of accumulated argument names
#'
#' @return Character vector of unique argument names
#'
#' @export
extract_args <- function(x, args = character()) {
  UseMethod("extract_args")
}

#' @export
extract_args.symbol <- function(x, args = character()) {
  # Add the base symbol name
  args <- unique(c(args, x$name))
  # Extract from indices/dims
  if (!is.null(x$dims) && length(x$dims) > 0) {
    for (dim in x$dims) {
      args <- extract_args(dim, args)
    }
  }
  args
}

#' @export
extract_args.ast_symbol <- function(x, args = character()) {
  # Add the base symbol name
  args <- unique(c(args, x$name))
  # Extract from indices/dims
  if (!is.null(x$dims) && length(x$dims) > 0) {
    for (dim in x$dims) {
      args <- extract_args(dim, args)
    }
  }
  args
}

#' @export
extract_args.constant <- function(x, args = character()) {
  # Constants don't add arguments
  args
}

#' @export
extract_args.ast_constant <- function(x, args = character()) {
  # Constants don't add arguments
  args
}

#' @export
extract_args.expression <- function(x, args = character()) {
  if (!is.null(x$lhs)) {
    args <- extract_args(x$lhs, args)
  }
  if (!is.null(x$rhs)) {
    args <- extract_args(x$rhs, args)
  }
  args
}

#' @export
extract_args.ast_expression <- function(x, args = character()) {
  if (!is.null(x$lhs)) {
    args <- extract_args(x$lhs, args)
  }
  if (!is.null(x$rhs)) {
    args <- extract_args(x$rhs, args)
  }
  args
}

#' @export
extract_args.unary <- function(x, args = character()) {
  args <- extract_args(x$rhs, args)
  args
}

#' @export
extract_args.ast_unary <- function(x, args = character()) {
  args <- extract_args(x$rhs, args)
  args
}

#' @export
extract_args.call <- function(x, args = character()) {
  for (arg in x$args) {
    args <- extract_args(arg, args)
  }
  args
}

#' @export
extract_args.ast_call <- function(x, args = character()) {
  for (arg in x$args) {
    args <- extract_args(arg, args)
  }
  args
}

#' @export
extract_args.setmin <- function(x, args = character()) {
  # Handle both old (index/value) and new (set/index/expr) structures
  if (!is.null(x$set)) {
    # New structure
    if (!is.null(x$set)) {
      args <- unique(c(args, x$set))
    }
    if (!is.null(x$expr)) {
      expr_args <- extract_args(x$expr, character())
      expr_args <- setdiff(expr_args, x$index)  # Exclude lambda parameter
      args <- unique(c(args, expr_args))
    }
  } else if (!is.null(x$index) && !is.null(x$value)) {
    # Old structure
    if (inherits(x$index, "dims") && length(x$index) > 0) {
      index_name <- names(x$index)[1]
      set_name <- x$index[[1]]$name
      args <- unique(c(args, set_name))
      # Extract args from the value (call to min)
      if (inherits(x$value, "call") && length(x$value$args) > 0) {
        expr_args <- extract_args(x$value$args[[1]], character())
        expr_args <- setdiff(expr_args, index_name)  # Exclude lambda parameter
        args <- unique(c(args, expr_args))
      }
    }
  }
  args
}

#' @export
extract_args.ast_setmin <- function(x, args = character()) {
  # Handle both old (index/value) and new (set/index/expr) structures
  if (!is.null(x$set)) {
    # New structure
    if (!is.null(x$set)) {
      args <- unique(c(args, x$set))
    }
    if (!is.null(x$expr)) {
      expr_args <- extract_args(x$expr, character())
      expr_args <- setdiff(expr_args, x$index)  # Exclude lambda parameter
      args <- unique(c(args, expr_args))
    }
  } else if (!is.null(x$index) && !is.null(x$value)) {
    # Old structure
    if (inherits(x$index, "dims") && length(x$index) > 0) {
      index_name <- names(x$index)[1]
      set_name <- x$index[[1]]$name
      args <- unique(c(args, set_name))
      # Extract args from the value (call to min)
      if (inherits(x$value, "call") && length(x$value$args) > 0) {
        expr_args <- extract_args(x$value$args[[1]], character())
        expr_args <- setdiff(expr_args, index_name)  # Exclude lambda parameter
        args <- unique(c(args, expr_args))
      }
    }
  }
  args
}

#' @export
extract_args.setmax <- function(x, args = character()) {
  # Handle both old (index/value) and new (set/index/expr) structures
  if (!is.null(x$set)) {
    # New structure
    if (!is.null(x$set)) {
      args <- unique(c(args, x$set))
    }
    if (!is.null(x$expr)) {
      expr_args <- extract_args(x$expr, character())
      expr_args <- setdiff(expr_args, x$index)
      args <- unique(c(args, expr_args))
    }
  } else if (!is.null(x$index) && !is.null(x$value)) {
    # Old structure
    if (inherits(x$index, "dims") && length(x$index) > 0) {
      index_name <- names(x$index)[1]
      set_name <- x$index[[1]]$name
      args <- unique(c(args, set_name))
      if (inherits(x$value, "call") && length(x$value$args) > 0) {
        expr_args <- extract_args(x$value$args[[1]], character())
        expr_args <- setdiff(expr_args, index_name)
        args <- unique(c(args, expr_args))
      }
    }
  }
  args
}

#' @export
extract_args.ast_setmax <- function(x, args = character()) {
  # Handle both old (index/value) and new (set/index/expr) structures
  if (!is.null(x$set)) {
    # New structure
    if (!is.null(x$set)) {
      args <- unique(c(args, x$set))
    }
    if (!is.null(x$expr)) {
      expr_args <- extract_args(x$expr, character())
      expr_args <- setdiff(expr_args, x$index)
      args <- unique(c(args, expr_args))
    }
  } else if (!is.null(x$index) && !is.null(x$value)) {
    # Old structure
    if (inherits(x$index, "dims") && length(x$index) > 0) {
      index_name <- names(x$index)[1]
      set_name <- x$index[[1]]$name
      args <- unique(c(args, set_name))
      if (inherits(x$value, "call") && length(x$value$args) > 0) {
        expr_args <- extract_args(x$value$args[[1]], character())
        expr_args <- setdiff(expr_args, index_name)
        args <- unique(c(args, expr_args))
      }
    }
  }
  args
}

#' @export
extract_args.if <- function(x, args = character()) {
  if (!is.null(x$condition)) {
    args <- extract_args(x$condition, args)
  }
  if (!is.null(x$then_expr)) {
    args <- extract_args(x$then_expr, args)
  }
  if (!is.null(x$else_expr)) {
    args <- extract_args(x$else_expr, args)
  }
  args
}

#' @export
extract_args.ast_if <- function(x, args = character()) {
  if (!is.null(x$condition)) {
    args <- extract_args(x$condition, args)
  }
  if (!is.null(x$then_expr)) {
    args <- extract_args(x$then_expr, args)
  }
  if (!is.null(x$else_expr)) {
    args <- extract_args(x$else_expr, args)
  }
  args
}

#' @export
extract_args.character <- function(x, args = character()) {
  # String literals don't add arguments
  args
}

#' @export
extract_args.numeric <- function(x, args = character()) {
  # Numeric literals don't add arguments
  args
}

#' @export
extract_args.logical <- function(x, args = character()) {
  # Logical literals don't add arguments
  args
}

#' Convert ast_formula to R Code
#'
#' @param x ast_formula object
#' @param ... Additional arguments passed to as_r for the wrapped expression
#' @return R code string
#' @export
as_r.ast_formula <- function(x, ...) {
  # Delegate to the wrapped expression
  as_r(x$expr, ...)
}

#' Extract Arguments from ast_formula
#'
#' @param x ast_formula object
#' @param args Character vector of accumulated argument names
#' @return Character vector of argument names
#' @export
extract_args.ast_formula <- function(x, args = character()) {
  # Return pre-computed args (already extracted during construction)
  unique(c(args, x$args))
}

#' Convert AST to R Function
#'
#' @description
#' Converts an AST expression to an executable R function. The function
#' arguments are automatically detected from symbols in the AST, or can
#' be explicitly specified.
#'
#' @param x An AST node (expression, symbol, etc.)
#' @param args Character vector of explicit argument names (optional).
#'   If NULL, arguments are automatically extracted from the AST.
#' @param envir Environment in which to create the function (default: parent.frame())
#' @param ... Additional arguments (currently unused)
#'
#' @return An R function that evaluates the AST expression
#'
#' @examples
#' \dontrun{
#' # Simple expression: DiscountRate[r]
#' ast <- ast_expression(
#'   op = "[",
#'   lhs = ast_symbol("DiscountRate"),
#'   rhs = ast_symbol("r")
#' )
#' fn <- as_rfunction(ast)
#' # Creates: function(DiscountRate, r) { DiscountRate[r] }
#' 
#' # Call it
#' result <- fn(DiscountRate = c(R1=0.05, R2=0.07), r = "R1")
#' # Returns: 0.05
#' }
#'
#' @export
as_rfunction <- function(x, args = NULL, envir = parent.frame(), ...) {
  # Convert AST to R code string
  body_code <- as_r(x, ...)
  
  # Auto-detect arguments if not provided
  if (is.null(args)) {
    args <- extract_args(x)
  }
  
  # Create formal arguments (all required, no defaults)
  formals_list <- setNames(
    rep(list(quote(expr=)), length(args)),
    args
  )
  
  # Parse the body code into an expression
  body_expr <- str2lang(body_code)
  
  # Create the function
  fn <- as.function(c(formals_list, body_expr), envir = envir)
  
  # Add attributes for debugging/inspection
  attr(fn, "ast") <- x
  attr(fn, "code") <- body_code
  attr(fn, "args") <- args
  
  fn
}

#' Print method for functions created from AST
#'
#' @param x A function created by as_rfunction
#' @param ... Additional arguments (unused)
#'
#' @export
print.ast_function <- function(x, ...) {
  cat("R Function from AST\n")
  cat("-------------------\n")
  cat("Arguments:", paste(attr(x, "args"), collapse = ", "), "\n")
  cat("Code:", attr(x, "code"), "\n\n")
  cat("Function:\n")
  print.function(x)
  invisible(x)
}
