# File: R/zzz.R

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.multimod <- list(
    multimod.hash_algo = "crc32",
    multimod.render_where_inline = FALSE,
    multimod.latex.where_indent = "2em",
    multimod.latex.use_indicator = FALSE,
    multimod.latex.indicator_symbol = 1 # "\\delta"
  )
  toset <- !(names(op.multimod) %in% names(op))
  if (any(toset)) options(op.multimod[toset])
  invisible()
}

# Column names used in data.table / dplyr expressions, and R6 bindings.
utils::globalVariables(c(".", "value", "n", "self", "private", "min_val", "max_val"))

# Package-level state (e.g. whether Julia was initialised in this session);
# never the global environment.
.mm_state <- new.env(parent = emptyenv())
