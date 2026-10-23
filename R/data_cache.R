# =============================================================================
# A memo of tables read from disk
# =============================================================================
#
# get_data() reads a symbol's table from the workspace or from the object's
# own reference (misc$path) every time it is asked, as the model it is given
# is a local copy that cannot keep the table. The index builders and the
# block plans ask for the same mapping several times per build, so the
# tables are kept here, keyed by content hash when the object carries one
# (misc$ref$hash) and by the resolved path otherwise, under a byte budget
# with the least recently used table dropped first.

.data_cache <- new.env(parent = emptyenv())
.data_cache$entries <- list()      # key -> list(data, bytes, last)
.data_cache$clock <- 0

.data_cache_limit <- function() {
  g <- suppressWarnings(as.numeric(getOption("multimod.data_cache_gb", 8)))
  if (is.na(g) || g < 0) 8 * 2^30 else g * 2^30
}

# the bytes a table holds, by column type: no walk over the values
.data_bytes <- function(x) {
  if (!is.data.frame(x)) return(as.numeric(utils::object.size(x)))
  n <- nrow(x)
  per <- vapply(x, function(col) {
    if (is.double(col)) 8 else if (is.character(col) || is.list(col)) 8 else if (is.factor(col)) 4 else 4
  }, numeric(1))
  n * sum(per) + 1e3
}

#' The memo key of a symbol's table
#' @param obj The parameter or mapping.
#' @param path The resolved path its data is read from, or `NULL`.
#' @keywords internal
#' @noRd
.data_memo_key <- function(obj, path = NULL) {
  h <- obj$misc$ref$hash
  if (!is.null(h) && nzchar(h)) return(paste0("hash:", h))
  if (!is.null(path)) return(paste0("path:", normalizePath(path, winslash = "/", mustWork = FALSE)))
  NULL
}

.data_memo_get <- function(key) {
  if (is.null(key)) return(NULL)
  e <- .data_cache$entries[[key]]
  if (is.null(e)) return(NULL)
  .data_cache$clock <- .data_cache$clock + 1
  e$last <- .data_cache$clock
  .data_cache$entries[[key]] <- e
  e$data
}

.data_memo_put <- function(key, x) {
  if (is.null(key) || is.null(x)) return(invisible(x))
  bytes <- .data_bytes(x)
  limit <- .data_cache_limit()
  if (bytes > limit) return(invisible(x))     # larger than the whole budget: not kept
  .data_cache$clock <- .data_cache$clock + 1
  .data_cache$entries[[key]] <- list(data = x, bytes = bytes, last = .data_cache$clock)
  .data_memo_trim(limit)
  invisible(x)
}

.data_memo_trim <- function(limit = .data_cache_limit()) {
  repeat {
    total <- sum(vapply(.data_cache$entries, function(e) e$bytes, numeric(1)))
    if (total <= limit || !length(.data_cache$entries)) break
    last <- vapply(.data_cache$entries, function(e) e$last, numeric(1))
    .data_cache$entries[[names(which.min(last))]] <- NULL
  }
  invisible(NULL)
}

.data_memo_drop <- function(key) {
  if (!is.null(key) && !is.null(.data_cache$entries[[key]])) .data_cache$entries[[key]] <- NULL
  invisible(NULL)
}

# read through the memo: `reader()` is called on a miss
.data_memo_read <- function(key, reader) {
  hit <- .data_memo_get(key)
  if (!is.null(hit)) return(hit)
  x <- reader()
  if (!is.null(x) && NROW(x) > 0) .data_memo_put(key, x)
  x
}

#' The memo of tables read from disk
#'
#' [get_data()] keeps the tables it reads from the workspace or from a
#' symbol's own reference in a session-wide memo, keyed by the table's
#' content hash when the symbol carries one and by its path otherwise, so
#' that a mapping several symbols share is read once per session rather
#' than once per use. The memo holds at most
#' `getOption("multimod.data_cache_gb", 8)` gigabytes, dropping the least
#' recently used table first.
#'
#' `clear_data_cache()` empties it; `data_cache_info()` lists what it holds.
#'
#' @return `clear_data_cache()` returns the number of tables dropped,
#'   invisibly. `data_cache_info()` returns a data frame with one row per
#'   table: `key`, `rows`, `bytes`.
#' @export
#' @examples
#' data_cache_info()
#' clear_data_cache()
clear_data_cache <- function() {
  n <- length(.data_cache$entries)
  .data_cache$entries <- list()
  invisible(n)
}

#' @rdname clear_data_cache
#' @export
data_cache_info <- function() {
  e <- .data_cache$entries
  data.frame(key = names(e),
             rows = vapply(e, function(x) NROW(x$data), numeric(1)),
             bytes = vapply(e, function(x) x$bytes, numeric(1)),
             stringsAsFactors = FALSE, row.names = NULL)
}

#' Bring a model's tables into memory
#'
#' Fills `$data` of every parameter and mapping that holds no table in
#' memory but has one on disk, through [get_data()] (and so through its
#' memo). The writers that render a model to text read `$data` directly;
#' they call this first, so a model whose tables are references renders
#' the same as one that holds them.
#'
#' @param model A multimod model.
#' @param symbols Names of the symbols to load; `NULL` for all.
#' @return The model with the tables attached.
#' @export
#' @examples
#' m <- load_model_data(example_models$toy)
load_model_data <- function(model, symbols = NULL) {
  fill <- function(objs, type) {
    for (nm in names(objs)) {
      if (!is.null(symbols) && !nm %in% symbols) next
      obj <- objs[[nm]]
      if (!is.null(obj$data) && NROW(obj$data) > 0) next
      if (is.null(model$storage$path) && !.claims_external_data(obj)) next
      d <- get_data(model, nm, type = type)
      if (is.data.frame(d)) objs[[nm]]$data <- d
    }
    objs
  }
  model$parameters <- fill(model$parameters, "parameter")
  model$mappings <- fill(model$mappings, "mapping")
  model
}
