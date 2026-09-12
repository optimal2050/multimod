# =============================================================================
# LP row and column index tables
# =============================================================================
#
# The row/column index of an energyRt-style model is not computed: it is read.
# Every variable and equation carries a `domain` naming a mapping, and that
# mapping's `data` is the exact set of index tuples at which the symbol exists.
# So one LP column exists per row of the variable's domain mapping, and one LP
# row per row of the equation's domain mapping.
#
# This is what makes the direct-matrix path viable at scale: the expensive
# question - which of the full Cartesian product actually exists - was already
# answered by energyRt's interpolation, and is sitting in data.tables.
#
# Two asymmetries to watch:
#   * `variable$domain` is a character (mapping name)
#   * `equation$domain` is an `ast_mapping` node (use `$name`)
#
# Symbols with no domain are scalars (vObjective, eqObjective) and contribute a
# single column / row.
# =============================================================================

#' Name of the mapping a symbol is gated by
#' @keywords internal
#' @noRd
.domain_name <- function(domain) {
  if (is.null(domain)) return(NA_character_)
  if (is.character(domain)) return(if (length(domain)) domain[1] else NA_character_)
  if (!is.null(domain$name)) return(domain$name)
  NA_character_
}

#' Tuple table for a symbol's domain mapping
#'
#' Returns a (possibly zero-row) data.table. A zero-row result means the symbol
#' is gated by a map that holds no tuples, so it contributes **nothing** to the
#' LP - which is different from having no domain at all (a scalar).
#' @keywords internal
#' @noRd
.domain_tuples <- function(model, domain_name) {
  if (is.null(model$mappings[[domain_name]])) {
    stop("Domain mapping '", domain_name, "' is not defined in the model.")
  }
  d <- get_data(model, domain_name, type = "mapping")
  if (is.null(d)) return(data.table::data.table())
  data.table::as.data.table(d)
}

#' Block for a symbol with no domain: exactly one scalar row/column
#' @keywords internal
#' @noRd
.scalar_block <- function(symbol, names = TRUE) {
  blk <- if (names) {
    data.table::data.table(symbol = symbol, name = symbol)
  } else {
    data.table::data.table(symbol = symbol)
  }
  data.table::setattr(blk, "dim_names", character())
  blk
}

#' Block for one symbol's tuples, plus (optionally) a readable name
#'
#' `names = FALSE` skips the `symbol.i1.i2...` name column. On a large model
#' the names are the single biggest allocation of index building — one fresh
#' string per LP row and column, pinned in R's string cache — and nothing on
#' the MPS/solution path reads them (the `.mps` uses positional names and the
#' decoder maps back by position and by the `i*` columns).
#' @keywords internal
#' @noRd
.symbol_block <- function(tuples, symbol, names = TRUE) {
  # Shallow rewrap: share the mapping's column vectors instead of deep-copying
  # the table. Every later change either replaces a column pointer (`set()` on
  # a converted or new column) or renames THIS table's own names vector, so
  # the model's stored mapping is never mutated.
  dims <- data.table::copy(names(tuples))
  blk <- data.table::setDT(as.list(tuples))
  # Canonicalise index members to character. Sets like `year` arrive as integer
  # from some sources and character from others; joining an integer key against
  # a character one is an error, and padding short symbols with NA_character_
  # would coerce inconsistently anyway.
  for (cl in dims) {
    if (!is.character(blk[[cl]])) {
      data.table::set(blk, j = cl, value = as.character(blk[[cl]]))
    }
  }
  if (names) {
    nm <- do.call(paste, c(list(symbol), as.list(blk), list(sep = ".")))
  }
  data.table::setnames(blk, dims, paste0("i", seq_along(dims)))
  data.table::set(blk, j = "symbol", value = symbol)
  if (names) data.table::set(blk, j = "name", value = nm)
  data.table::setattr(blk, "dim_names", dims)
  blk
}

#' Stack per-symbol blocks into one index table
#' @keywords internal
#' @noRd
.stack_index <- function(pieces, key_col, extra_cols) {
  pieces <- pieces[!vapply(pieces, is.null, TRUE)]
  if (!length(pieces)) {
    stop("No symbols contributed any ", key_col,
         ": every domain mapping was empty.")
  }
  arity <- max(vapply(
    pieces, function(p) sum(grepl("^i[0-9]+$", names(p))), integer(1)
  ))
  out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  idx <- paste0("i", seq_len(max(arity, 1L)))
  for (cl in setdiff(idx, names(out))) {
    data.table::set(out, j = cl, value = NA_character_)
  }
  data.table::set(out, j = key_col, value = seq_len(nrow(out)))
  # `name` is absent when the blocks were built with names = FALSE
  data.table::setcolorder(out, intersect(
    c(key_col, "symbol", "name", idx, extra_cols), names(out)))
  out[]
}

#' Build the LP column index
#'
#' One row per LP column. Columns are enumerated per variable from the tuples of
#' the mapping named by `variable$domain`; a variable with no domain is treated
#' as a scalar and contributes a single column.
#'
#' @param model A multimod model with data attached.
#' @param verbose Logical; report per-variable column counts.
#' @param names Logical; add the readable `name` column (`symbol.i1.i2...`).
#'   `FALSE` skips it — on large models the names dominate index-building
#'   memory, and the MPS/solution path never reads them.
#'
#' @return A `data.table` with columns `j` (1-based column index), `symbol`,
#'   `name` (`symbol.i1.i2...`, when `names = TRUE`), `i1`..`iN` (index
#'   members, `NA` beyond a symbol's arity), and bounds `lo`, `up`, `vtype`.
#'   Per-symbol dimension names are kept in the `dim_names` attribute (a named
#'   list).
#' @export
build_col_index <- function(model, verbose = FALSE, names = TRUE) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  vars <- model$variables
  if (!length(vars)) stop("Model has no variables.")

  dim_names <- list()
  pieces <- vector("list", length(vars))

  for (k in seq_along(vars)) {
    v <- vars[[k]]
    nm <- base::names(vars)[k]
    dn <- .domain_name(v$domain)
    if (is.na(dn)) {
      blk <- .scalar_block(nm, names = names)     # ungated scalar -> 1 column
    } else {
      tp <- .domain_tuples(model, dn)
      if (!nrow(tp)) {                            # empty map -> no columns at all
        if (verbose) message(sprintf("  %-30s %8d  (empty map %s)", nm, 0L, dn))
        next
      }
      blk <- .symbol_block(tp, nm, names = names)
    }
    dim_names[[nm]] <- attr(blk, "dim_names")

    lo <- if (!is.null(v$bounds$lo)) as.numeric(v$bounds$lo)[1] else NA_real_
    up <- if (!is.null(v$bounds$up)) as.numeric(v$bounds$up)[1] else NA_real_
    vt <- if (!is.null(v$vtype) && nzchar(v$vtype[1])) v$vtype[1] else "continuous"

    # vtype implies bounds only where they are not stated explicitly
    if (is.na(lo) && vt %in% c("positive", "binary")) lo <- 0
    if (is.na(up) && vt == "binary") up <- 1
    if (is.na(lo)) lo <- -Inf
    if (is.na(up)) up <- Inf

    data.table::set(blk, j = "lo", value = lo)
    data.table::set(blk, j = "up", value = up)
    data.table::set(blk, j = "vtype", value = vt)
    pieces[[k]] <- blk
    if (verbose) message(sprintf("  %-30s %8d", nm, nrow(blk)))
  }

  out <- .stack_index(pieces, "j", c("lo", "up", "vtype"))
  data.table::setattr(out, "dim_names", dim_names)
  out
}

#' Build the LP row index
#'
#' One row per LP constraint. Rows are enumerated per equation from the tuples
#' of the mapping named by `equation$domain`; an equation with no domain is
#' treated as a scalar and contributes a single row.
#'
#' Counting is **per equation**, which is what an LP needs. energyRt's
#' `model_size()` counts per *map* and therefore undercounts whenever one
#' mapping gates several equations.
#'
#' @param model A multimod model with data attached.
#' @param verbose Logical; report per-equation row counts.
#'
#' @param names Logical; add the readable `name` column — see
#'   [build_col_index()].
#'
#' @return A `data.table` with columns `i` (1-based row index), `symbol`,
#'   `name` (when `names = TRUE`), `i1`..`iN`, `sense` (`"=="`, `"<="`,
#'   `">="`) and `rhs` (`NA_real_` until coefficient extraction fills it).
#' @export
build_row_index <- function(model, verbose = FALSE, names = TRUE) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  eqs <- model$equations
  if (!length(eqs)) stop("Model has no equations.")

  dim_names <- list()
  pieces <- vector("list", length(eqs))

  for (k in seq_along(eqs)) {
    e <- eqs[[k]]
    nm <- base::names(eqs)[k]
    dn <- .domain_name(e$domain)
    if (is.na(dn)) {
      blk <- .scalar_block(nm, names = names)     # ungated scalar -> 1 row
    } else {
      tp <- .domain_tuples(model, dn)
      if (!nrow(tp)) {                            # empty map -> no rows at all
        if (verbose) message(sprintf("  %-30s %8d  (empty map %s)", nm, 0L, dn))
        next
      }
      blk <- .symbol_block(tp, nm, names = names)
    }
    dim_names[[nm]] <- attr(blk, "dim_names")

    rel <- if (!is.null(e$relation) && nzchar(e$relation[1])) e$relation[1] else NA_character_
    if (is.na(rel)) {
      stop("Equation '", nm, "' has no relation; cannot determine constraint sense.")
    }
    data.table::set(blk, j = "sense", value = rel)
    data.table::set(blk, j = "rhs", value = NA_real_)
    pieces[[k]] <- blk
    if (verbose) message(sprintf("  %-30s %8d  %s", nm, nrow(blk), rel))
  }

  out <- .stack_index(pieces, "i", c("sense", "rhs"))
  data.table::setattr(out, "dim_names", dim_names)
  out
}
