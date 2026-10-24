# =============================================================================
# Rule mappings: a mapping defined by joins of smaller tables
# =============================================================================
#
# A large mapping is often a product or a join of small ones - every process
# with every timeslice of its timeframe, every weather series with every leaf
# slice. Storing the result repeats the small tables' content millions of
# times; a rule stores the recipe and the result is enumerated when a reader
# asks for it, in labels for the text writers and in integer codes for the
# matrix route.

#' A mapping rule
#'
#' The tuples of the mapping are the natural join of the tables named in
#' `of`, in order (tables without a shared column form a cross product),
#' kept where they match every table in `gate` (semi-joins) and none in
#' `minus` (anti-joins), projected to `cols` and made unique. A name
#' resolves to an embedded table in `tables`, else to a mapping of the
#' model, else to a set of the model (a one-column table under the set's
#' name). Columns of a join that are not in `cols` are hidden: they take
#' part in the joins and are dropped by the projection.
#'
#' @param of Character; the tables joined.
#' @param gate,minus Character; tables whose matching tuples are kept, or
#'   dropped. Matching is on the columns shared with the join.
#' @param tables A named list of data frames embedded in the rule; small
#'   tables that are not symbols of the model.
#' @param cols Character; the columns of the result, in the mapping's
#'   dimension order. [new_mapping()] fills it from `dims` when `NULL`.
#' @return An object of class `mm_rule`; its `hash` identifies the rule's
#'   content (names and embedded tables).
#' @export
#' @examples
#' pf <- data.frame(tech = c("coal", "wind"), frame = c("ANNUAL", "HOUR"))
#' fs <- data.frame(frame = c("ANNUAL", "HOUR", "HOUR"),
#'                  slice = c("y", "h1", "h2"))
#' r <- mapping_rule(of = c("pf", "fs"), tables = list(pf = pf, fs = fs),
#'                   cols = c("tech", "slice"))
#' m <- new_mapping("mTechSlice", dims = c("tech", "slice"), rule = r)
#' materialise_rule(list(), r)
mapping_rule <- function(of, gate = NULL, minus = NULL, tables = NULL, cols = NULL) {
  stopifnot(is.character(of), length(of) >= 1L, all(nzchar(of)))
  gate <- as.character(gate %||% character())
  minus <- as.character(minus %||% character())
  tables <- tables %||% list()
  if (length(tables)) {
    if (is.null(names(tables)) || !all(nzchar(names(tables))) || anyDuplicated(names(tables))) {
      stop("mapping_rule: `tables` must be a uniquely named list", call. = FALSE)
    }
    tables <- lapply(tables, function(t) {
      if (!is.data.frame(t)) stop("mapping_rule: every embedded table must be a data frame", call. = FALSE)
      t <- as.data.frame(t, stringsAsFactors = FALSE)
      for (j in seq_along(t)) if (!is.character(t[[j]])) t[[j]] <- as.character(t[[j]])
      unique(t)
    })
  }
  if (!is.null(cols)) stopifnot(is.character(cols), all(nzchar(cols)), !anyDuplicated(cols))
  r <- list(of = of, gate = gate, minus = minus, tables = tables, cols = cols, hash = NULL)
  r$hash <- rlang::hash(r[c("of", "gate", "minus", "tables", "cols")])
  structure(r, class = "mm_rule")
}

#' @export
print.mm_rule <- function(x, ...) {
  cat("<mapping rule> ", paste(x$of, collapse = " * "), sep = "")
  if (length(x$gate)) cat(" & ", paste(x$gate, collapse = " & "), sep = "")
  if (length(x$minus)) cat(" - ", paste(x$minus, collapse = " - "), sep = "")
  cat(" -> (", paste(x$cols, collapse = ", "), ")\n", sep = "")
  if (length(x$tables)) {
    cat("  embedded:", paste(sprintf("%s[%d]", names(x$tables), vapply(x$tables, nrow, 1L)), collapse = " "), "\n")
  }
  invisible(x)
}

#' The names a rule reads: joins, gates and subtractions
#' @keywords internal
#' @noRd
.rule_leaves <- function(rule) unique(c(rule$of, rule$gate, rule$minus))

#' One leaf of a rule as a data.table of character columns
#'
#' Resolution order: embedded table, mapping of the model, set of the model.
#' @keywords internal
#' @noRd
.rule_leaf <- function(model, rule, nm) {
  t <- rule$tables[[nm]]
  if (!is.null(t)) return(data.table::as.data.table(t))
  if (!is.null(model$mappings[[nm]])) {
    if (!is.null(model$mappings[[nm]]$rule) && is.null(model$mappings[[nm]]$data)) {
      # a rule of a rule: its own enumeration
      d <- materialise_rule(model, model$mappings[[nm]]$rule)
    } else {
      d <- get_data(model, nm, type = "mapping")
    }
    d <- if (is.null(d)) data.table::data.table() else data.table::as.data.table(d)
    if (!ncol(d)) {
      nms <- tryCatch(get_dim_names(model$mappings[[nm]]$dims), error = function(e) character())
      d <- data.table::setDT(stats::setNames(rep(list(character()), length(nms)), nms))
    }
    for (cl in names(d)) if (!is.character(d[[cl]])) data.table::set(d, j = cl, value = as.character(d[[cl]]))
    return(d)
  }
  if (!is.null(model$sets[[nm]])) {
    mem <- get_data(model, nm, type = "set")
    return(data.table::setDT(stats::setNames(list(as.character(mem %||% character())), nm)))
  }
  stop("mapping rule: '", nm, "' is neither an embedded table nor a mapping or set of the model.",
       call. = FALSE)
}

#' Evaluate a rule over its leaves
#'
#' `leaf(nm)` returns a data.table; its columns are joined by name. The
#' result has the columns `cols`, unique, in the canonical order of the
#' joins (the first table's rows outermost).
#' @keywords internal
#' @noRd
.rule_eval <- function(rule, leaf) {
  out <- NULL
  for (nm in rule$of) {
    t <- leaf(nm)
    if (is.null(out)) { out <- t; next }
    shared <- intersect(names(out), names(t))
    if (length(shared)) {
      out <- merge(out, t, by = shared, allow.cartesian = TRUE, sort = FALSE)
    } else {
      # a cross product: every row of `out` with every row of `t`
      if (!nrow(out) || !nrow(t)) {
        out <- data.table::setDT(c(as.list(out[0]), as.list(t[0])))
      } else {
        a <- rep(seq_len(nrow(out)), each = nrow(t))
        b <- rep(seq_len(nrow(t)), times = nrow(out))
        out <- data.table::setDT(c(as.list(out[a]), as.list(t[b])))
      }
    }
  }
  # semi- and anti-joins that keep the rows of `out` in their order
  matched <- function(nm) {
    t <- leaf(nm)
    shared <- intersect(names(out), names(t))
    if (!length(shared)) stop("mapping rule: '", nm, "' shares no column with the join.", call. = FALSE)
    ut <- unique(t[, shared, with = FALSE])
    !is.na(ut[out, on = shared, which = TRUE, mult = "first"])
  }
  for (nm in rule$gate) out <- out[matched(nm)]
  for (nm in rule$minus) out <- out[!matched(nm)]
  miss <- setdiff(rule$cols, names(out))
  if (length(miss)) {
    stop("mapping rule: column(s) ", paste(miss, collapse = ", "), " are in none of the joined tables.",
         call. = FALSE)
  }
  out <- out[, rule$cols, with = FALSE]
  if (ncol(out) < length(rule$cols) || anyDuplicated(names(out))) stop("mapping rule: ambiguous columns.", call. = FALSE)
  unique(out, by = rule$cols)
}

#' Enumerate the tuples of a rule
#'
#' @param model The model whose mappings and sets the rule names; may be an
#'   empty list when every leaf is embedded.
#' @param rule An `mm_rule`.
#' @return A data.frame of character columns `rule$cols`, one row per tuple.
#' @export
#' @rdname mapping_rule
materialise_rule <- function(model, rule) {
  stopifnot(inherits(rule, "mm_rule"))
  if (is.null(rule$cols)) stop("materialise_rule: the rule has no `cols`.", call. = FALSE)
  out <- .rule_eval(rule, function(nm) .rule_leaf(model, rule, nm))
  data.table::setDF(out)
  out
}

#' The tuples of a rule as codes of a build dictionary
#'
#' Columns in `rule$cols` are coded with `codes` (the dictionary of the
#' index tables); hidden join columns with a dictionary of their own, so
#' that their labels never enter the build's. The result is cached on the
#' dictionary by the rule's hash, so the column index, the row index and
#' the equations of one build share one enumeration.
#' @return A data.table of integer columns `rule$cols`, each flagged
#'   `mx_coded` as [.mx_code_in()] expects.
#' @keywords internal
#' @noRd
rule_tuples_coded <- function(model, rule, codes) {
  key <- paste0(rule$hash, "|", paste(.rule_leaf_ids(model, rule), collapse = "|"))
  hit <- codes$rules[[key]]
  if (!is.null(hit)) return(hit)
  hidden <- .mx_new_codes()
  leaf <- function(nm) {
    t <- .rule_leaf(model, rule, nm)
    for (cl in names(t)) {
      v <- .mx_code_in(if (cl %in% rule$cols) codes else hidden, t[[cl]])
      data.table::set(t, j = cl, value = v)
    }
    t
  }
  out <- .rule_eval(rule, leaf)
  # keyed, so a consumer keying a shallow rewrap of it (the equations join
  # on these columns) finds it sorted and leaves the shared vectors alone
  data.table::setkeyv(out, rule$cols)
  for (cl in names(out)) data.table::setattr(out[[cl]], "mx_coded", TRUE)
  if (is.null(codes$rules)) codes$rules <- list()
  codes$rules[[key]] <- out
  out
}

# the identity of the leaves taken from the model, so that two models with
# the same rule but different mappings never share a cached enumeration
.rule_leaf_ids <- function(model, rule) {
  vapply(setdiff(.rule_leaves(rule), names(rule$tables)), function(nm) {
    obj <- model$mappings[[nm]] %||% model$sets[[nm]]
    if (is.null(obj)) return(nm)
    if (!is.null(obj$rule)) return(paste0(nm, "@", obj$rule$hash))
    d <- obj$data
    if (!is.null(d)) return(paste0(nm, "#", NROW(d), ":", rlang::hash(d)))
    paste0(nm, "~", .data_memo_key(obj, get_data_path(obj, model$base_path)))
  }, character(1), USE.NAMES = FALSE)
}

#' Number of tuples of a rule
#' @keywords internal
#' @noRd
.rule_count <- function(model, rule) nrow(materialise_rule(model, rule))

#' The tuples of a mapping, from its table or its rule
#'
#' The one accessor for a mapping's data outside [get_data()]: a table
#' held in memory, else the enumeration of its rule, else the table read
#' from the workspace or the per-object reference. Readers that look at
#' `$data` directly see `NULL` for a rule mapping.
#' @keywords internal
#' @noRd
.mapping_data <- function(model, name) {
  m <- model$mappings[[name]]
  if (is.null(m)) return(NULL)
  if (!is.null(m$data)) return(m$data)
  get_data(model, name, type = "mapping")
}

#' Write and read a rule beside a mapping's data in a workspace
#' @keywords internal
#' @noRd
.rule_write <- function(rule, file) {
  payload <- list(of = rule$of, gate = rule$gate, minus = rule$minus, cols = rule$cols,
                  tables = rule$tables, hash = rule$hash)
  jsonlite::write_json(payload, file, dataframe = "columns", auto_unbox = FALSE, pretty = TRUE, null = "null")
}

.rule_read <- function(file) {
  p <- jsonlite::read_json(file, simplifyVector = TRUE)
  tables <- p$tables
  if (!length(tables)) tables <- NULL else tables <- lapply(tables, function(t) as.data.frame(t, stringsAsFactors = FALSE))
  mapping_rule(of = as.character(p$of), gate = as.character(p$gate %||% character()),
               minus = as.character(p$minus %||% character()), tables = tables,
               cols = as.character(p$cols))
}
