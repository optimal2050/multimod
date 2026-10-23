# =============================================================================
# Coefficient extraction: AST -> (row, column, coefficient) triplets
# =============================================================================
#
# Evaluates an equation's AST **set-at-a-time**: every node is evaluated for all
# of its index tuples at once, with data.table joins, never tuple-by-tuple.
# Tuple-at-a-time evaluation (see evaluate_formula() in R/evaluate_parameters.R)
# is infeasible at this scale, and the filtering maps exist precisely to avoid
# enumerating the Cartesian product.
#
# ## Representation
#
# An "environment" (`env`) is a data.table of currently-bound index tuples:
#   .k              row id within this environment
#   <index columns> one per bound index name (equation dims, plus any iterators
#                   introduced by an enclosing sum/prod)
#
# Evaluating a node returns a data.table of terms:
#   .k    which env row the term belongs to
#   j     LP column index, or NA for a purely numeric term
#   coef  the coefficient
#
# Several rows may share (.k, j); they are summed on assembly. A term with
# j = NA is a constant and ends up on the right-hand side.
#
# ## Sparsity
#
# A `when` node restricts the environment. Rows that fail the condition are
# dropped, which is exactly right: the term contributes nothing there. This is
# the mechanism that keeps the whole evaluation sparse.
# =============================================================================

# Tell data.table that this package uses its `[` semantics. Without it,
# `dt[i]` inside the package namespace silently falls back to data.frame
# subsetting and `[.data.frame` errors with "undefined columns selected".
# Safe here: no other file in the package relies on the data.frame fallback -
# the rest of multimod does its table work with dplyr.
.datatable.aware <- TRUE

#' Signal an AST node the matrix backend cannot handle
#'
#' Never fall through silently: an unhandled node means missing coefficients,
#' and a silently wrong matrix is far worse than an error.
#' @keywords internal
#' @noRd
.mx_unsupported <- function(node, where = "") {
  stop(
    "as_matrix: unsupported AST node of class '", class(node)[1], "'",
    if (nzchar(where)) paste0(" in ", where) else "",
    ".\n  The matrix backend must not silently skip nodes - add a handler ",
    "or fix the model."
  )
}

#' Index names a symbol reference is indexed by, in order
#' @keywords internal
#' @noRd
.mx_ref_dims <- function(node) {
  d <- node$dims
  if (is.null(d) || length(d) == 0L) return(character())
  vapply(d, dim_binding_name, character(1), USE.NAMES = FALSE)
}

#' Storage columns of a symbol's data table, in declaration order
#' @keywords internal
#' @noRd
.mx_storage_cols <- function(dt, value_col = "value") {
  setdiff(names(dt), value_col)
}

# --- integer coding of index members ----------------------------------------
#
# Every join of the evaluator runs on integer codes. One dictionary per build
# maps each distinct member label (as a string) to a code; the mapping is a
# bijection, so a join on codes gives exactly the rows a join on labels would.
# Members arrive as character or integer (`year`) from different stores; both
# are coded through their character form, which is the canonical type the
# label join used before.

#' @keywords internal
#' @noRd
.mx_new_codes <- function() {
  e <- new.env(parent = emptyenv())
  e$levels <- character()
  e
}

#' Code member labels, extending the dictionary with labels not seen yet
#' @return Integer codes; `NA` for `NA` labels.
#' @keywords internal
#' @noRd
.mx_code <- function(ctx, x) .mx_code_in(ctx$codes, x)

#' Code member labels into a given dictionary
#' @param codes A dictionary from `.mx_new_codes()`.
#' @keywords internal
#' @noRd
.mx_code_in <- function(codes, x) {
  if (is.integer(x) && isTRUE(attr(x, "mx_coded"))) return(x)
  if (!is.character(x)) x <- as.character(x)
  lv <- codes$levels
  out <- data.table::chmatch(x, lv)
  miss <- is.na(out) & !is.na(x)
  if (any(miss)) {
    lv <- c(lv, unique(x[miss]))
    codes$levels <- lv
    out[miss] <- data.table::chmatch(x[miss], lv)
  }
  out
}

#' Is this a build dictionary?
#' @keywords internal
#' @noRd
.mx_is_codes <- function(x) is.environment(x) && is.character(x$levels)

#' Labels behind codes
#' @keywords internal
#' @noRd
.mx_decode <- function(ctx, code) ctx$codes$levels[code]

#' A shallow rewrap of a table with the given columns coded
#'
#' The input's column vectors are shared, never written; the coded columns
#' replace them in the rewrap only, under `new` names.
#' @keywords internal
#' @noRd
.mx_code_cols <- function(ctx, dt, cols, new = cols) {
  out <- data.table::setDT(as.list(dt))
  for (k in seq_along(cols)) {
    data.table::set(out, j = cols[k], value = .mx_code(ctx, out[[cols[k]]]))
  }
  if (!identical(cols, new)) data.table::setnames(out, cols, new)
  out
}

#' The index columns of an index table as codes of the build's dictionary
#'
#' An index table built with the build's own dictionary (`codes =` on
#' [build_col_index()]) is taken as it is, columns shared; one with labels is
#' coded into the dictionary. A table coded with another dictionary is
#' refused: its codes would not mean the same members.
#' @param keep The non-index columns to carry.
#' @keywords internal
#' @noRd
.mx_index_coded <- function(ctx, x, keep) {
  idx <- grep("^i[0-9]+$", names(x), value = TRUE)
  own <- attr(x, "codes")
  if (.mx_is_codes(own)) {
    if (!identical(own, ctx$codes)) {
      stop("as_matrix: the index table is coded with a different dictionary ",
           "than this build's.", call. = FALSE)
    }
    return(data.table::setDT(as.list(x)[c(keep, idx)]))
  }
  .mx_code_cols(ctx, data.table::setDT(as.list(x)[c(keep, idx)]), idx)
}

# --- operation trace (development aid) --------------------------------------
#
# `options(multimod.mx_trace = list(dir = , equations = ))` records every join
# the evaluator performs for the named equations: the input tables as Parquet
# and one line per operation in `ops.csv` (sequence, equation, operation,
# table names, join specification as JSON). A replay of the trace on another
# engine measures that engine on the real key distributions of real blocks.

#' @keywords internal
#' @noRd
.mx_trace_open <- function(spec) {
  if (is.null(spec)) return(NULL)
  dir.create(spec$dir, recursive = TRUE, showWarnings = FALSE)
  e <- new.env(parent = emptyenv())
  e$dir <- spec$dir
  e$equations <- spec$equations
  e$n <- 0L
  e$ops <- file.path(spec$dir, "ops.csv")
  if (file.exists(e$ops)) unlink(e$ops)
  e
}

#' @keywords internal
#' @noRd
.mx_trace <- function(ctx, op, tables, spec = list()) {
  tr <- ctx$trace
  if (is.null(tr)) return(invisible())
  eq <- ctx$trace_eq
  if (!is.null(tr$equations) && !(isTRUE(eq %in% tr$equations))) return(invisible())
  tr$n <- tr$n + 1L
  tag <- sprintf("%05d", tr$n)
  for (nm in names(tables)) {
    arrow::write_parquet(as.data.frame(tables[[nm]]),
                         file.path(tr$dir, paste0(tag, "_", nm, ".parquet")))
  }
  line <- data.frame(seq = tr$n, equation = eq %||% "", op = op,
                     tables = paste(names(tables), collapse = ";"),
                     rows = paste(vapply(tables, nrow, 1L), collapse = ";"),
                     spec = as.character(jsonlite::toJSON(spec, auto_unbox = TRUE)),
                     stringsAsFactors = FALSE)
  new <- !file.exists(tr$ops)
  utils::write.table(line, tr$ops, append = !new, sep = ",", row.names = FALSE,
                     col.names = new, qmethod = "double")
  invisible()
}

#' Empty term frame
#' @keywords internal
#' @noRd
.mx_no_terms <- function() {
  data.table::data.table(.k = integer(), j = integer(), coef = numeric())
}

#' Constant term over every row of env
#' @keywords internal
#' @noRd
.mx_const <- function(env, value) {
  data.table::data.table(.k = env$.k, j = NA_integer_, coef = as.numeric(value))
}

#' Are all terms purely numeric (no LP column)?
#' @keywords internal
#' @noRd
.mx_is_const <- function(tm) !nrow(tm) || all(is.na(tm$j))

# --- node evaluation --------------------------------------------------------

#' Evaluate one AST node against an environment of bound index tuples
#'
#' @param node AST node.
#' @param env `data.table` with `.k` plus one column per bound index name.
#' @param ctx Evaluation context: the model, its column index, and caches.
#' @return `data.table(.k, j, coef)`.
#' @keywords internal
#' @noRd
.mx_eval <- function(node, env, ctx) {
  if (is.null(node)) return(.mx_no_terms())
  cls <- sub("^mm_", "", class(node)[1])

  switch(
    cls,
    constant   = .mx_eval_constant(node, env, ctx),
    symbol     = .mx_eval_symbol(node, env, ctx),
    setmin     = .mx_eval_extremum(node, env, ctx, min),
    setmax     = .mx_eval_extremum(node, env, ctx, max),
    call       = .mx_eval_call(node, env, ctx),
    parameter  = .mx_eval_parameter(node, env, ctx),
    variable   = .mx_eval_variable(node, env, ctx),
    expression = .mx_eval_expression(node, env, ctx),
    unary      = .mx_eval_unary(node, env, ctx),
    when       = .mx_eval_when(node, env, ctx),
    func       = .mx_eval_func(node, env, ctx),
    sum        = .mx_eval_sum(node, env, ctx),   # ast_sum() nodes; GAMS sums arrive as func
    prod       = .mx_eval_prod(node, env, ctx),
    .mx_unsupported(node)
  )
}

#' @keywords internal
#' @noRd
.mx_eval_constant <- function(node, env, ctx) {
  v <- suppressWarnings(as.numeric(node$value))
  if (is.na(v)) {
    stop("as_matrix: non-numeric constant '", node$value, "'.")
  }
  .mx_const(env, v)
}

#' A bare index used as a number
#'
#' MathProg set members are often numeric labels (`set YEAR := 1990 .. 2010`)
#' and arithmetic on the index itself is idiomatic: `y + OperationalLife[r,t]`.
#' GAMS spells that `ord()` or `val()`, so a bare symbol in arithmetic reaches
#' here only from MathProg. A non-numeric member still fails loudly.
#' @keywords internal
#' @noRd
.mx_eval_symbol <- function(node, env, ctx) {
  raw <- dim_binding_name(node)
  nm <- .mx_env_name(ctx, env, raw)
  if (!nm %in% names(env)) {
    stop("as_matrix: index '", raw, "' is used as a value but is not bound ",
         "here.")
  }
  lab <- .mx_decode(ctx, env[[nm]])
  v <- suppressWarnings(as.numeric(lab))
  if (anyNA(v)) {
    stop("as_matrix: index '", nm, "' is used as a number, but member(s) ",
         paste(utils::head(unique(lab[is.na(v)]), 3), collapse = ", "),
         " are not numeric labels.")
  }
  data.table::data.table(.k = env$.k, j = NA_integer_, coef = v)
}

#' A scalar function call
#'
#' Only the ones that stay linear are evaluated, and only over variable-free
#' arguments: `min`/`max` compare their arguments row by row, `abs` takes a
#' magnitude. `max(yy)` over a single argument is that argument, which is how
#' MathProg's `max{yy in YEAR} max(yy)` reads.
#' @keywords internal
#' @noRd
.mx_eval_call <- function(node, env, ctx) {
  nm <- tolower(node$name %||% "")
  args <- node$args
  if (!length(args)) {
    stop("as_matrix: '", node$name, "()' has no arguments.")
  }
  vals <- lapply(args, function(a) {
    tm <- .mx_eval(a, env, ctx)
    if (!.mx_is_const(tm)) {
      stop("as_matrix: '", node$name, "()' of a variable-bearing expression ",
           "is non-linear.")
    }
    v <- tm[, list(v = sum(coef)), by = ".k"]
    out <- v[data.table::data.table(.k = env$.k), on = ".k"]
    ifelse(is.na(out$v), 0, out$v)
  })
  res <- switch(
    nm,
    max = Reduce(pmax, vals),
    min = Reduce(pmin, vals),
    abs = abs(vals[[1]]),
    stop("as_matrix: function '", node$name, "()' cannot be evaluated into a ",
         "linear matrix. Supported: min, max, abs.")
  )
  data.table::data.table(.k = env$.k, j = NA_integer_, coef = as.numeric(res))
}

#' Smallest or largest value an expression takes over a set
#'
#' `min{yy in YEAR} yy` and its max counterpart are scalars: the index is bound
#' to each member in turn, the body evaluated, and the extremum taken. The
#' result does not vary over `env`, so it is returned as a constant term.
#' @keywords internal
#' @noRd
.mx_eval_extremum <- function(node, env, ctx, f) {
  iters <- .mx_index_names(node$index)
  if (length(iters) != 1L) {
    stop("as_matrix: a set minimum or maximum over ", length(iters),
         " indices is not supported; one index is.")
  }
  isets <- .mx_index_sets(node$index)
  base <- if (iters %in% names(isets)) isets[[iters]] else iters
  members <- .mx_set_members(ctx, .mx_base_set(ctx$model, base))
  if (!length(members)) {
    stop("as_matrix: set '", base, "' has no members, so its extremum is ",
         "undefined.")
  }
  sub <- data.table::data.table(.k = seq_along(members))
  sub[[iters]] <- members
  tm <- .mx_eval(node$value, sub, ctx)
  if (!.mx_is_const(tm)) {
    stop("as_matrix: a set extremum over a variable-bearing expression is ",
         "non-linear.")
  }
  vals <- tm[, list(v = sum(coef)), by = ".k"]$v
  if (!length(vals) || anyNA(vals)) {
    stop("as_matrix: a set extremum did not evaluate to a number.")
  }
  .mx_const(env, f(vals))
}

#' @keywords internal
#' @noRd
.mx_eval_parameter <- function(node, env, ctx) {
  nm <- node$name
  # a lag or lead points at a different member; resolve it before the join
  env <- .mx_shift_env(node, env, ctx)
  if (!nrow(env)) return(.mx_no_terms())
  pp <- .mx_prep_param(nm, ctx)

  ref <- .mx_ref_names(.mx_ref_dims(node), env, ctx)

  # scalar parameter
  if (!length(ref)) {
    v <- if (!is.null(pp$first)) pp$first else pp$defv
    return(.mx_const(env, v))
  }

  if (is.null(pp$first)) {
    return(.mx_const(env, pp$defv))  # nothing stored: every tuple is the default
  }

  if (length(pp$cols) != length(ref)) {
    stop("as_matrix: parameter '", nm, "' is stored with ", length(pp$cols),
         " index column(s) but is referenced with ", length(ref), ".")
  }
  miss <- setdiff(ref, names(env))
  if (length(miss)) {
    stop("as_matrix: parameter '", nm, "' references unbound index/indices: ",
         paste(miss, collapse = ", "), ".")
  }

  if (!is.null(pp$wild)) {
    return(.mx_param_wildcard(pp, ref, env, nm, ctx))
  }

  # a tuple may legitimately be absent -> it takes the parameter default,
  # NOT zero (energyRt stores sparsely, e.g. only the 'lo' rows of a bound)
  .mx_trace(ctx, "lookup", list(env = env, lk = pp$lk),
            list(on = as.list(stats::setNames(ref, pp$keys))))
  out <- pp$lk[env, on = stats::setNames(ref, pp$keys)]
  coef <- out$value
  coef[is.na(coef)] <- pp$defv
  data.table::data.table(.k = env$.k, j = NA_integer_, coef = as.numeric(coef))
}

#' A parameter's lookup table, prepared once per build
#'
#' Codes the index columns, applies the duplicate policy and keys the table,
#' so that every reference joins a ready table instead of copying and
#' de-duplicating the data again. Columns are `.c1..cN` plus `value`.
#' @return A list: `defv` (the default), `first` (the first stored value, or
#'   `NULL` when nothing is stored), `cols` (storage columns), `keys`
#'   (`.c1..cN`), `lk` (the keyed table; `NULL` for a wildcard parameter) and
#'   `wild` (the prepared wildcard patterns, else `NULL`).
#' @keywords internal
#' @noRd
.mx_prep_param <- function(nm, ctx) {
  pp <- ctx$param_cache[[nm]]
  if (!is.null(pp)) return(pp)
  dat <- get_data(ctx$model, nm, type = "parameter")
  dat <- if (is.null(dat)) data.table::data.table() else data.table::as.data.table(dat)
  # An infinite stored value means "no restriction here": every energyRt
  # backend drops the row and lets the default apply, rather than emitting an
  # infinite coefficient - write_pyomo.R:493, write_jump.R:287,
  # write_glpk.R:237, write_gams.R:513 all spell `value != Inf`. Reproducing
  # their numbers means reproducing that filter; keeping the rows instead
  # produced 26,280 -Inf coefficients on IB_2050 and HiGHS refused the matrix.
  if ("value" %in% names(dat) && nrow(dat)) {
    ok <- is.finite(dat$value)
    if (!all(ok)) dat <- dat[ok]
  }
  defv <- ctx$model$parameters[[nm]]$defVal
  defv <- if (is.null(defv) || !is.numeric(defv)) 0 else as.numeric(defv)[1]
  # ... and the same writers map an infinite DEFAULT to 0 (`if (def == Inf)
  # def <- 0`). Surprising as a modelling choice, but it is what generated every
  # reference solution this backend is checked against.
  if (!is.finite(defv)) defv <- 0

  pp <- list(defv = defv, first = NULL, cols = character(), keys = character(),
             lk = NULL, wild = NULL)
  if (nrow(dat) && "value" %in% names(dat)) {
    pp$first <- as.numeric(dat$value[1])
    cols <- .mx_storage_cols(dat)
    pp$cols <- cols
    if (length(cols)) {
      keys <- paste0(".c", seq_along(cols))
      lk <- .mx_code_cols(ctx, dat, cols, keys)[, c(keys, "value"), with = FALSE]
      data.table::set(lk, j = "value", value = as.numeric(lk$value))
      pp$keys <- keys
      # A wildcard (NA) index means "every member of this dimension" - energyRt
      # writes them when interpolating with fold = TRUE. Joining on an NA key
      # would match nothing and silently substitute the default, so wildcard
      # rows are looked up on their explicit dimensions only, most explicit
      # first: an explicit value wins over a wildcard one, as when the rows
      # are expanded.
      wild <- vapply(keys, function(k) anyNA(lk[[k]]), logical(1))
      if (any(wild)) {
        pp$wild <- .mx_prep_wildcard(lk, keys, nm, ctx)
      } else {
        lk <- .mx_dedup_param(lk, keys, nm, ctx)
        data.table::setkeyv(lk, keys)
        pp$lk <- lk
      }
    }
  }
  ctx$param_cache[[nm]] <- pp
  pp
}

#' Collapse repeated index tuples of a parameter under the duplicate policy
#'
#' A repeated index tuple would multiply rows on the join. Repeats that all
#' carry the same value are harmless and get collapsed; repeats carrying
#' DIFFERENT values are ambiguous - there is no defensible way to pick one, so
#' say so rather than silently taking whichever data.table returns first.
#' @keywords internal
#' @noRd
.mx_dedup_param <- function(lk, keys, nm, ctx, what = "index tuple(s)") {
  if (!anyDuplicated(lk, by = keys)) return(lk)
  nd <- lk[, .(n = length(unique(value))), by = keys]
  bad <- nd[n > 1L]
  if (nrow(bad)) {
    ex <- paste(.mx_decode(ctx, unlist(bad[1L, keys, with = FALSE])), collapse = ", ")
    pol <- ctx$on_duplicate
    if (is.null(pol) || identical(pol, "error")) {
      stop("as_matrix: parameter '", nm, "' has ", nrow(bad),
           " index tuple(s) stored with more than one distinct value",
           " (e.g. ", ex, ").
  The lookup is ambiguous. Fix the data, or",
           " pass on_duplicate = \"last\" to take the last value, which is",
           " what GAMS and Pyomo do silently.")
    }
    # Report once per parameter: this discards data, and doing it quietly is
    # how an arbitrary choice among many candidates becomes invisible.
    if (is.null(ctx$dup_warned[[nm]])) {
      ctx$dup_warned[[nm]] <- TRUE
      message("as_matrix: parameter '", nm, "' - ", nrow(bad),
              " index tuple(s) had multiple distinct values; keeping the ",
              pol, " (e.g. ", ex, ").")
    }
  }
  if (identical(ctx$on_duplicate, "first")) {
    unique(lk, by = keys, fromLast = FALSE)
  } else {
    unique(lk, by = keys, fromLast = TRUE)   # GAMS/Pyomo semantics
  }
}

#' Prepare the explicit-column groups of a wildcard parameter
#'
#' Rows are grouped by which columns are explicit; the groups are ordered from
#' the most explicit to the least. Within a group, repeated keys follow the
#' parameter's duplicate policy, as in the plain lookup.
#' @return A list of groups: `pos` (positions of the explicit columns),
#'   `keys` (their `.c*` names), `sub` (the keyed table) or, for the
#'   all-wildcard group, `value`.
#' @keywords internal
#' @noRd
.mx_prep_wildcard <- function(lk, keys, nm, ctx) {
  pat <- do.call(paste0, lapply(keys, function(k) ifelse(is.na(lk[[k]]), "0", "1")))
  pats <- unique(pat)
  nexp <- vapply(strsplit(pats, ""), function(x) sum(x == "1"), integer(1))
  pats <- pats[order(-nexp)]
  lapply(pats, function(pt) {
    pos <- which(strsplit(pt, "")[[1]] == "1")
    kp <- keys[pos]
    sub <- lk[pat == pt, c(kp, "value"), with = FALSE]
    if (!length(pos)) {
      v <- if (identical(ctx$on_duplicate, "first")) sub$value[1] else sub$value[nrow(sub)]
      if (length(unique(sub$value)) > 1L &&
          (is.null(ctx$on_duplicate) || identical(ctx$on_duplicate, "error"))) {
        stop("as_matrix: parameter '", nm, "' has more than one distinct ",
             "all-wildcard value; the lookup is ambiguous.")
      }
      return(list(pos = pos, keys = character(), value = v))
    }
    if (anyDuplicated(sub, by = kp)) {
      nd <- sub[, list(n = length(unique(value))), by = kp]
      if (any(nd$n > 1L) &&
          (is.null(ctx$on_duplicate) || identical(ctx$on_duplicate, "error"))) {
        stop("as_matrix: parameter '", nm, "' has wildcard rows with more ",
             "than one distinct value for the same explicit index tuple; the ",
             "lookup is ambiguous.")
      }
      sub <- unique(sub, by = kp,
                    fromLast = !identical(ctx$on_duplicate, "first"))
    }
    data.table::setkeyv(sub, kp)
    list(pos = pos, keys = kp, sub = sub)
  })
}

#' Look up a parameter whose index columns carry wildcards (NA)
#'
#' The prepared groups are tried from the most explicit to the least, each
#' joined on its explicit columns only, and a tuple keeps the first value
#' found.
#' @keywords internal
#' @noRd
.mx_param_wildcard <- function(pp, ref, env, nm, ctx) {
  coef <- rep(NA_real_, nrow(env))
  for (g in pp$wild) {
    todo <- which(is.na(coef))
    if (!length(todo)) break
    if (!length(g$keys)) {
      coef[todo] <- g$value
      next
    }
    hit <- g$sub[env[todo], on = stats::setNames(ref[g$pos], g$keys)]$value
    coef[todo] <- hit
  }
  coef[is.na(coef)] <- pp$defv
  data.table::data.table(.k = env$.k, j = NA_integer_, coef = as.numeric(coef))
}

#' @keywords internal
#' @noRd
.mx_eval_variable <- function(node, env, ctx) {
  nm <- node$name
  # a lag or lead points at a different column; resolve it before the join
  env <- .mx_shift_env(node, env, ctx)
  if (!nrow(env)) return(.mx_no_terms())
  ci <- ctx$col_cache[[nm]]
  if (is.null(ci)) {
    ci <- .mx_col_coded(ctx)[symbol == nm]
    ctx$col_cache[[nm]] <- ci
  }
  if (!nrow(ci)) {
    # variable exists in the AST but has no columns in this scenario (its
    # domain map is empty): the term simply does not exist
    return(.mx_no_terms())
  }

  ref <- .mx_ref_names(.mx_ref_dims(node), env, ctx)
  if (!length(ref)) {                       # scalar variable, e.g. vObjective
    return(data.table::data.table(.k = env$.k, j = ci$j[1], coef = 1))
  }

  idx <- paste0("i", seq_along(ref))
  if (!all(idx %in% names(ci))) {
    stop("as_matrix: variable '", nm, "' is referenced with ", length(ref),
         " index/indices but its column index has fewer.")
  }
  miss <- setdiff(ref, names(env))
  if (length(miss)) {
    stop("as_matrix: variable '", nm, "' references unbound index/indices: ",
         paste(miss, collapse = ", "), ".")
  }

  # no column there -> no term
  .mx_trace(ctx, "lookup_j", list(env = env, lk = ci),
            list(on = as.list(stats::setNames(ref, idx))))
  out <- ci[env, on = stats::setNames(ref, idx), nomatch = NULL]
  if (!nrow(out)) return(.mx_no_terms())
  data.table::data.table(.k = out$.k, j = out$j, coef = 1)
}

#' The column index with its members coded, once per build
#' @keywords internal
#' @noRd
.mx_col_coded <- function(ctx) {
  cc <- ctx$col_coded
  if (!is.null(cc)) return(cc)
  cc <- .mx_index_coded(ctx, ctx$col_index, c("j", "symbol"))
  ctx$col_coded <- cc
  cc
}

#' @keywords internal
#' @noRd
.mx_eval_unary <- function(node, env, ctx) {
  tm <- .mx_eval(node$rhs, env, ctx)
  if (identical(node$op, "-")) {
    if (nrow(tm)) {
      tm <- data.table::data.table(.k = tm$.k, j = tm$j, coef = -tm$coef)
    }
    return(tm)
  }
  if (identical(node$op, "+")) return(tm)
  stop("as_matrix: unsupported unary operator '", node$op, "'.")
}

#' @keywords internal
#' @noRd
.mx_eval_expression <- function(node, env, ctx) {
  op <- node$op

  if (op %in% c("+", "-")) {
    l <- .mx_eval(node$lhs, env, ctx)
    r <- .mx_eval(node$rhs, env, ctx)
    if (op == "-" && nrow(r)) {
      r <- data.table::data.table(.k = r$.k, j = r$j, coef = -r$coef)
    }
    return(data.table::rbindlist(list(l, r), use.names = TRUE))
  }

  if (op == "*") {
    l <- .mx_eval(node$lhs, env, ctx)
    r <- .mx_eval(node$rhs, env, ctx)
    lc <- .mx_is_const(l); rc <- .mx_is_const(r)
    if (lc && rc) return(.mx_scale(l, r, "*"))
    if (lc) return(.mx_scale(r, l, "*"))
    if (rc) return(.mx_scale(l, r, "*"))
    stop("as_matrix: product of two variable-bearing expressions is non-linear ",
         "and cannot be represented in an LP matrix.")
  }

  if (op == "/") {
    l <- .mx_eval(node$lhs, env, ctx)
    r <- .mx_eval(node$rhs, env, ctx)
    if (!.mx_is_const(r)) {
      stop("as_matrix: division by a variable-bearing expression is non-linear.")
    }
    return(.mx_scale(l, r, "/"))
  }

  if (op == "**" || op == "^") {
    l <- .mx_eval(node$lhs, env, ctx)
    r <- .mx_eval(node$rhs, env, ctx)
    if (!.mx_is_const(l) || !.mx_is_const(r)) {
      stop("as_matrix: exponentiation involving variables is non-linear.")
    }
    # A power does not distribute over terms: `(1 + r)^n` is not `1^n + r^n`,
    # and a discount base is written as exactly that sum. Scaling term by term
    # computed `1 + r^n`, which is ~1 for a small rate, so a discount factor
    # came out as 1 and a sinking-fund denominator as 0.
    return(data.table::data.table(
      .k = env$.k, j = NA_integer_,
      coef = .mx_reduce(l, env) ^ .mx_reduce(r, env)))
  }

  stop("as_matrix: unsupported operator '", op, "' in an equation body. ",
       "Comparison/logical operators are only valid inside a condition.")
}

#' Compute the parameters a model defines by a formula
#'
#' A parameter written `param DiscountFactor{r, y} := (1 + DiscountRate[r])^...`
#' carries a formula and no data. The text backends hand that formula to the
#' target language and never need its values; the matrix backend has no target
#' language, so it has to work them out.
#'
#' Left alone, such a parameter falls back to its scalar default, which is a
#' silent error: every index takes one number. That only surfaced because a
#' default of 0 produced infinities an equation divided by - a default of 1
#' would have given a plausible wrong answer instead.
#'
#' [evaluate_parameters()] clears a formula once it has computed it, so a
#' parameter that still carries one was not evaluated at all - an ordering it
#' could not resolve, say - and is refused by name rather than left to its
#' default. A formula over an empty index set is evaluated and yields no rows,
#' which is the right answer and is not refused.
#'
#' The values are computed on a local copy; the caller's model is unchanged.
#' @param model A model, possibly carrying formula parameters.
#' @param verbose Passed to [evaluate_parameters()].
#' @return The model, with formula parameters valued.
#' @keywords internal
#' @noRd
.mx_materialise_formulas <- function(model, verbose = FALSE) {
  has_fml <- function(p) !is.null(p$formula)
  # a table on disk or behind a reference counts as data
  no_data <- function(p) (is.null(p$data) || NROW(p$data) == 0L) && !.claims_external_data(p)
  pend <- vapply(model$parameters, function(p) has_fml(p) && no_data(p),
                 logical(1))
  if (!any(pend)) return(model)

  out <- tryCatch(evaluate_parameters(model, verbose = verbose),
                  error = function(e) {
                    stop("as_matrix: could not compute the parameter(s) ",
                         "defined by a formula (",
                         paste(names(model$parameters)[pend], collapse = ", "),
                         "): ", conditionMessage(e), call. = FALSE)
                  })

  still <- vapply(out$parameters, function(p) has_fml(p) && no_data(p),
                  logical(1))
  if (any(still)) {
    stop("as_matrix: parameter(s) ",
         paste(names(out$parameters)[still], collapse = ", "),
         " are defined by a formula but still hold no values, so the matrix ",
         "would be built from their scalar default instead. Compute them ",
         "with evaluate_parameters() and check the inputs the formula ",
         "refers to.", call. = FALSE)
  }
  out
}

#' Resolve lag and lead indices in a reference to the members they point at
#'
#' `vStock[r, y-1]` refers to a different column than `vStock[r, y]`, so the
#' environment's `y` is rewritten to the member one position earlier before the
#' join. The offset counts positions in the set's member order, which is why
#' that order is preserved end to end.
#'
#' A linear lag that runs off the end has no member to point at, and the row is
#' dropped: the term simply does not exist, which is what GAMS's `y-1` and
#' MathProg's arithmetic both do. A circular one wraps instead.
#' @param node A reference node whose `dims` may contain `shift` entries.
#' @param env Bound index tuples.
#' @param ctx Evaluation context.
#' @return `env`, with shifted columns rewritten and out-of-range rows removed.
#' @keywords internal
#' @noRd
.mx_shift_env <- function(node, env, ctx) {
  d <- node$dims
  if (is.null(d) || !length(d)) return(env)
  if (!any(vapply(d, function(e) inherits(e, "shift"), logical(1)))) return(env)

  out <- env
  for (e in d) {
    if (!inherits(e, "shift")) next
    nm <- .mx_env_name(ctx, out, as.character(e$symbol)[1])
    if (!nm %in% names(out)) next        # unbound; the caller reports it
    members <- .mx_set_members(ctx, .mx_base_set(ctx$model, nm))
    if (!length(members)) return(out[0])

    pos <- match(out[[nm]], members)
    tgt <- pos + as.integer(e$offset)
    if (isTRUE(e$circular)) {
      tgt <- ((tgt - 1L) %% length(members)) + 1L
    }
    ok <- !is.na(tgt) & tgt >= 1L & tgt <= length(members)
    out <- data.table::copy(out[ok])
    if (!nrow(out)) return(out)
    data.table::set(out, j = nm, value = members[tgt[ok]])
  }
  out
}

#' Reference index names, as the environment actually binds them
#'
#' A reference and the index that binds it need not be spelled the same: a
#' MathProg sum binds the iterator (`sum{l in L}` binds `l`) while the body may
#' name the set (`x[L]`), and the model's alias groups say the two are one
#' index. Resolving here keeps the join in step with the binding.
#' @keywords internal
#' @noRd
.mx_ref_names <- function(ref, env, ctx) {
  if (!length(ref)) return(ref)
  vapply(ref, function(x) .mx_env_name(ctx, env, x), character(1),
         USE.NAMES = FALSE)
}

#' Collapse terms to one value per env row
#'
#' A constant expression arrives as several terms sharing a `.k` - `1 + r` is
#' two - which assembly would sum. Any operator that does not distribute over
#' that sum has to do the summing first.
#' @keywords internal
#' @noRd
.mx_reduce <- function(tm, env) {
  v <- rep(0, nrow(env))
  if (nrow(tm)) {
    agg <- tm[, list(v = sum(coef)), by = ".k"]
    v[match(agg$.k, env$.k)] <- agg$v
  }
  v
}

#' Scale terms by a per-env-row constant
#' @keywords internal
#' @noRd
.mx_scale <- function(tm, cst, op) {
  if (!nrow(tm)) return(.mx_no_terms())
  # collapse the constant side to one value per env row
  cv <- cst[, .(v = sum(coef)), by = .k]
  out <- cv[tm, on = ".k"]
  v <- out$v
  v[is.na(v)] <- 0
  cf <- switch(op, "*" = out$coef * v, "/" = out$coef / v, "^" = out$coef ^ v)
  data.table::data.table(.k = out$.k, j = out$j, coef = cf)
}

#' @keywords internal
#' @noRd
.mx_eval_when <- function(node, env, ctx) {
  keep <- .mx_condition(node$condition, env, ctx)
  sub <- env[keep]
  then_tm <- if (nrow(sub)) .mx_eval(node$then, sub, ctx) else .mx_no_terms()
  if (is.null(node$otherwise)) return(then_tm)
  other <- env[!keep]
  if (!nrow(other)) return(then_tm)
  data.table::rbindlist(
    list(then_tm, .mx_eval(node$otherwise, other, ctx)), use.names = TRUE
  )
}

#' @keywords internal
#' @noRd
.mx_eval_func <- function(node, env, ctx) {
  fn <- tolower(node$name)
  if (fn == "sum")  return(.mx_eval_sum(node, env, ctx))
  if (fn == "prod") return(.mx_eval_prod(node, env, ctx))
  if (fn == "ord")  return(.mx_eval_ord(node, env, ctx))
  if (fn == "val")  return(.mx_eval_val(node, env, ctx))
  stop("as_matrix: unsupported function '", node$name, "'.")
}

#' `<set>.val` - numeric value of the bound member (GAMS semantics: the
#' label read as a number, e.g. year "2030" -> 2030). Used by generated
#' vintage/lifespan window arithmetic in user constraints.
#' @keywords internal
#' @noRd
.mx_eval_val <- function(node, env, ctx) {
  arg <- node$value
  if (is.list(arg) && !inherits(arg, "ast") && length(arg)) arg <- arg[[1]]
  set_name <- .mx_env_name(ctx, env, dim_binding_name(arg))
  if (!set_name %in% names(env)) {
    stop("as_matrix: val(", set_name, ") refers to an unbound index.")
  }
  lab <- .mx_decode(ctx, env[[set_name]])
  v <- suppressWarnings(as.numeric(lab))
  if (anyNA(v)) {
    stop("as_matrix: val(", set_name, ") - member(s) ",
         paste(utils::head(unique(lab[is.na(v)]), 3), collapse = ", "),
         " are not numeric labels.")
  }
  data.table::data.table(.k = env$.k, j = NA_integer_, coef = v)
}

#' Resolve an expression's index name to the env binding, through the
#' model's short index aliases (year -> y) and alias groups (yearp -> year).
#' @keywords internal
#' @noRd
.mx_env_name <- function(ctx, env, set_name) {
  if (set_name %in% names(env)) return(set_name)
  ia <- ctx$model$index_aliases
  if (!is.null(ia)) {
    base <- names(ia)[ia == set_name]
    if (length(base) && base[1] %in% names(env)) return(base[1])
  }
  if (!is.null(ctx$model$aliases)) {
    for (grp in ctx$model$aliases) {
      if (set_name %in% grp) {
        hit <- intersect(unlist(grp), names(env))
        if (length(hit)) return(hit[1])
      }
    }
  }
  set_name
}

#' `ord(set)` - 1-based position of the bound member within its set
#' @keywords internal
#' @noRd
.mx_eval_ord <- function(node, env, ctx) {
  arg <- node$value
  if (is.list(arg) && !inherits(arg, "ast") && length(arg)) arg <- arg[[1]]
  set_name <- .mx_env_name(ctx, env, dim_binding_name(arg))
  if (!set_name %in% names(env)) {
    stop("as_matrix: ord(", set_name, ") refers to an unbound index.")
  }
  members <- .mx_set_members(ctx, .mx_base_set(ctx$model, set_name))
  pos <- match(env[[set_name]], members)
  if (anyNA(pos)) {
    stop("as_matrix: ord(", set_name, ") - some bound members are not in the set.")
  }
  data.table::data.table(.k = env$.k, j = NA_integer_, coef = as.numeric(pos))
}

#' @keywords internal
#' @noRd
.mx_eval_prod <- function(node, env, ctx) {
  inner <- .mx_sum_env(node, env, ctx)
  if (is.null(inner) || !nrow(inner$env)) return(.mx_const(env, 1))
  tm <- .mx_eval(node$value, inner$env, ctx)
  if (!.mx_is_const(tm)) {
    stop("as_matrix: prod() over variable-bearing terms is non-linear.")
  }
  if (!nrow(tm)) return(.mx_const(env, 1))
  per <- tm[, .(v = sum(coef)), by = .k]                    # value per inner row
  per <- inner$map[per, on = ".k"]                          # attach outer row
  agg <- per[, .(coef = prod(v)), by = .(.k = .outer)]
  out <- agg[data.table::data.table(.k = env$.k), on = ".k"]
  cf <- out$coef
  cf[is.na(cf)] <- 1                                        # empty product = 1
  data.table::data.table(.k = out$.k, j = NA_integer_, coef = cf)
}

#' @keywords internal
#' @noRd
.mx_eval_sum <- function(node, env, ctx) {
  inner <- .mx_sum_env(node, env, ctx)
  if (is.null(inner) || !nrow(inner$env)) return(.mx_no_terms())
  tm <- .mx_eval(node$value, inner$env, ctx)
  if (!nrow(tm)) return(.mx_no_terms())
  .mx_trace(ctx, "groupsum", list(map = inner$map, tm = tm))
  tm <- inner$map[tm, on = ".k"]
  tm[, .(coef = sum(coef)), by = .(.k = .outer, j)]
}

#' Build the inner environment of a sum/prod
#'
#' Returns the expanded environment plus a map from inner `.k` to the outer
#' `.k` it came from.
#' @keywords internal
#' @noRd
.mx_sum_env <- function(node, env, ctx) {
  idx <- node$index
  cond <- NULL
  if (inherits(idx, "when")) {         # GAMS: sum(i$cond, expr)
    cond <- idx$condition
    idx <- idx$then
  }
  if (is.null(cond) && !is.null(node$domain)) cond <- node$domain  # ast_sum(domain=)
  iters <- .mx_index_names(idx)
  if (!length(iters)) return(NULL)

  # A shallow rewrap: the column vectors are shared with `env` and never
  # written to; renaming and adding columns touch this table only.
  cur <- data.table::setDT(as.list(env))
  # An enclosing sum already left its own `.outer` here. Nesting must not stack
  # them: each level maps only from its own `.k` to its parent, so drop the
  # grandparent marker before claiming the name. Leaving it produced two columns
  # called `.outer` and silently mis-mapped nested sums back to the wrong rows.
  if (".outer" %in% names(cur)) data.table::set(cur, j = ".outer", value = NULL)
  data.table::setnames(cur, ".k", ".outer")

  # Bind the summation index by JOINING the gating map, not by expanding the
  # full Cartesian product and filtering afterwards. `sum(tech$mTechInpComm(...))`
  # over 2,000 technologies would otherwise materialise env x 2,000 rows before
  # discarding almost all of them - on a large model that exceeds R's 2^31 row
  # limit outright. The map already holds exactly the valid combinations.
  #
  # Candidates are the sum's own conditions plus the mapping gates of its body
  # (`sum(i$A, x$B)` sums over i in A and B, so B may bind i too). Each round
  # first applies every mapping condition whose indices are all bound (a
  # semi-join: rows only shrink), then joins the candidate whose exact output
  # size - counted per shared key before anything is built - is smallest. A
  # join above the row budget runs in chunks, each filtered before the next.
  conds <- .mx_flatten_and(cond)
  gates <- .mx_body_gates(node$value)
  cands <- c(conds, gates)
  own <- seq_along(cands) <= length(conds)
  used <- rep(FALSE, length(cands))
  repeat {
    for (n in which(!used)) {
      f <- .mx_semi_join(cur, cands[[n]], ctx, iters, resolve = own[n])
      if (!is.null(f)) {
        cur <- f
        used[n] <- TRUE
        if (!nrow(cur)) return(NULL)
      }
    }
    if (all(iters %in% names(cur))) break
    best <- NULL
    for (n in which(!used)) {
      pl <- .mx_join_plan(cur, cands[[n]], iters, ctx)
      if (!is.null(pl) && (is.null(best) || pl$n < best$n)) {
        best <- pl
        best$i <- n
      }
    }
    if (is.null(best)) break
    used[best$i] <- TRUE
    rest <- cands[!used]
    cur <- .mx_budget_merge(cur, best, rest, own[!used], iters, ctx)
    if (!nrow(cur)) return(NULL)
  }
  used <- used[own]

  # Anything still unbound has no map to drive it: expand densely, with a guard.
  remaining <- setdiff(iters, names(cur))
  if (length(remaining)) {
    cur <- .mx_cross_dense(cur, remaining, ctx, sets = .mx_index_sets(idx))
    if (is.null(cur) || !nrow(cur)) return(NULL)
  }

  # `.k` must exist before any condition is evaluated: evaluating a value
  # predicate walks the AST against this environment and keys its result by `.k`.
  data.table::set(cur, j = ".k", value = seq_len(nrow(cur)))

  # Apply whatever conditions the joins did not already consume. Filtering keeps
  # the surviving `.k` values, which stay unique - that is all the map needs.
  for (n in which(!used)) {
    keep <- .mx_condition(conds[[n]], cur, ctx)
    cur <- cur[keep]
    if (!nrow(cur)) return(NULL)
  }

  list(env = cur, map = cur[, .(.k, .outer)])
}

#' Split an `and` chain into its individual conditions
#' @keywords internal
#' @noRd
.mx_flatten_and <- function(cond) {
  if (is.null(cond)) return(list())
  if (inherits(cond, "expression") && cond$op %in% c("and", "&", "&&")) {
    return(c(.mx_flatten_and(cond$lhs), .mx_flatten_and(cond$rhs)))
  }
  list(cond)
}

#' Bind new iterators by inner-joining a mapping, when that is possible
#'
#' Returns the expanded table, or NULL when this condition cannot drive the
#' binding (not a mapping, nothing in common with what is already bound, or it
#' would bind an index that is not a summation iterator).
#' @keywords internal
#' @noRd
.mx_join_cond <- function(cur, cond, iters, ctx) {
  if (!inherits(cond, "mm_mapping")) return(NULL)
  nm <- cond$name
  dat <- .mx_map_data(nm, ctx)

  # Emptiness is decided BEFORE arity, and before asking what this map would
  # bind. An unlinked mapping arrives as a data.table with zero COLUMNS, so an
  # arity test placed first returns NULL - and the caller then reads that as
  # "no map constrains this index" and falls through to .mx_cross_dense(),
  # which on a large model tries to materialise billions of rows. The
  # conditions reaching here are `and`-conjuncts, so a map holding no tuples
  # makes the whole sum empty whatever it would have bound.
  if (!nrow(dat)) return(cur[0])

  ref <- .mx_ref_dims(cond)
  if (!length(ref) || length(names(dat)) != length(ref)) return(NULL)

  shared <- intersect(ref, names(cur))
  newc <- setdiff(ref, names(cur))
  if (!length(shared)) return(NULL)          # not a filter on what we have
  if (!length(newc)) return(NULL)            # binds nothing new - leave as a filter
  if (!all(newc %in% iters)) return(NULL)    # would bind a non-iterator index

  merge(cur, .mx_map_named(dat, ref), by = shared, allow.cartesian = TRUE)
}

#' A mapping's prepared table under the reference's index names
#'
#' A shallow rewrap of the cached table, renamed; the column vectors are
#' shared and never written to.
#' @keywords internal
#' @noRd
.mx_map_named <- function(dat, ref) {
  lk <- data.table::setDT(as.list(dat))
  data.table::setnames(lk, names(lk), ref)
  lk
}

#' Mapping gates that zero a sum's whole body
#'
#' `x$m` without an else branch, or a product with such a factor, is zero
#' wherever `m` fails, so `m` may restrict the summation index as well.
#' @keywords internal
#' @noRd
.mx_body_gates <- function(v) {
  if (is.null(v)) return(list())
  if (inherits(v, "when") && is.null(v$otherwise)) {
    cs <- .mx_flatten_and(v$condition)
    return(c(Filter(function(x) inherits(x, "mm_mapping"), cs),
             .mx_body_gates(v$then)))
  }
  if (inherits(v, "expression") && identical(v$op, "*")) {
    return(c(.mx_body_gates(v$lhs), .mx_body_gates(v$rhs)))
  }
  if (inherits(v, "expression") && identical(v$op, "/")) {
    return(.mx_body_gates(v$lhs))
  }
  list()
}

#' Filter by a mapping whose indices are all bound
#'
#' Returns the surviving rows, or NULL when the condition is not a mapping or
#' references an index that is not bound yet - including a summation index
#' `iters` still to be bound, which alias resolution would otherwise read as a
#' bound one (`yearp` as `year`). `resolve = FALSE` matches index
#' names literally: a body gate may name an alias (`timeslicep`) that an inner
#' sum binds later, which must not be read as the bound `timeslice`.
#' @keywords internal
#' @noRd
.mx_semi_join <- function(cur, cond, ctx, iters = character(), resolve = TRUE) {
  if (!inherits(cond, "mm_mapping")) return(NULL)
  ref <- .mx_ref_dims(cond)
  if (any(ref %in% setdiff(iters, names(cur)))) return(NULL)
  dat <- .mx_map_data(cond$name, ctx)
  if (resolve) ref <- .mx_ref_names(ref, cur, ctx)
  if (!length(ref) || anyDuplicated(ref) || !all(ref %in% names(cur))) {
    return(NULL)
  }
  if (!nrow(dat)) return(cur[0])
  if (length(names(dat)) != length(ref)) return(NULL)
  # the prepared table is unique over all its columns, so each row of `cur`
  # matches at most once: `which` gives the match per row, in `cur`'s order
  .mx_trace(ctx, "semi", list(cur = cur, lk = dat),
            list(on = as.list(stats::setNames(ref, names(dat)))))
  hit <- dat[cur, on = stats::setNames(ref, names(dat)), which = TRUE]
  cur[!is.na(hit)]
}

#' Plan an index-binding join: its lookup table and exact output row count
#' @keywords internal
#' @noRd
.mx_join_plan <- function(cur, cond, iters, ctx) {
  if (!inherits(cond, "mm_mapping")) return(NULL)
  dat <- .mx_map_data(cond$name, ctx)
  if (!nrow(dat)) return(list(lk = NULL, shared = character(), n = 0,
                              name = cond$name))
  ref <- .mx_ref_dims(cond)
  if (!length(ref) || length(names(dat)) != length(ref)) return(NULL)
  shared <- intersect(ref, names(cur))
  newc <- setdiff(ref, names(cur))
  if (!length(shared) || !length(newc) || !all(newc %in% iters)) return(NULL)
  lk <- .mx_map_named(dat, ref)
  a <- cur[, .N, by = shared]
  b <- lk[, .N, by = shared]
  m <- b[a, on = shared, nomatch = NULL]
  list(lk = lk, shared = shared, name = cond$name,
       n = sum(as.numeric(m$N) * as.numeric(m$i.N)))
}

#' Row budget for one join step (`options(multimod.join_rows = )`)
#' @keywords internal
#' @noRd
.mx_join_budget <- function() {
  as.numeric(getOption("multimod.join_rows", 5e7))
}

#' Execute a planned join within the row budget
#'
#' Rows of `cur` are taken in blocks whose exact join output fits the budget;
#' each block is joined and immediately filtered by the remaining candidates
#' that its new columns complete, then the blocks are bound together.
#' @keywords internal
#' @noRd
.mx_budget_merge <- function(cur, plan, rest, resolve, iters, ctx) {
  if (is.null(plan$lk)) return(cur[0])
  budget <- .mx_join_budget()
  sh <- plan$shared
  join1 <- function(x) {
    .mx_trace(ctx, "merge", list(x = x, lk = plan$lk), list(by = as.list(sh)))
    out <- merge(x, plan$lk, by = sh, allow.cartesian = TRUE)
    for (k in seq_along(rest)) {
      if (!nrow(out)) break
      f <- .mx_semi_join(out, rest[[k]], ctx, iters, resolve = resolve[k])
      if (!is.null(f)) out <- f
    }
    out
  }
  if (plan$n <= budget) return(join1(cur))
  cnt <- plan$lk[, .N, by = sh]
  w <- cnt[cur[, sh, with = FALSE], on = sh]$N
  w[is.na(w)] <- 0
  blk <- pmax(1, ceiling(cumsum(as.numeric(w)) / budget))
  big <- max(w)
  if (big > 2^31 - 1) {
    stop("as_matrix: joining mapping '", plan$name, "' on (",
         paste(sh, collapse = ", "),
         ") gives ", format(big, big.mark = ","), " rows for a single row of ",
         "the equation, beyond R's vector limit.", call. = FALSE)
  }
  parts <- lapply(split(seq_len(nrow(cur)), blk), function(ix) join1(cur[ix]))
  out <- data.table::rbindlist(parts, use.names = TRUE)
  if (nrow(out) > 2^31 - 1) {
    stop("as_matrix: a summation needs ", format(nrow(out), big.mark = ","),
         " rows after filtering, beyond R's vector limit.", call. = FALSE)
  }
  out
}

#' Replicate rows once per member of each still-unbound iterator
#'
#' The dense fallback, used only when no mapping constrains the iterator. Guards
#' the 2^31 row limit so an oversized expansion reports what it was doing rather
#' than failing with "long vectors not supported yet".
#' @keywords internal
#' @noRd
.mx_cross_dense <- function(cur, iters, ctx, sets = NULL) {
  cur <- data.table::copy(cur)
  for (it in iters) {
    if (it %in% names(cur)) next
    base <- if (!is.null(sets) && it %in% names(sets)) sets[[it]] else it
    members <- .mx_set_members(ctx, .mx_base_set(ctx$model, base))
    if (!length(members)) return(NULL)
    n_out <- as.numeric(nrow(cur)) * length(members)
    if (n_out > 2^31 - 1) {
      stop("as_matrix: expanding the summation index '", it, "' densely would ",
           "need ", format(n_out, big.mark = ","), " rows, beyond R's vector ",
           "limit.
  No mapping constrains this index, so the sum cannot be ",
           "kept sparse. Give it a gating map, or narrow the equation domain.")
    }
    # replicate each bound row once per member of the new iterator; copy()
    # because a rep-subset is a shallow copy and set() would warn on it
    cur <- data.table::copy(cur[rep(seq_len(nrow(cur)), each = length(members))])
    data.table::set(cur, j = it,
                    value = rep(members, times = nrow(cur) / length(members)))
  }
  cur[]
}

#' Iterator names paired with the sets they range over
#'
#' MathProg writes `sum{t in TECHNOLOGY}`: the entry's name is the iterator the
#' body binds to, and the entry itself names the set whose members it takes.
#' The two differ whenever the model does not declare the iterator as an alias
#' (`sum{yy in YEAR}`), so the members cannot be found from the name alone. A
#' GAMS sum leaves entries unnamed and both come out the same.
#' @return Character vector of set names, named by iterator.
#' @keywords internal
#' @noRd
.mx_index_sets <- function(idx) {
  nms <- .mx_index_names(idx)
  if (!length(nms)) return(character())
  sets <- if (inherits(idx, "dims")) {
    vapply(idx, dim_binding_name, character(1), USE.NAMES = FALSE)
  } else {
    dim_binding_name(idx)
  }
  if (length(sets) != length(nms)) sets <- nms
  stats::setNames(sets, nms)
}

#' Iterator names introduced by a sum/prod index
#' @keywords internal
#' @noRd
.mx_index_names <- function(idx) {
  if (is.null(idx)) return(character())
  if (inherits(idx, "dims")) {
    out <- vapply(idx, dim_binding_name, character(1), USE.NAMES = FALSE)
    # A MathProg sum names its iterators - `sum{t in TECHNOLOGY}` - and the
    # body binds to those names, not to the set's. The entry name carries the
    # iterator, while dim_binding_name() reads the set it ranges over. A GAMS
    # sum leaves the entries unnamed, so nothing changes there.
    nms <- names(idx)
    if (!is.null(nms)) {
      use <- nzchar(nms) & !is.na(nms)
      out[use] <- nms[use]
    }
    return(out)
  }
  nm <- dim_binding_name(idx)
  if (nzchar(nm)) nm else character()
}

#' Keep the rows of an index block that satisfy an equation's condition
#'
#' A domain that is an expression rather than a mapping filters the index set
#' (MathProg's `{r in R: test}`). [build_row_index()] has already materialised
#' those rows, so the condition is evaluated over them with the same evaluator
#' the equation bodies use - no Cartesian expansion beyond the rows that exist.
#'
#' Index names come from the equation's iterator aliases when it has them: a
#' condition is written in terms of the iterators (`TradeRoute[r,rr,f,y]`), and
#' a set that repeats would otherwise give two columns of the same name.
#' @param eq The equation, carrying `$domain` and `$dims_index_aliases`.
#' @param blk Index block with `i1..iN` columns and a `dim_names` attribute.
#' @param model The model, for parameter and set data.
#' @param codes The dictionary `blk` is coded with, or `NULL` when it holds
#'   labels.
#' @return Logical vector over the rows of `blk`.
#' @keywords internal
#' @noRd
.mx_filter_rows <- function(eq, blk, model, codes = NULL) {
  dims <- .mx_eq_dims(eq)
  if (!length(dims)) {
    dims <- attr(blk, "dim_names")
    if (is.null(dims)) dims <- character()
  }
  if (anyDuplicated(dims)) {
    stop("as_matrix: equation '", eq$name, "' binds duplicate index name(s): ",
         paste(unique(dims[duplicated(dims)]), collapse = ", "),
         ". Its condition cannot be resolved without distinct iterator names.",
         call. = FALSE)
  }

  ctx <- new.env(parent = emptyenv())
  ctx$model <- model
  ctx$codes <- if (.mx_is_codes(codes)) codes else .mx_new_codes()
  ctx$param_cache <- list()
  ctx$map_cache <- list()
  ctx$set_cache <- list()

  env <- data.table::data.table(.k = seq_len(nrow(blk)))
  idx <- paste0("i", seq_along(dims))
  for (n in seq_along(dims)) {
    env[[dims[n]]] <- if (.mx_is_codes(codes)) blk[[idx[n]]] else .mx_code(ctx, blk[[idx[n]]])
  }

  keep <- .mx_condition(eq$domain, env, ctx)
  if (!is.logical(keep) || length(keep) != nrow(blk)) {
    stop("as_matrix: the condition on equation '", eq$name, "' did not reduce ",
         "to one logical value per row.", call. = FALSE)
  }
  keep & !is.na(keep)
}

# --- conditions -------------------------------------------------------------

#' Evaluate a `$`-condition to a logical mask over env rows
#'
#' Conditions are overwhelmingly set membership (a mapping reference), which
#' becomes a semi-join. Comparisons and and/or are supported for the handful of
#' value predicates.
#' @keywords internal
#' @noRd
.mx_condition <- function(node, env, ctx) {
  if (is.null(node)) return(rep(TRUE, nrow(env)))
  cls <- sub("^mm_", "", class(node)[1])

  if (cls == "mapping") return(.mx_membership(node, env, ctx))

  if (cls == "unary" && identical(node$op, "not")) {
    return(!.mx_condition(node$rhs, env, ctx))
  }

  if (cls == "expression") {
    op <- node$op
    if (op %in% c("and", "&", "&&")) {
      return(.mx_condition(node$lhs, env, ctx) & .mx_condition(node$rhs, env, ctx))
    }
    if (op %in% c("or", "|", "||")) {
      return(.mx_condition(node$lhs, env, ctx) | .mx_condition(node$rhs, env, ctx))
    }
    if (op %in% c("<", "<=", ">", ">=", "=", "==", "<>", "!=")) {
      l <- .mx_numeric(node$lhs, env, ctx)
      r <- .mx_numeric(node$rhs, env, ctx)
      return(switch(
        op,
        "<" = l < r, "<=" = l <= r, ">" = l > r, ">=" = l >= r,
        "=" = l == r, "==" = l == r, "<>" = l != r, "!=" = l != r
      ))
    }
    stop("as_matrix: unsupported operator '", op, "' in a condition.")
  }

  # a bare symbol/set used as a condition is a membership test we cannot resolve
  .mx_unsupported(node, "a condition")
}

#' Tuple table of a mapping, prepared once, with the two failure modes kept apart
#'
#' A mapping the model never declared is a model error and must say so. A
#' declared mapping holding no tuples is legitimate - it means the gated
#' expression is empty - and returns a zero-row table. Keep the two apart:
#' conflating them turns a missing map into "no map constrains this index".
#'
#' The table comes back with its members coded, its tuples unique and its
#' columns named `.c1..cN`; callers join on positions.
#' @keywords internal
#' @noRd
.mx_map_data <- function(nm, ctx) {
  dat <- ctx$map_cache[[nm]]
  if (!is.null(dat)) return(dat)
  if (is.null(ctx$model$mappings[[nm]])) {
    stop("as_matrix: mapping '", nm, "' is referenced but not declared in the",
         " model.\n  A gating map that does not exist cannot be treated as",
         " unconstrained - the sum would expand densely over every member.")
  }
  dat <- get_data(ctx$model, nm, type = "mapping")
  dat <- if (is.null(dat)) data.table::data.table() else data.table::as.data.table(dat)
  if (length(names(dat))) {
    keys <- paste0(".c", seq_along(names(dat)))
    dat <- unique(.mx_code_cols(ctx, dat, names(dat), keys), by = keys)
    data.table::setkeyv(dat, keys)
  }
  ctx$map_cache[[nm]] <- dat
  dat
}

#' Set-membership test: is the referenced tuple in the mapping?
#' @keywords internal
#' @noRd
.mx_membership <- function(node, env, ctx) {
  nm <- node$name
  dat <- .mx_map_data(nm, ctx)
  if (!nrow(dat)) return(rep(FALSE, nrow(env)))   # empty map: nothing qualifies

  ref <- .mx_ref_names(.mx_ref_dims(node), env, ctx)
  cols <- names(dat)
  if (length(cols) != length(ref)) {
    stop("as_matrix: mapping '", nm, "' has ", length(cols),
         " column(s) but is referenced with ", length(ref), " index/indices.")
  }
  miss <- setdiff(ref, names(env))
  if (length(miss)) {
    stop("as_matrix: mapping '", nm, "' references unbound index/indices: ",
         paste(miss, collapse = ", "), ".")
  }

  # unique over all columns, so each env row matches at most once
  .mx_trace(ctx, "semi", list(cur = env, lk = dat),
            list(on = as.list(stats::setNames(ref, cols))))
  !is.na(dat[env, on = stats::setNames(ref, cols), which = TRUE])
}

#' Evaluate a purely numeric sub-expression to one value per env row
#' @keywords internal
#' @noRd
.mx_numeric <- function(node, env, ctx) {
  tm <- .mx_eval(node, env, ctx)
  if (!.mx_is_const(tm)) {
    stop("as_matrix: a condition compared against a variable-bearing expression.")
  }
  v <- tm[, .(v = sum(coef)), by = .k]
  out <- v[data.table::data.table(.k = env$.k), on = ".k"]
  ifelse(is.na(out$v), 0, out$v)
}

# --- sets -------------------------------------------------------------------

#' Base set behind an alias (yearp -> year)
#' @keywords internal
#' @noRd
.mx_base_set <- function(model, name) {
  if (!is.null(model$aliases)) {
    for (grp in model$aliases) if (name %in% grp) return(grp[[1]])
  }
  name
}

#' Members of a set as codes, in member order, cached
#' @keywords internal
#' @noRd
.mx_set_members <- function(ctx, set_name) {
  hit <- ctx$set_cache[[set_name]]
  if (!is.null(hit)) return(hit)
  s <- ctx$model$sets[[set_name]]
  mem <- if (is.null(s)) integer() else
    .mx_code(ctx, as.character(extract_set_members(s)))
  ctx$set_cache[[set_name]] <- mem
  mem
}

# --- equation assembly ------------------------------------------------------

utils::globalVariables(c(".k", ".outer", ".I", "coef", "j", "v", "seq_len", "symbol"))

#' Environment of bound indices for one equation
#'
#' Binding names come from the equation's **domain condition**, not its
#' declaration. GAMS lets an equation be declared with one set of index names
#' and defined with another, and the body binds to the definition's names:
#'
#'     eqImportTot(comm, region, year, timeslice)                    # declared
#'     eqImportTot(comm, dst,    year, timeslice)$mImport(comm, dst, ...)  # defined
#'
#' The domain node carries the definition's names, so it is the correct source.
#' It also disambiguates a set that repeats - the trade equations declare
#' `(trade, comm, region, region, ...)` but define `(trade, comm, src, dst, ...)`,
#' and two columns both called `region` could not be bound at all.
#'
#' Names are matched positionally to the domain mapping's columns (which may
#' carry de-duplicated names such as `comm.1` where a set repeats).
#' @keywords internal
#' @noRd
NULL

#' The index names an equation's body binds to
#'
#' A mapping domain carries the definition's names and is authoritative. A
#' filter domain has no such node, and the body is then written in terms of the
#' iterators the equation declared - `SpecifiedAnnualDemand[r,f,y]` - so those
#' are used: the declared set names would not match the references, and a set
#' that repeats could not be bound at all.
#' @keywords internal
#' @noRd
.mx_eq_dims <- function(eq) {
  dims <- .mx_ref_dims(eq$domain)
  if (length(dims)) return(dims)
  decl <- if (is.null(eq$dims)) character() else
    vapply(eq$dims, dim_binding_name, character(1), USE.NAMES = FALSE)
  al <- eq$dims_index_aliases
  if (!is.null(al) && length(al) == length(decl) &&
      all(nzchar(al)) && !anyDuplicated(unname(al))) {
    return(unname(al))
  }
  decl
}

.mx_equation_env <- function(eq, rows) {
  dims <- .mx_eq_dims(eq)
  if (anyDuplicated(dims)) {
    stop("as_matrix: equation '", eq$name, "' binds duplicate index name(s): ",
         paste(unique(dims[duplicated(dims)]), collapse = ", "),
         ". The domain condition should give each position a distinct name.")
  }
  env <- data.table::data.table(.k = rows$i)
  if (!length(dims)) return(env)
  idx <- paste0("i", seq_along(dims))
  if (!all(idx %in% names(rows))) {
    stop("as_matrix: row index has fewer index columns than equation '",
         eq$name, "' declares.")
  }
  for (n in seq_along(dims)) env[[dims[n]]] <- rows[[idx[n]]]
  env
}

#' Extract (row, column, coefficient) triplets from a model
#'
#' Walks each equation's AST set-at-a-time and returns the LP matrix entries
#' together with the right-hand side implied by the equation's constant terms.
#'
#' @param model A multimod model with data attached.
#' @param col_index,row_index Index tables from [build_col_index()] /
#'   [build_row_index()]. Built on demand when not supplied.
#' @param equations Optional character vector limiting which equations to
#'   process; useful when bringing the backend up equation by equation.
#' @param on_duplicate What to do when a parameter stores the same index tuple
#'   with more than one distinct value. `"error"` (default) refuses, because the
#'   lookup is genuinely ambiguous; `"last"` takes the last, matching what GAMS
#'   and Pyomo do silently; `"first"` takes the first. Either non-default choice
#'   reports the parameter and the number of tuples affected.
#' @param verbose Logical; report per-equation term counts and timings.
#' @param chunk_rows Integer; evaluate each equation over at most this many LP
#'   rows at a time. Each LP row's terms are independent, so slicing by row is
#'   exact; what it bounds is the pre-aggregation intermediate of a `sum()`
#'   (outer rows x inner tuples), which on a large full-year model otherwise
#'   allocates tens of gigabytes in one piece. The aggregated per-chunk
#'   triplets concatenate without renumbering (`.k` carries the global row id).
#'
#' @param sink Internal: an open streaming sink (see [write_mps()] with
#'   `stream = TRUE`). The matrix entries are written to it as each chunk is
#'   built and `triplets` comes back empty; `rhs` is returned as usual.
#'
#' @return A list with `triplets` (`data.table(i, j, coef)`, aggregated), `rhs`
#'   (`data.table(i, rhs)`), and `row_index` / `col_index` as used.
#' @export
build_triplets <- function(model, col_index = NULL, row_index = NULL,
                           equations = NULL,
                           on_duplicate = c("error", "last", "first"),
                           verbose = FALSE, chunk_rows = 1000000L, sink = NULL) {
  on_duplicate <- match.arg(on_duplicate)
  stopifnot(inherits(model, "multimod") || inherits(model, "mm_model"))
  stopifnot(is.numeric(chunk_rows), length(chunk_rows) == 1, chunk_rows >= 1)
  chunk_rows <- as.integer(chunk_rows)
  if (is.null(col_index)) col_index <- build_col_index(model)
  if (is.null(row_index)) row_index <- build_row_index(model)

  ctx <- new.env(parent = emptyenv())
  ctx$model <- model
  # index tables built with a dictionary (`codes =`) bring it; both must
  # share it, as the column codes are joined against the row codes
  own <- attr(row_index, "codes")
  if (.mx_is_codes(own) && !identical(attr(col_index, "codes"), own)) {
    stop("build_triplets: the row and column index tables are coded with ",
         "different dictionaries.", call. = FALSE)
  }
  ctx$codes <- if (.mx_is_codes(own)) own else .mx_new_codes()
  ctx$col_index <- col_index
  ctx$col_cache <- list()
  ctx$param_cache <- list()
  ctx$map_cache <- list()
  ctx$set_cache <- list()
  ctx$on_duplicate <- on_duplicate
  ctx$dup_warned <- list()
  ctx$trace <- .mx_trace_open(getOption("multimod.mx_trace"))

  # The row index with its members coded, and each equation's row range: the
  # rows of one equation are contiguous, so a range replaces a scan of the
  # symbol column per equation.
  rows_coded <- .mx_index_coded(ctx, row_index, c("i", "symbol"))
  ranges <- rows_coded[, list(from = .I[1L], to = .I[.N]), by = "symbol"]

  todo <- if (is.null(equations)) ranges$symbol else
    intersect(equations, ranges$symbol)

  tri <- vector("list", length(todo))
  rhs <- vector("list", length(todo))
  # `options(multimod.engine = "native")` runs the equations the block-plan
  # lowering covers on the native executor; the others stay on this engine.
  engine <- getOption("multimod.engine", "r")
  native <- list()
  ctx$lower_why <- list()
  ctx$native_stats <- list()
  cache_limit <- .mx_cache_limit()
  gc_rows <- 0L

  for (n in seq_along(todo)) {
    nm <- todo[n]
    eq <- model$equations[[nm]]
    if (is.null(eq)) next
    ctx$trace_eq <- nm
    rg <- ranges[symbol == nm]
    lowered <- if (identical(engine, "native")) .mx_lower_equation(eq, ctx) else NULL
    native[[nm]] <- !is.null(lowered)

    t0 <- Sys.time()
    nr <- rg$to - rg$from + 1L
    n_chunks <- max(1L, as.integer(ceiling(nr / chunk_rows)))
    mats <- vector("list", n_chunks)
    csts <- vector("list", n_chunks)
    for (ck in seq_len(n_chunks)) {
      # each chunk sliced straight from the coded row table: no copy of the
      # equation's rows as a whole
      sl <- rows_coded[(rg$from + (ck - 1L) * chunk_rows):min(rg$from + ck * chunk_rows - 1L, rg$to)]
      if (!is.null(lowered)) {
        blk <- .mx_exec_block(lowered, sl, ctx, sink)
        mats[[ck]] <- blk$mat
        csts[[ck]] <- blk$cst
        st <- ctx$native_stats[[nm]]
        ctx$native_stats[[nm]] <- if (is.null(st)) blk$stats else {
          all <- union(names(st), names(blk$stats))
          stats::setNames(ifelse(is.na(st[all]), 0, st[all]) +
                            ifelse(is.na(blk$stats[all]), 0, blk$stats[all]), all)
        }
        next
      }
      env <- .mx_equation_env(eq, sl)
      lhs <- .mx_eval(eq$lhs, env, ctx)
      rhsv <- .mx_eval(eq$rhs, env, ctx)
      if (nrow(rhsv)) {
        rhsv <- data.table::data.table(.k = rhsv$.k, j = rhsv$j,
                                       coef = -rhsv$coef)
      }
      all_tm <- data.table::rbindlist(list(lhs, rhsv), use.names = TRUE)

      if (nrow(all_tm)) {
        mat <- all_tm[!is.na(j), .(coef = sum(coef)), by = .(i = .k, j)]
        mat <- mat[coef != 0]
        cst <- all_tm[is.na(j), .(rhs = -sum(coef)), by = .(i = .k)]
      } else {
        mat <- data.table::data.table(i = integer(), j = integer(),
                                      coef = numeric())
        cst <- data.table::data.table(i = integer(), rhs = numeric())
      }
      if (!is.null(sink)) {
        if (inherits(sink, "mm_scan")) mm_scan_add(sink, mat$i, mat$j, mat$coef) else
          mm_sink_write(sink, mat$i, mat$j, mat$coef)
        mat <- mat[0]
      }
      mats[[ck]] <- mat
      csts[[ck]] <- cst
    }
    tri[[n]] <- if (n_chunks == 1L) mats[[1L]] else
      data.table::rbindlist(mats, use.names = TRUE)
    rhs[[n]] <- if (n_chunks == 1L) csts[[1L]] else
      data.table::rbindlist(csts, use.names = TRUE)
    # the chunk tables and intermediates of the equations done so far are
    # collected once they amount to a few million rows of work, which keeps
    # the heap near the live data instead of growing between R's own
    # collections; with a cache budget, the executor's least recently used
    # indexes go too
    rm(mats, csts)
    gc_rows <- gc_rows + nr
    if (gc_rows > 2000000L) { gc(FALSE); gc_rows <- 0L }
    if (is.finite(cache_limit) && !is.null(ctx$ex)) mm_block_trim(ctx$ex, cache_limit)
    if (verbose) {
      message(sprintf("  %-30s rows=%-7d nnz=%-8d %.1fs", nm, nr,
                      nrow(tri[[n]]),
                      as.numeric(Sys.time() - t0, units = "secs")))
    }
  }

  if (verbose && !is.null(ctx$ex)) {
    message(sprintf("  executor index cache: %.2f GB", mm_block_cache(ctx$ex) / 2^30))
  }
  out <- list(
    triplets  = data.table::rbindlist(tri, use.names = TRUE),
    rhs       = data.table::rbindlist(rhs, use.names = TRUE),
    row_index = row_index,
    col_index = col_index
  )
  attr(out, "native") <- native
  attr(out, "lower_why") <- ctx$lower_why
  attr(out, "native_stats") <- ctx$native_stats
  out
}
