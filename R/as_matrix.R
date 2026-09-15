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

#' Coerce join-key columns to character in place
#'
#' Index members must join on a single canonical type: `year` arrives as
#' integer from some stores and character from others, and data.table refuses
#' to join across the two.
#' @keywords internal
#' @noRd
.mx_chr_keys <- function(dt, cols) {
  for (cl in cols) {
    if (cl %in% names(dt) && !is.character(dt[[cl]])) {
      data.table::set(dt, j = cl, value = as.character(dt[[cl]]))
    }
  }
  dt
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
  cls <- class(node)[1]

  switch(
    cls,
    constant   = .mx_eval_constant(node, env, ctx),
    parameter  = .mx_eval_parameter(node, env, ctx),
    variable   = .mx_eval_variable(node, env, ctx),
    expression = .mx_eval_expression(node, env, ctx),
    unary      = .mx_eval_unary(node, env, ctx),
    when       = .mx_eval_when(node, env, ctx),
    func       = .mx_eval_func(node, env, ctx),
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

#' @keywords internal
#' @noRd
.mx_eval_parameter <- function(node, env, ctx) {
  nm <- node$name
  dat <- ctx$param_cache[[nm]]
  if (is.null(dat)) {
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
    ctx$param_cache[[nm]] <- dat
  }
  defv <- ctx$model$parameters[[nm]]$defVal
  defv <- if (is.null(defv) || !is.numeric(defv)) 0 else as.numeric(defv)[1]
  # ... and the same writers map an infinite DEFAULT to 0 (`if (def == Inf)
  # def <- 0`). Surprising as a modelling choice, but it is what generated every
  # reference solution this backend is checked against.
  if (!is.finite(defv)) defv <- 0

  ref <- .mx_ref_dims(node)

  # scalar parameter
  if (!length(ref)) {
    v <- if (nrow(dat) && "value" %in% names(dat)) as.numeric(dat$value[1]) else defv
    return(.mx_const(env, v))
  }

  if (!nrow(dat) || !"value" %in% names(dat)) {
    return(.mx_const(env, defv))     # nothing stored: every tuple is the default
  }

  cols <- .mx_storage_cols(dat)
  if (length(cols) != length(ref)) {
    stop("as_matrix: parameter '", nm, "' is stored with ", length(cols),
         " index column(s) but is referenced with ", length(ref), ".")
  }
  miss <- setdiff(ref, names(env))
  if (length(miss)) {
    stop("as_matrix: parameter '", nm, "' references unbound index/indices: ",
         paste(miss, collapse = ", "), ".")
  }

  lk <- data.table::copy(data.table::as.data.table(dat))
  data.table::setnames(lk, cols, ref)
  lk <- lk[, c(ref, "value"), with = FALSE]

  # A wildcard (NA) index means "every member of this dimension" - energyRt
  # writes them when interpolating with fold = TRUE. They must have been
  # expanded at import; if one reaches the join it matches nothing, the tuple
  # takes the parameter's default, and the model solves to a wrong answer with
  # no error at all. Refuse instead.
  wild <- vapply(ref, function(k) anyNA(lk[[k]]), logical(1))
  if (any(wild)) {
    stop("as_matrix: parameter '", nm, "' has wildcard (NA) values in index ",
         "column(s) ", paste(ref[wild], collapse = ", "), ".
  This is a ",
         "folded parameter that was not expanded on import. Joining on an NA ",
         "key silently substitutes the default, so the matrix would be wrong ",
         "rather than missing.
  Re-import with inMemory = TRUE, or ",
         "re-interpolate the scenario with fold = FALSE.")
  }

  .mx_chr_keys(lk, ref)

  # A repeated index tuple would multiply rows on the join. Repeats that all
  # carry the same value are harmless and get collapsed; repeats carrying
  # DIFFERENT values are ambiguous - there is no defensible way to pick one, so
  # say so rather than silently taking whichever data.table returns first.
  if (anyDuplicated(lk, by = ref)) {
    nd <- lk[, .(n = length(unique(value))), by = ref]
    bad <- nd[n > 1L]
    if (nrow(bad)) {
      ex <- paste(unlist(bad[1L, ref, with = FALSE]), collapse = ", ")
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
    lk <- if (identical(ctx$on_duplicate, "first")) {
      unique(lk, by = ref, fromLast = FALSE)
    } else {
      unique(lk, by = ref, fromLast = TRUE)   # GAMS/Pyomo semantics
    }
  }

  # a tuple may legitimately be absent -> it takes the parameter default,
  # NOT zero (energyRt stores sparsely, e.g. only the 'lo' rows of a bound)
  out <- lk[env, on = ref]
  coef <- out$value
  coef[is.na(coef)] <- defv
  data.table::data.table(.k = env$.k, j = NA_integer_, coef = as.numeric(coef))
}

#' @keywords internal
#' @noRd
.mx_eval_variable <- function(node, env, ctx) {
  nm <- node$name
  ci <- ctx$col_cache[[nm]]
  if (is.null(ci)) {
    ci <- ctx$col_index[ctx$col_index$symbol == nm]
    ctx$col_cache[[nm]] <- ci
  }
  if (!nrow(ci)) {
    # variable exists in the AST but has no columns in this scenario (its
    # domain map is empty): the term simply does not exist
    return(.mx_no_terms())
  }

  ref <- .mx_ref_dims(node)
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

  lk <- ci[, c("j", idx), with = FALSE]
  data.table::setnames(lk, idx, ref)
  out <- lk[env, on = ref, nomatch = NULL]   # no column there -> no term
  if (!nrow(out)) return(.mx_no_terms())
  data.table::data.table(.k = out$.k, j = out$j, coef = 1)
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
    return(.mx_scale(l, r, "^"))
  }

  stop("as_matrix: unsupported operator '", op, "' in an equation body. ",
       "Comparison/logical operators are only valid inside a condition.")
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
  v <- suppressWarnings(as.numeric(as.character(env[[set_name]])))
  if (anyNA(v)) {
    stop("as_matrix: val(", set_name, ") - member(s) ",
         paste(utils::head(unique(env[[set_name]][is.na(v)]), 3),
               collapse = ", "),
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
  pos <- match(as.character(env[[set_name]]), members)
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
  iters <- .mx_index_names(idx)
  if (!length(iters)) return(NULL)

  cur <- data.table::copy(env)
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
  conds <- .mx_flatten_and(cond)
  used <- rep(FALSE, length(conds))
  for (n in seq_along(conds)) {
    if (all(iters %in% names(cur))) break
    j <- .mx_join_cond(cur, conds[[n]], iters, ctx)
    if (!is.null(j)) {
      cur <- j
      used[n] <- TRUE
      if (!nrow(cur)) return(NULL)
    }
  }

  # Anything still unbound has no map to drive it: expand densely, with a guard.
  remaining <- setdiff(iters, names(cur))
  if (length(remaining)) {
    cur <- .mx_cross_dense(cur, remaining, ctx)
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
  if (!inherits(cond, "mapping")) return(NULL)
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

  lk <- data.table::copy(dat)
  data.table::setnames(lk, names(lk), ref)
  .mx_chr_keys(lk, ref)
  lk <- unique(lk, by = ref)
  merge(cur, lk, by = shared, allow.cartesian = TRUE)
}

#' Replicate rows once per member of each still-unbound iterator
#'
#' The dense fallback, used only when no mapping constrains the iterator. Guards
#' the 2^31 row limit so an oversized expansion reports what it was doing rather
#' than failing with "long vectors not supported yet".
#' @keywords internal
#' @noRd
.mx_cross_dense <- function(cur, iters, ctx) {
  cur <- data.table::copy(cur)
  for (it in iters) {
    if (it %in% names(cur)) next
    members <- .mx_set_members(ctx, .mx_base_set(ctx$model, it))
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

#' Iterator names introduced by a sum/prod index
#' @keywords internal
#' @noRd
.mx_index_names <- function(idx) {
  if (is.null(idx)) return(character())
  if (inherits(idx, "dims")) {
    return(vapply(idx, dim_binding_name, character(1), USE.NAMES = FALSE))
  }
  nm <- dim_binding_name(idx)
  if (nzchar(nm)) nm else character()
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
  cls <- class(node)[1]

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

#' Tuple table of a mapping, cached, with the two failure modes kept apart
#'
#' A mapping the model never declared is a model error and must say so. A
#' declared mapping holding no tuples is legitimate - it means the gated
#' expression is empty - and returns a zero-row table. The two used to be
#' conflated, and the second silently became "no map constrains this index".
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

  ref <- .mx_ref_dims(node)
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

  lk <- data.table::copy(dat)
  data.table::setnames(lk, cols, ref)
  .mx_chr_keys(lk, ref)
  lk <- unique(lk[, ref, with = FALSE], by = ref)
  data.table::set(lk, j = ".mx_hit", value = TRUE)
  res <- lk[env, on = ref]
  !is.na(res$.mx_hit)
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

#' Members of a set, cached
#' @keywords internal
#' @noRd
.mx_set_members <- function(ctx, set_name) {
  hit <- ctx$set_cache[[set_name]]
  if (!is.null(hit)) return(hit)
  s <- ctx$model$sets[[set_name]]
  mem <- if (is.null(s)) character() else as.character(extract_set_members(s))
  ctx$set_cache[[set_name]] <- mem
  mem
}

# --- equation assembly ------------------------------------------------------

utils::globalVariables(c(".k", ".outer", ".mx_hit", "coef", "j", "v", "seq_len"))

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
.mx_equation_env <- function(eq, rows) {
  dims <- .mx_ref_dims(eq$domain)
  if (!length(dims)) {
    dims <- if (is.null(eq$dims)) character() else
      vapply(eq$dims, dim_binding_name, character(1), USE.NAMES = FALSE)
  }
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
#' @return A list with `triplets` (`data.table(i, j, coef)`, aggregated), `rhs`
#'   (`data.table(i, rhs)`), and `row_index` / `col_index` as used.
#' @export
build_triplets <- function(model, col_index = NULL, row_index = NULL,
                           equations = NULL,
                           on_duplicate = c("error", "last", "first"),
                           verbose = FALSE, chunk_rows = 1000000L) {
  on_duplicate <- match.arg(on_duplicate)
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  stopifnot(is.numeric(chunk_rows), length(chunk_rows) == 1, chunk_rows >= 1)
  chunk_rows <- as.integer(chunk_rows)
  if (is.null(col_index)) col_index <- build_col_index(model)
  if (is.null(row_index)) row_index <- build_row_index(model)

  ctx <- new.env(parent = emptyenv())
  ctx$model <- model
  ctx$col_index <- col_index
  ctx$col_cache <- list()
  ctx$param_cache <- list()
  ctx$map_cache <- list()
  ctx$set_cache <- list()
  ctx$on_duplicate <- on_duplicate
  ctx$dup_warned <- list()

  todo <- if (is.null(equations)) unique(row_index$symbol) else
    intersect(equations, unique(row_index$symbol))

  tri <- vector("list", length(todo))
  rhs <- vector("list", length(todo))

  for (n in seq_along(todo)) {
    nm <- todo[n]
    eq <- model$equations[[nm]]
    if (is.null(eq)) next
    rows <- row_index[row_index$symbol == nm]

    t0 <- Sys.time()
    nr <- nrow(rows)
    n_chunks <- max(1L, as.integer(ceiling(nr / chunk_rows)))
    mats <- vector("list", n_chunks)
    csts <- vector("list", n_chunks)
    for (ck in seq_len(n_chunks)) {
      sl <- if (n_chunks == 1L) rows else
        rows[(((ck - 1L) * chunk_rows) + 1L):min(ck * chunk_rows, nr)]
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
      mats[[ck]] <- mat
      csts[[ck]] <- cst
    }
    tri[[n]] <- if (n_chunks == 1L) mats[[1L]] else
      data.table::rbindlist(mats, use.names = TRUE)
    rhs[[n]] <- if (n_chunks == 1L) csts[[1L]] else
      data.table::rbindlist(csts, use.names = TRUE)
    if (verbose) {
      message(sprintf("  %-30s rows=%-7d nnz=%-8d %.1fs", nm, nr,
                      nrow(tri[[n]]),
                      as.numeric(Sys.time() - t0, units = "secs")))
    }
  }

  list(
    triplets  = data.table::rbindlist(tri, use.names = TRUE),
    rhs       = data.table::rbindlist(rhs, use.names = TRUE),
    row_index = row_index,
    col_index = col_index
  )
}
