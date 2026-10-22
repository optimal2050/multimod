# =============================================================================
# Block plans: lowering an equation for the native executor
# =============================================================================
#
# An equation is lowered into a list of *terms*, each a sequence of steps over
# the equation's row table (see core/include/multimod/block.h): `gate`
# (semi-join with a mapping), `expand` (binding merge that introduces a
# summation index), `filter` (a condition evaluated per row), `factor` (a
# numeric expression of parameters and constants evaluated per row) and `var`
# (column lookup). The executor multiplies the factors per row, emits
# (row, column, coefficient) for a term with a variable and a constant for a
# term without, and sums per (row, column).
#
# A sub-expression free of variables and summations is one factor; the rest
# is distributed into a sum of products. The lowering covers references,
# constants, arithmetic, `when` with mapping or value conditions, and sums
# over mappings or sets. Anything else (lag/lead, wildcard parameters,
# `prod`, `ord`, `val`, set extrema, a bare index as a number) makes the
# lowering return NULL and the equation is evaluated by the R engine instead.
# Equivalence is checked per equation by .mx_engine_report() and by the
# matrix goldens with `options(multimod.engine = "native")`.
# =============================================================================

#' Lower one equation into a block plan
#' @return A list `plan` (as the executor reads it) and `tables` (the named
#'   list of coded tables it refers to), or NULL when the equation uses a
#'   construct the lowering does not cover; the construct is then recorded in
#'   `ctx$lower_why[[name]]`.
#' @keywords internal
#' @noRd
.mx_lower_equation <- function(eq, ctx) {
  dims <- .mx_eq_dims(eq)
  if (anyDuplicated(dims)) return(NULL)
  L <- new.env(parent = emptyenv())
  L$ctx <- ctx
  L$tables <- list()
  L$fail <- FALSE
  L$why <- NULL
  lhs <- .mx_lower(eq$lhs, dims, L)
  rhs <- .mx_lower(eq$rhs, dims, L)
  if (L$fail || is.null(lhs) || is.null(rhs)) {
    ctx$lower_why[[eq$name]] <- L$why %||% "unknown"
    return(NULL)
  }
  rhs <- lapply(rhs, function(t) { t$sign <- -t$sign; t })
  list(plan = list(rows = "rows", row_keys = as.character(dims), terms = c(lhs, rhs)),
       tables = L$tables)
}

# A lowering failure: mark it with the construct that caused it (the first
# one wins) and return NULL up the recursion
.mx_lower_fail <- function(L, why) {
  if (is.null(L$why)) L$why <- why
  L$fail <- TRUE
  NULL
}

# names as the environment binds them (short aliases, alias groups)
.mx_bound_names <- function(ref, bound, ctx) {
  fake <- stats::setNames(vector("list", length(bound)), bound)
  vapply(ref, function(x) .mx_env_name(ctx, fake, x), character(1), USE.NAMES = FALSE)
}

.mx_has_shift <- function(node) {
  d <- node$dims
  !is.null(d) && length(d) && any(vapply(d, function(e) inherits(e, "shift"), logical(1)))
}

.mx_term <- function(steps, sign = 1) list(sign = sign, steps = steps)
.mx_cls <- function(node) sub("^mm_", "", class(node)[1])

# --- tables shipped with the plan -------------------------------------------

# The tables are shipped as lists of the cached column vectors: `as.list()`
# of a data.table shares the vectors, so nothing is copied.
.mx_plan_param <- function(nm, pp, L) {
  id <- paste0("p:", nm)
  if (is.null(L$tables[[id]])) L$tables[[id]] <- as.list(pp$lk)[c(pp$keys, "value")]
  id
}

.mx_plan_map <- function(nm, dat, L) {
  id <- paste0("m:", nm)
  if (is.null(L$tables[[id]])) L$tables[[id]] <- as.list(dat)
  id
}

.mx_plan_var <- function(nm, ci, L) {
  id <- paste0("v:", nm)
  if (is.null(L$tables[[id]])) {
    L$tables[[id]] <- as.list(ci)[c("j", grep("^i[0-9]+$", names(ci), value = TRUE))]
  }
  id
}

# --- numeric expressions (no variable, no summation) ------------------------

#' A per-row numeric expression, or NULL when the node is not one
#' @keywords internal
#' @noRd
.mx_num <- function(node, bound, L) {
  if (is.null(node)) return(NULL)
  ctx <- L$ctx
  cls <- .mx_cls(node)
  if (cls == "constant") {
    v <- suppressWarnings(as.numeric(node$value))
    if (is.na(v)) return(NULL)
    return(list(kind = "const", value = v))
  }
  if (cls == "parameter") {
    if (.mx_has_shift(node)) return(NULL)
    pp <- .mx_prep_param(node$name, ctx)
    ref <- .mx_bound_names(.mx_ref_dims(node), bound, ctx)
    if (!length(ref)) {
      return(list(kind = "const", value = if (!is.null(pp$first)) pp$first else pp$defv))
    }
    if (is.null(pp$first)) return(list(kind = "const", value = pp$defv))
    if (length(pp$cols) != length(ref) || !all(ref %in% bound) || !is.null(pp$wild)) return(NULL)
    return(list(kind = "param", table = .mx_plan_param(node$name, pp, L),
                on = stats::setNames(pp$keys, ref), column = "value", default = pp$defv))
  }
  if (cls == "unary") {
    a <- .mx_num(node$rhs, bound, L)
    if (is.null(a)) return(NULL)
    if (identical(node$op, "+")) return(a)
    if (identical(node$op, "-")) return(list(kind = "bin", op = "*", a = list(kind = "const", value = -1), b = a))
    return(NULL)
  }
  if (cls == "expression") {
    op <- node$op
    if (!op %in% c("+", "-", "*", "/", "**", "^")) return(NULL)
    a <- .mx_num(node$lhs, bound, L)
    b <- .mx_num(node$rhs, bound, L)
    if (is.null(a) || is.null(b)) return(NULL)
    return(list(kind = "bin", op = if (op == "**") "^" else op, a = a, b = b))
  }
  if (cls == "when") {
    cond <- .mx_cond(node$condition, bound, L)
    a <- .mx_num(node$then, bound, L)
    if (is.null(cond) || is.null(a)) return(NULL)
    b <- NULL
    if (!is.null(node$otherwise)) {
      b <- .mx_num(node$otherwise, bound, L)
      if (is.null(b)) return(NULL)
    }
    return(list(kind = "when", cond = cond, a = a, b = b))
  }
  agg <- if (cls == "sum") "+" else if (cls == "prod") "*" else
    if (cls == "func" && tolower(node$name) %in% c("sum", "prod")) {
      if (tolower(node$name) == "sum") "+" else "*"
    } else NULL
  if (!is.null(agg)) {
    st <- .mx_agg_steps(node, bound, L)
    if (is.null(st)) return(NULL)
    if (identical(st, "empty")) return(list(kind = "const", value = if (agg == "*") 1 else 0))
    body <- .mx_num(node$value, st$cur, L)
    if (is.null(body)) return(NULL)
    return(list(kind = "agg", op = agg, steps = st$steps, body = body))
  }
  NULL
}

# --- conditions --------------------------------------------------------------

# condition names: a summation iterator stays literal, anything else resolves
# through the aliases against what is bound
.mx_resolve_cond_names <- function(ref, bound, iters, ctx) {
  vapply(ref, function(x) {
    if (x %in% iters) x else .mx_bound_names(x, bound, ctx)
  }, character(1), USE.NAMES = FALSE)
}

#' A condition over bound indices, or NULL when it cannot be lowered
#' @keywords internal
#' @noRd
.mx_cond <- function(node, bound, L, iters = character()) {
  if (is.null(node)) return(NULL)
  ctx <- L$ctx
  cls <- .mx_cls(node)
  if (cls == "mapping") {
    ref <- .mx_ref_dims(node)
    if (!length(ref) || anyDuplicated(ref)) return(NULL)
    ref <- .mx_resolve_cond_names(ref, bound, iters, ctx)
    if (!all(ref %in% bound)) return(NULL)
    dat <- .mx_map_data(node$name, ctx)
    if (!nrow(dat)) {   # an empty map: nothing qualifies
      return(list(kind = "cmp", op = "==", a = list(kind = "const", value = 0), b = list(kind = "const", value = 1)))
    }
    if (length(names(dat)) != length(ref)) return(NULL)
    return(list(kind = "member", table = .mx_plan_map(node$name, dat, L),
                on = stats::setNames(names(dat), ref)))
  }
  if (cls == "unary" && identical(node$op, "not")) {
    a <- .mx_cond(node$rhs, bound, L, iters)
    if (is.null(a)) return(NULL)
    return(list(kind = "not", a = a))
  }
  if (cls == "expression") {
    op <- node$op
    if (op %in% c("and", "&", "&&", "or", "|", "||")) {
      a <- .mx_cond(node$lhs, bound, L, iters)
      b <- .mx_cond(node$rhs, bound, L, iters)
      if (is.null(a) || is.null(b)) return(NULL)
      return(list(kind = if (op %in% c("and", "&", "&&")) "and" else "or", a = a, b = b))
    }
    if (op %in% c("<", "<=", ">", ">=", "=", "==", "<>", "!=")) {
      a <- .mx_num(node$lhs, bound, L)
      b <- .mx_num(node$rhs, bound, L)
      if (is.null(a) || is.null(b)) return(NULL)
      op <- switch(op, "=" = "==", "<>" = "!=", op)
      return(list(kind = "cmp", op = op, a = a, b = b))
    }
  }
  NULL
}

# --- terms -------------------------------------------------------------------

#' @keywords internal
#' @noRd
.mx_lower <- function(node, bound, L) {
  if (L$fail) return(NULL)
  if (is.null(node)) return(list())
  e <- .mx_num(node, bound, L)
  if (!is.null(e)) return(list(.mx_term(list(list(op = "factor", expr = e)))))
  cls <- .mx_cls(node)
  switch(
    cls,
    variable   = .mx_lower_variable(node, bound, L),
    expression = .mx_lower_expression(node, bound, L),
    unary      = .mx_lower_unary(node, bound, L),
    when       = .mx_lower_when(node, bound, L),
    func       = .mx_lower_func(node, bound, L),
    sum        = .mx_lower_sum(node, bound, L),
    .mx_lower_fail(L, paste0("node:", cls))
  )
}

.mx_lower_unary <- function(node, bound, L) {
  tm <- .mx_lower(node$rhs, bound, L)
  if (is.null(tm)) return(NULL)
  if (identical(node$op, "-")) return(lapply(tm, function(t) { t$sign <- -t$sign; t }))
  if (identical(node$op, "+")) return(tm)
  .mx_lower_fail(L, paste0("unary:", node$op))
}

.mx_lower_variable <- function(node, bound, L) {
  if (.mx_has_shift(node)) return(.mx_lower_fail(L, "shift"))
  ctx <- L$ctx
  nm <- node$name
  ci <- ctx$col_cache[[nm]]
  if (is.null(ci)) {
    ci <- .mx_col_coded(ctx)[symbol == nm]
    ctx$col_cache[[nm]] <- ci
  }
  if (!nrow(ci)) return(list())              # no columns in this scenario: no term
  ref <- .mx_bound_names(.mx_ref_dims(node), bound, ctx)
  id <- .mx_plan_var(nm, ci, L)
  if (!length(ref)) {                        # a scalar variable
    return(list(.mx_term(list(list(op = "var", table = id, on = character(), column = "j")))))
  }
  idx <- paste0("i", seq_along(ref))
  if (!all(idx %in% names(ci))) return(.mx_lower_fail(L, "var:arity"))
  if (!all(ref %in% bound)) return(.mx_lower_fail(L, "var:unbound"))
  list(.mx_term(list(list(op = "var", table = id, on = stats::setNames(idx, ref), column = "j"))))
}

.mx_terms_has_var <- function(terms) {
  any(vapply(terms, function(t) any(vapply(t$steps, function(s) s$op == "var", logical(1))), logical(1)))
}

# product of two term lists: every pair, steps concatenated
.mx_terms_mult <- function(a, b, L) {
  if (.mx_terms_has_var(a) && .mx_terms_has_var(b)) return(.mx_lower_fail(L, "nonlinear"))
  out <- list()
  for (x in a) for (y in b) {
    out[[length(out) + 1L]] <- .mx_term(c(x$steps, y$steps), x$sign * y$sign)
  }
  out
}

.mx_lower_expression <- function(node, bound, L) {
  op <- node$op
  if (op %in% c("+", "-")) {
    l <- .mx_lower(node$lhs, bound, L)
    r <- .mx_lower(node$rhs, bound, L)
    if (is.null(l) || is.null(r)) return(NULL)
    if (op == "-") r <- lapply(r, function(t) { t$sign <- -t$sign; t })
    return(c(l, r))
  }
  if (op == "*") {
    l <- .mx_lower(node$lhs, bound, L)
    r <- .mx_lower(node$rhs, bound, L)
    if (is.null(l) || is.null(r)) return(NULL)
    return(.mx_terms_mult(l, r, L))
  }
  if (op == "/") {
    l <- .mx_lower(node$lhs, bound, L)
    if (is.null(l)) return(NULL)
    d <- .mx_num(node$rhs, bound, L)     # the divisor must be a per-row number
    if (is.null(d)) return(.mx_lower_fail(L, "divisor"))
    return(lapply(l, function(t) {
      t$steps <- c(t$steps, list(list(op = "factor", expr = d, invert = TRUE)))
      t
    }))
  }
  .mx_lower_fail(L, paste0("op:", op))
}

# one conjunct of a condition as a step: a gate for a bound mapping, a filter
# for anything else the condition lowering covers
.mx_cond_step <- function(cond, bound, L, iters = character()) {
  if (inherits(cond, "mm_mapping")) {
    c <- .mx_cond(cond, bound, L, iters)
    if (is.null(c)) return(NULL)
    if (c$kind == "member") return(list(op = "gate", table = c$table, on = c$on))
    return(list(op = "filter", cond = c))
  }
  c <- .mx_cond(cond, bound, L, iters)
  if (is.null(c)) return(NULL)
  list(op = "filter", cond = c)
}

.mx_lower_when <- function(node, bound, L) {
  if (!is.null(node$otherwise)) return(.mx_lower_fail(L, "when:otherwise"))
  steps <- list()
  for (cond in .mx_flatten_and(node$condition)) {
    s <- .mx_cond_step(cond, bound, L)
    if (is.null(s)) return(.mx_lower_fail(L, paste0("cond:", .mx_cls(cond))))
    steps[[length(steps) + 1L]] <- s
  }
  body <- .mx_lower(node$then, bound, L)
  if (is.null(body)) return(NULL)
  lapply(body, function(t) { t$steps <- c(steps, t$steps); t })
}

.mx_lower_func <- function(node, bound, L) {
  fn <- tolower(node$name)
  if (fn == "sum") return(.mx_lower_sum(node, bound, L))
  .mx_lower_fail(L, paste0("func:", fn))
}

#' The binding steps of a sum or product
#'
#' Iterators are bound through the mappings that share a bound index
#' (expansions), the rest through the dense cross with the set's members;
#' the conditions that bind nothing filter the expanded rows afterwards.
#' @return list(steps, cur) with `cur` the names bound for the body; the
#'   string "empty" when a gate holds no tuples (the aggregate is empty);
#'   NULL, with the reason in `L$agg_why`, when a construct is not covered.
#' @keywords internal
#' @noRd
.mx_agg_steps <- function(node, bound, L) {
  ctx <- L$ctx
  no <- function(why) { L$agg_why <- why; NULL }
  idx <- node$index
  cond <- NULL
  if (inherits(idx, "when")) { cond <- idx$condition; idx <- idx$then }
  if (is.null(cond) && !is.null(node$domain)) cond <- node$domain
  iters <- .mx_index_names(idx)
  if (!length(iters)) return(no("agg:noindex"))
  if (any(iters %in% bound)) return(no("agg:shadow"))
  conds <- .mx_flatten_and(cond)
  # Binding candidates: the sum's own conditions, and the mapping gates of
  # its body (`x$m` zeroes the body wherever `m` fails, so `m` may bind the
  # iterator too). A body gate's names are read literally, as the R engine
  # does; an own condition resolves through the aliases.
  gates <- .mx_body_gates(node$value)
  cands <- c(conds, gates)
  own <- seq_along(cands) <= length(conds)
  used <- rep(FALSE, length(cands))

  steps <- list()
  cur <- bound
  repeat {
    best <- NULL
    for (n in which(!used)) {
      cn <- cands[[n]]
      if (!inherits(cn, "mm_mapping")) next
      ref <- .mx_ref_dims(cn)
      if (!length(ref) || anyDuplicated(ref)) { if (own[n]) return(no("agg:map-ref")) else next }
      if (own[n]) ref <- .mx_resolve_cond_names(ref, cur, iters, ctx)
      dat <- .mx_map_data(cn$name, ctx)
      if (!nrow(dat)) { if (own[n]) return("empty") else next }
      if (length(names(dat)) != length(ref)) { if (own[n]) return(no("agg:map-arity")) else next }
      shared <- ref[ref %in% cur]
      newc <- ref[!ref %in% cur]
      if (!length(newc)) next                          # a filter; applied below
      if (!all(newc %in% iters)) { if (own[n]) return(no("agg:binds-foreign")) else next }
      if (!length(shared)) next                        # needs a bound index first
      # the most selective join first: the fewest table rows per distinct
      # value of the shared keys (the fan-out of the expansion); an own
      # condition wins a tie
      score <- .mx_map_fanout(cn$name, dat, names(dat)[ref %in% cur], ctx) - if (own[n]) 1e-9 else 0
      if (is.null(best) || score < best$score) {
        best <- list(n = n, ref = ref, dat = dat, shared = shared, newc = newc, score = score)
      }
    }
    if (is.null(best)) break
    cn <- cands[[best$n]]
    id <- .mx_plan_map(cn$name, best$dat, L)
    cols <- names(best$dat)
    steps[[length(steps) + 1L]] <- list(
      op = "expand", table = id,
      on = stats::setNames(cols[best$ref %in% cur], best$shared),
      bind = stats::setNames(cols[!best$ref %in% cur], best$newc))
    cur <- c(cur, best$newc)
    used[best$n] <- TRUE
    if (all(iters %in% cur)) break
  }
  used <- used[own]
  remaining <- setdiff(iters, cur)
  if (length(remaining)) {
    sets <- .mx_index_sets(idx)
    for (it in remaining) {
      base <- if (it %in% names(sets)) sets[[it]] else it
      members <- .mx_set_members(ctx, .mx_base_set(ctx$model, base))
      if (!length(members)) return("empty")
      id <- paste0("s:", base)
      if (is.null(L$tables[[id]])) L$tables[[id]] <- list(m = members)
      steps[[length(steps) + 1L]] <- list(op = "expand", table = id, on = character(),
                                          bind = stats::setNames("m", it))
      cur <- c(cur, it)
    }
  }
  for (n in which(!used)) {
    s <- .mx_cond_step(conds[[n]], cur, L, iters)
    if (is.null(s)) return(no(paste0("agg:cond:", .mx_cls(conds[[n]]))))
    steps[[length(steps) + 1L]] <- s
  }
  list(steps = steps, cur = cur)
}

# average rows of a mapping per distinct tuple of `cols`: the fan-out of an
# expansion joined on those columns; cached per build
.mx_map_fanout <- function(nm, dat, cols, ctx) {
  key <- paste(c(nm, cols), collapse = "\x1f")
  f <- ctx$fanout_cache[[key]]
  if (!is.null(f)) return(f)
  f <- nrow(dat) / max(1, data.table::uniqueN(dat, by = cols))
  if (is.null(ctx$fanout_cache)) ctx$fanout_cache <- list()
  ctx$fanout_cache[[key]] <- f
  f
}

.mx_lower_sum <- function(node, bound, L) {
  st <- .mx_agg_steps(node, bound, L)
  if (is.null(st)) return(.mx_lower_fail(L, L$agg_why %||% "sum"))
  if (identical(st, "empty")) return(list())
  body <- .mx_lower(node$value, st$cur, L)
  if (is.null(body)) return(NULL)
  lapply(body, function(t) { t$steps <- c(st$steps, t$steps); t })
}

# --- execution --------------------------------------------------------------

#' Run a lowered equation over one chunk of its rows
#' @param lowered The result of .mx_lower_equation().
#' @param rows The coded row table of the chunk (`i`, `i1..iN`).
#' @return list(mat = data.table(i, j, coef), cst = data.table(i, rhs))
#' @keywords internal
#' @noRd
.mx_exec_block <- function(lowered, rows, ctx = NULL) {
  dims <- lowered$plan$row_keys
  rt <- list(k = rows$i)
  for (n in seq_along(dims)) rt[[dims[n]]] <- rows[[paste0("i", n)]]
  ex <- if (!is.null(ctx)) .mx_executor(ctx, lowered$tables) else
    mm_block_open(lowered$tables, .mx_threads())
  out <- mm_block_run(ex, lowered$plan, rt)
  list(mat = data.table::data.table(i = out$i, j = out$j, coef = out$coef),
       cst = data.table::data.table(i = out$rhs_i, rhs = out$rhs),
       stats = out$stats)
}

#' The build's executor, with the tables of a lowered equation registered
#'
#' One executor serves every equation of a build, so the hash indexes it
#' builds on a parameter, mapping or column table are built once. The R
#' vectors behind the registered tables are kept in `ctx$ex_keep` for as
#' long as the executor lives, since it reads them in place.
#' @keywords internal
#' @noRd
.mx_executor <- function(ctx, tables) {
  if (is.null(ctx$ex)) {
    ctx$ex <- mm_block_open(list(), .mx_threads())
    ctx$ex_keep <- list()
  }
  new <- setdiff(names(tables), names(ctx$ex_keep))
  if (length(new)) {
    mm_block_add(ctx$ex, tables[new])
    ctx$ex_keep[new] <- tables[new]
  }
  ctx$ex
}

# threads of the native executor: `options(multimod.threads = )`, default the
# machine's cores
.mx_threads <- function() {
  n <- getOption("multimod.threads", NULL)
  if (is.null(n)) n <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 1L)
  n <- suppressWarnings(as.integer(n))
  if (is.na(n) || n < 1L) 1L else n
}

#' Compare the two engines equation by equation
#'
#' A development aid: for every equation of `model`, whether the lowering
#' covers it and, when it does, whether the native executor reproduces the R
#' engine's triplets and right-hand sides exactly, with the time of each.
#' @return A data.frame with one row per equation.
#' @keywords internal
#' @noRd
.mx_engine_report <- function(model, chunk_rows = 1000000L) {
  model <- .mx_materialise_formulas(model)
  col_index <- build_col_index(model, names = FALSE)
  row_index <- build_row_index(model, names = FALSE)
  eqs <- unique(row_index$symbol)
  rows <- lapply(eqs, function(nm) {
    r <- withr::with_options(list(multimod.engine = "r"), {
      t0 <- proc.time()[[3]]
      a <- build_triplets(model, col_index, row_index, equations = nm, chunk_rows = chunk_rows)
      list(t = proc.time()[[3]] - t0, tri = a$triplets, rhs = a$rhs)
    })
    n <- withr::with_options(list(multimod.engine = "native"), {
      t0 <- proc.time()[[3]]
      a <- build_triplets(model, col_index, row_index, equations = nm, chunk_rows = chunk_rows)
      list(t = proc.time()[[3]] - t0, tri = a$triplets, rhs = a$rhs,
           native = attr(a, "native"), why = attr(a, "lower_why"))
    })
    key <- function(d) d[order(d$i, d$j)]
    # the right-hand side is compared as values per row: a constant term the
    # R engine never emits and a zero one the executor emits are the same 0
    dense <- function(d) { v <- d[d$rhs != 0]; v[order(v$i)] }
    same <- identical(nrow(r$tri), nrow(n$tri)) &&
      isTRUE(all.equal(key(r$tri), key(n$tri), check.attributes = FALSE)) &&
      isTRUE(all.equal(dense(r$rhs), dense(n$rhs), check.attributes = FALSE))
    data.frame(equation = nm, rows = sum(row_index$symbol == nm), nnz = nrow(r$tri),
               native = isTRUE(n$native[[nm]]), same = same,
               why = if (isTRUE(n$native[[nm]])) "" else (n$why[[nm]] %||% ""),
               secs_r = r$t, secs_native = n$t, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
