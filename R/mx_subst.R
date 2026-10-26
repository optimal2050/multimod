# =============================================================================
# Substitution of definition equations at build time
# =============================================================================
#
# A definition equation is an equality whose domain is exactly the index set
# of one of its variables, `x[d] = f(d)` with `f` linear in the other
# variables. Substituting it removes the equation's rows and the columns of
# `x` on its domain from the LP: wherever another equation refers to `x` at a
# tuple of the domain, the reference is lowered as `f` at that tuple; outside
# the domain it stays a column lookup. The substitution is the same one a
# doubleton presolve makes after the build, made before the rows and columns
# exist.
#
# For the decode, the definition is evaluated over its own rows and written
# beside the index tables as (tuple of x, column, coefficient) and constant
# parts; read_mps_solution() rebuilds `x` from them and the solved columns.
# =============================================================================

#' Variable nodes of an AST, in order of appearance
#' @keywords internal
#' @noRd
.mx_ast_vars <- function(node) {
  if (is.null(node) || !is.list(node)) return(list())
  if (identical(.mx_cls(node), "variable")) return(list(node))
  out <- list()
  for (el in node) if (is.list(el)) out <- c(out, .mx_ast_vars(el))
  out
}

#' Lower and upper bound of a variable, as build_col_index() sets them
#' @keywords internal
#' @noRd
.mx_var_bounds <- function(v) {
  lo <- if (!is.null(v$bounds$lo)) as.numeric(v$bounds$lo)[1] else NA_real_
  up <- if (!is.null(v$bounds$up)) as.numeric(v$bounds$up)[1] else NA_real_
  vt <- if (!is.null(v$vtype) && nzchar(v$vtype[1])) v$vtype[1] else "continuous"
  if (is.na(lo) && vt %in% c("positive", "binary")) lo <- 0
  if (is.na(up) && vt == "binary") up <- 1
  list(lo = if (is.na(lo)) -Inf else lo, up = if (is.na(up)) Inf else up, vtype = vt)
}

#' Prepare the substitution of definition equations
#'
#' For each equation: the eliminated variable (the one indexed by exactly
#' the equation's domain, appearing once) and the domain's tuples in that
#' variable's index order, coded with the build's dictionary.
#' @param equations Names of the equations to substitute.
#' @param codes The build's dictionary.
#' @return A named list (by eliminated variable) of `list(var, eq, keys, lo)`,
#'   or NULL when `equations` is empty.
#' @keywords internal
#' @noRd
.mx_subst_prepare <- function(model, equations, codes) {
  if (!length(equations)) return(NULL)
  ctx0 <- new.env(parent = emptyenv())
  ctx0$model <- model
  spec <- list()
  for (en in equations) {
    eq <- model$equations[[en]]
    if (is.null(eq)) stop("substitute: the model has no equation '", en, "'.", call. = FALSE)
    rel <- if (is.null(eq$relation)) NA_character_ else eq$relation[1]
    if (!identical(rel, "==")) {
      stop("substitute: '", en, "' is not an equality.", call. = FALSE)
    }
    dn <- .domain_name(eq$domain)
    if (is.na(dn)) stop("substitute: '", en, "' has no domain mapping.", call. = FALSE)
    dims <- .mx_eq_dims(eq)
    tp <- .domain_tuples(model, dn, codes)
    if (!nrow(tp)) next                     # no rows: nothing to substitute
    vars <- c(.mx_ast_vars(eq$lhs), .mx_ast_vars(eq$rhs))
    vn <- vapply(vars, function(v) v$name, character(1))
    # an occurrence indexed by exactly the domain
    exact <- vapply(seq_along(vars), function(k) {
      v <- vars[[k]]
      if (vn[k] %in% names(spec) || .mx_has_shift(v)) return(FALSE)
      r <- .mx_bound_names(.mx_ref_dims(v), dims, ctx0)
      length(r) == length(dims) && !anyDuplicated(r) && setequal(r, dims)
    }, logical(1))
    # an occurrence written with exactly the domain's index names (an alias
    # such as `timeslicep` resolves to the domain's name above, so it is
    # told apart here)
    literal <- vapply(seq_along(vars), function(k) {
      r <- .mx_ref_dims(vars[[k]])
      length(r) == length(dims) && !anyDuplicated(r) && setequal(r, dims)
    }, logical(1))
    n_all <- table(vn)
    lv <- names(n_all)
    n_exact <- table(factor(vn[exact], levels = lv))
    n_lit <- table(factor(vn[exact & literal], levels = lv))
    # a variable that also appears at other tuples of its own definition (a
    # roll-up over finer timeslices or regions) is the one defined; otherwise
    # the one variable appearing once, over the domain
    recursive <- lv[n_lit == 1L & n_all > 1L & n_exact >= 1L]
    plain <- lv[n_exact == 1L & n_all == 1L]
    pick <- if (length(recursive)) recursive else plain
    if (length(pick) != 1L) {
      stop("substitute: '", en, "' does not define one variable over its domain ",
           "(candidates: ", if (length(pick)) paste(pick, collapse = ", ") else "none",
           ").", call. = FALSE)
    }
    cand <- which(exact & vn == pick & (literal | !length(recursive)))[1]
    v <- vars[[cand]]
    x <- v$name
    b <- .mx_var_bounds(model$variables[[x]])
    if (is.finite(b$up) || !(b$lo == 0 || b$lo == -Inf) || b$vtype %in% c("binary", "integer")) {
      stop("substitute: '", x, "' (defined by '", en, "') must be continuous with ",
           "a lower bound of 0 or none and no upper bound.", call. = FALSE)
    }
    pos <- match(.mx_bound_names(.mx_ref_dims(v), dims, ctx0), dims)
    cols <- as.list(tp)[pos]
    for (k in seq_along(cols)) cols[[k]] <- .mx_code_in(codes, cols[[k]])
    keys <- data.table::setDT(stats::setNames(cols, paste0("i", seq_along(pos))))
    if (anyDuplicated(keys)) stop("substitute: the domain of '", en, "' repeats a tuple.", call. = FALSE)
    # the same tuples in the equation's own index order, for the rows it keeps
    rcols <- as.list(tp)
    for (k in seq_along(rcols)) rcols[[k]] <- .mx_code_in(codes, rcols[[k]])
    rkeys <- data.table::setDT(stats::setNames(rcols, paste0("i", seq_along(rcols))))
    spec[[x]] <- list(var = x, eq = en, keys = keys, rkeys = rkeys, lo = b$lo,
                      recursive = length(recursive) > 0L, kept = 0L)
  }
  spec
}

#' Leave out of a self-referring substitution the tuples whose definition
#' does not imply the variable's lower bound
#'
#' A tuple whose own terms (its flows, not the finer tuples it rolls up)
#' carry a free column, a negative coefficient or a negative constant would
#' lose `x >= 0` with its column. Such a tuple keeps its row and its column,
#' and the tuples rolling it up refer to that column, whose bound holds. The
#' definitions are evaluated once against the column index without the
#' substituted columns.
#' @return `subst` with those tuples removed from `keys` and `rkeys` (their
#'   number in `kept`), and the variables left with no tuple dropped.
#' @keywords internal
#' @noRd
.mx_subst_restrict <- function(model, subst, codes, verbose = FALSE) {
  rec <- names(subst)[vapply(subst, function(s) isTRUE(s$recursive) && s$lo == 0, logical(1))]
  if (!length(rec)) return(subst)
  ztol <- as.numeric(getOption("multimod.zero_tol", 1e-12))
  ci0 <- build_col_index(model, names = FALSE, codes = codes,
                         exclude = lapply(subst, function(s) s$keys))
  ctx0 <- .mx_new_ctx(model, ci0, codes, "error", subst)
  lo <- as.numeric(ci0$lo)
  ncol0 <- nrow(ci0)
  for (x in rec) {
    sp <- subst[[x]]
    ev <- .mx_subst_eval(x, ctx0, .mx_subst_def(x, ctx0))
    tt <- ev$tt
    own <- tt$j <= ncol0
    bad <- c(tt$k[own][tt$coef[own] < -ztol | lo[tt$j[own]] < 0], tt$k[!own][tt$coef[!own] < -ztol])
    if (nrow(ev$cc)) {
      cs <- ev$cc[, list(value = sum(value)), by = "k"]
      bad <- c(bad, cs$k[cs$value < -ztol])
    }
    bad <- sort(unique(bad))
    if (!length(bad)) next
    keep <- setdiff(seq_len(nrow(sp$keys)), bad)
    if (verbose) {
      message(sprintf("  %-24s %d of %d tuples keep their row and column (the definition does not imply %s >= 0)",
                      x, length(bad), nrow(sp$keys), x))
    }
    if (!length(keep)) {
      subst[[x]] <- NULL
      next
    }
    sp$keys <- sp$keys[keep]
    sp$rkeys <- sp$rkeys[keep]
    sp$kept <- length(bad)
    subst[[x]] <- sp
  }
  subst
}

#' The rows a substitution removes: whole equations, or the tuples of a
#' self-referring one that are substituted
#' @keywords internal
#' @noRd
.mx_subst_skip <- function(subst) {
  out <- list()
  for (s in subst) out[[s$eq]] <- if (s$kept > 0L) s$rkeys else TRUE
  out
}

#' Rename the environment columns a lowered plan binds or reads
#'
#' `on` and `bind` vectors carry environment names as their names; `f` maps
#' them. Table-side names are left alone.
#' @keywords internal
#' @noRd
.mx_rename_env <- function(x, f) {
  if (!is.list(x)) return(x)
  for (k in seq_along(x)) {
    nmk <- names(x)[k]
    el <- x[[k]]
    if (is.null(el)) next
    if (!is.null(nmk) && nmk %in% c("on", "bind") && is.character(el)) {
      if (length(el)) {
        names(el) <- f(names(el))
        x[[k]] <- el
      }
    } else if (is.list(el)) {
      x[[k]] <- .mx_rename_env(el, f)
    }
  }
  x
}

#' The definition of a substituted variable, solved for it
#'
#' The definition equation is lowered (other substituted variables are
#' substituted in turn), the one term holding the variable is found, and the
#' others are divided by its coefficient: `x = sum of terms`.
#' @return list(terms, tables, xenv): the terms over the environment names
#'   `xenv` (the variable's index positions, in order) and their tables.
#' @keywords internal
#' @noRd
.mx_subst_def <- function(nm, ctx) {
  def <- ctx$subst_def[[nm]]
  if (!is.null(def)) return(def)
  if (nm %in% ctx$subst_busy) {
    stop("substitute: the definitions are cyclic through '", nm, "'.", call. = FALSE)
  }
  ctx$subst_busy <- c(ctx$subst_busy, nm)
  on.exit(ctx$subst_busy <- setdiff(ctx$subst_busy, nm), add = TRUE)
  sp <- ctx$subst[[nm]]
  eq <- ctx$model$equations[[sp$eq]]
  dims <- .mx_eq_dims(eq)
  L <- new.env(parent = emptyenv())
  L$ctx <- ctx
  L$tables <- list()
  L$fail <- FALSE
  L$why <- NULL
  L$defining <- nm
  L$defining_dims <- dims
  lhs <- .mx_lower(eq$lhs, dims, L)
  rhs <- .mx_lower(eq$rhs, dims, L)
  if (L$fail || is.null(lhs) || is.null(rhs)) {
    stop("substitute: '", sp$eq, "' is not covered by the native lowering (",
         L$why %||% "unknown", ").", call. = FALSE)
  }
  rhs <- lapply(rhs, function(t) { t$sign <- -t$sign; t })
  terms <- c(lhs, rhs)
  vid <- paste0("v:", nm)
  isx <- vapply(terms, function(t) {
    any(vapply(t$steps, function(s) identical(s$op, "var") && isTRUE(s$defined), logical(1)))
  }, logical(1))
  if (sum(isx) != 1L) {
    stop("substitute: '", nm, "' must appear in exactly one term of '", sp$eq, "'.", call. = FALSE)
  }
  xt <- terms[[which(isx)]]
  ops <- vapply(xt$steps, function(s) s$op, character(1))
  if (!all(ops %in% c("factor", "var"))) {
    stop("substitute: the coefficient of '", nm, "' in '", sp$eq,
         "' must be a number per row.", call. = FALSE)
  }
  vs <- xt$steps[[which(ops == "var")]]
  xenv <- names(vs$on)[match(paste0("i", seq_along(vs$on)), unname(vs$on))]
  inv <- lapply(xt$steps[ops == "factor"], function(s) { s$invert <- !isTRUE(s$invert); s })
  others <- lapply(terms[!isx], function(t) {
    t$sign <- -t$sign * xt$sign
    t$steps <- c(t$steps, inv)
    t
  })
  def <- list(terms = others, tables = L$tables, xenv = xenv)
  ctx$subst_def[[nm]] <- def
  def
}

#' A definition evaluated over its domain
#'
#' @return list(tt = data.table(k, j, coef), cc = data.table(k, value)):
#'   the entries and the constants of `x = ...` per tuple `k` of the domain.
#' @keywords internal
#' @noRd
.mx_subst_eval <- function(nm, ctx, def, chunk = 4000000L) {
  sp <- ctx$subst[[nm]]
  n <- nrow(sp$keys)
  ex <- .mx_executor(ctx, def$tables)
  plan <- list(rows = "rows", row_keys = def$xenv, terms = def$terms)
  tt <- list(); cc <- list()
  for (p in seq_len(max(1L, ceiling(n / chunk)))) {
    r <- seq.int((p - 1L) * chunk + 1L, min(p * chunk, n))
    if (!length(r) || n == 0L) break
    rt <- c(list(k = r), stats::setNames(lapply(sp$keys, function(v) v[r]), def$xenv))
    o <- mm_block_run(ex, plan, rt)
    tt[[p]] <- data.table::data.table(k = o$i, j = o$j, coef = o$coef)
    cc[[p]] <- data.table::data.table(k = o$rhs_i, value = -o$rhs)
  }
  list(tt = data.table::rbindlist(tt), cc = data.table::rbindlist(cc))
}

#' The composed form of a self-referring definition, computed once per build
#' @keywords internal
#' @noRd
.mx_subst_mat <- function(nm, ctx) {
  mat <- ctx$subst_mat[[nm]]
  if (is.null(mat)) {
    mat <- .mx_subst_materialise(nm, ctx, .mx_subst_def(nm, ctx))
    ctx$subst_mat[[nm]] <- mat
  }
  mat
}

#' A self-referring definition, composed into columns
#'
#' The definition is evaluated over its domain with each reference to the
#' variable at another tuple left as a placeholder column (numbered past the
#' LP's columns). The placeholders form a graph between the tuples; its levels
#' (a tuple with no placeholder first, then the tuples whose placeholders are
#' all of lower levels) order the composition, and a graph that does not
#' level is a cycle. Each level is composed from the levels below, so every
#' tuple ends as a combination of the LP's columns and a constant.
#' @return list(T = data.table(k, j, coef), C = data.table(k, value), levels)
#'   over the tuples `k` of the domain.
#' @keywords internal
#' @noRd
.mx_subst_materialise <- function(nm, ctx, def) {
  sp <- ctx$subst[[nm]]
  n <- nrow(sp$keys)
  ncol_lp <- nrow(ctx$col_index)
  ztol <- as.numeric(getOption("multimod.zero_tol", 1e-12))
  ev <- .mx_subst_eval(nm, ctx, def)
  tt <- ev$tt
  cc <- ev$cc
  is_self <- tt$j > ncol_lp
  W <- tt[is_self, list(k, kc = j - ncol_lp, w = coef)]
  G <- tt[!is_self]
  # a tuple of the domain by its members
  tuple <- function(k) paste(vapply(sp$keys, function(v) ctx$codes$levels[v[k]], character(1)), collapse = ", ")
  if (any(W$kc == W$k)) {
    k1 <- W$k[W$kc == W$k][1]
    stop("substitute: '", nm, "' refers to itself at the same tuple in '", sp$eq,
         "' (", tuple(k1), ").", call. = FALSE)
  }
  # the levels of the placeholder graph
  lev <- rep(NA_integer_, n)
  has_out <- logical(n); has_out[W$k] <- TRUE
  lev[!has_out] <- 0L
  todo <- which(has_out)
  L <- 0L
  while (length(todo)) {
    L <- L + 1L
    # a tuple is ready when every tuple it refers to has a level
    w_todo <- W[k %in% todo]
    blocked <- unique(w_todo$k[is.na(lev[w_todo$kc])])
    ready <- setdiff(todo, blocked)
    if (!length(ready)) {
      stop("substitute: the definition of '", nm, "' in '", sp$eq, "' is cyclic ",
           "(", length(todo), " tuples refer to each other in a loop, e.g. ", tuple(todo[1]), ").",
           call. = FALSE)
    }
    lev[ready] <- L
    todo <- blocked
  }
  # compose level by level: T(k) = G(k) + sum w T(kc), C(k) = c(k) + sum w C(kc)
  data.table::setkey(G, k)
  data.table::setkey(W, k)
  if (nrow(cc)) cc <- cc[, list(value = sum(value)), by = "k"]
  Tl <- G[k %in% which(lev == 0L)]
  Cl <- cc[k %in% which(lev == 0L)]
  T_all <- list(Tl); C_all <- list(Cl)
  done_T <- Tl; done_C <- Cl
  for (l in seq_len(L)) {
    ks <- which(lev == l)
    wl <- W[k %in% ks]
    tj <- done_T[wl, on = c(k = "kc"), allow.cartesian = TRUE, nomatch = NULL,
                 list(k = i.k, j = j, coef = coef * i.w)]
    Tl <- data.table::rbindlist(list(G[k %in% ks], tj))
    Tl <- Tl[, list(coef = sum(coef)), by = c("k", "j")][abs(coef) >= ztol]
    cj <- if (nrow(done_C)) done_C[wl, on = c(k = "kc"), allow.cartesian = TRUE, nomatch = NULL,
                                   list(k = i.k, value = value * i.w)] else NULL
    Cl <- data.table::rbindlist(list(cc[k %in% ks], cj))
    if (nrow(Cl)) Cl <- Cl[, list(value = sum(value)), by = "k"][abs(value) >= ztol]
    T_all[[l + 1L]] <- Tl; C_all[[l + 1L]] <- Cl
    done_T <- data.table::rbindlist(list(done_T, Tl)); done_C <- data.table::rbindlist(list(done_C, Cl))
  }
  T <- data.table::rbindlist(T_all)[abs(coef) >= ztol]
  C <- data.table::rbindlist(C_all)
  if (nrow(C)) C <- C[abs(value) >= ztol]
  data.table::setorder(T, k, j)
  list(T = T, C = C, levels = L)
}

#' A reference to a substituted variable, lowered
#'
#' On the definition's domain (a gate on its tuples) the reference is the
#' definition at the referenced tuple; elsewhere it is the column lookup,
#' which finds no column on the domain.
#' @keywords internal
#' @noRd
.mx_subst_ref <- function(node, bound, L) {
  ctx <- L$ctx
  nm <- node$name
  ref <- .mx_bound_names(.mx_ref_dims(node), bound, ctx)
  if (!all(ref %in% bound)) return(.mx_lower_fail(L, "var:unbound"))
  def <- .mx_subst_def(nm, ctx)
  if (length(ref) != length(def$xenv)) return(.mx_lower_fail(L, "var:arity"))
  if (isTRUE(ctx$subst[[nm]]$recursive)) return(.mx_subst_ref_mat(node, ref, bound, .mx_subst_mat(nm, ctx), L))
  for (id in names(def$tables)) {
    if (is.null(L$tables[[id]])) L$tables[[id]] <- def$tables[[id]]
  }
  gid <- paste0("d:", nm)
  if (is.null(L$tables[[gid]])) L$tables[[gid]] <- as.list(ctx$subst[[nm]]$keys)
  ctx$subst_n <- (ctx$subst_n %||% 0L) + 1L
  sfx <- paste0("~", ctx$subst_n)
  map <- stats::setNames(ref, def$xenv)
  f <- function(n) ifelse(n %in% names(map), unname(map[n]), paste0(n, sfx))
  gate <- list(op = "gate", table = gid,
               on = stats::setNames(paste0("i", seq_along(ref)), ref))
  out <- lapply(.mx_rename_env(def$terms, f), function(t) {
    t$steps <- c(list(gate), t$steps)
    t
  })
  c(out, .mx_lower_var_lookup(node, bound, L))
}

#' A reference to a composed (self-referring) definition, lowered
#'
#' The composed definition is a table of (tuple, column, coefficient): the
#' reference expands over it at the referenced tuple, takes the coefficient
#' and the column from it; a second term adds the constants. Elsewhere the
#' reference is the column lookup.
#' @keywords internal
#' @noRd
.mx_subst_ref_mat <- function(node, ref, bound, mat, L) {
  ctx <- L$ctx
  nm <- node$name
  sp <- ctx$subst[[nm]]
  idx <- paste0("i", seq_along(ref))
  tid <- paste0("D:", nm)
  if (is.null(L$tables[[tid]])) {
    T <- mat$T
    L$tables[[tid]] <- c(stats::setNames(lapply(sp$keys, function(v) v[T$k]), idx),
                         list(jj = as.integer(T$j), c = as.numeric(T$coef)))
  }
  ctx$subst_n <- (ctx$subst_n %||% 0L) + 1L
  jn <- paste0("jj~", ctx$subst_n)
  on_k <- stats::setNames(idx, ref)
  on_kj <- stats::setNames(c(idx, "jj"), c(ref, jn))
  out <- list(.mx_term(list(
    list(op = "expand", table = tid, on = on_k, bind = stats::setNames("jj", jn)),
    list(op = "factor", expr = list(kind = "param", table = tid, on = on_kj, column = "c", default = 0)),
    list(op = "var", table = tid, on = on_kj, column = "jj"))))
  C <- mat$C
  if (nrow(C)) {
    cid <- paste0("K:", nm)
    if (is.null(L$tables[[cid]])) {
      L$tables[[cid]] <- c(stats::setNames(lapply(sp$keys, function(v) v[C$k]), idx),
                           list(value = as.numeric(C$value)))
    }
    out[[2L]] <- .mx_term(list(
      list(op = "gate", table = cid, on = on_k),
      list(op = "factor", expr = list(kind = "param", table = cid, on = on_k, column = "value", default = 0))))
  }
  c(out, .mx_lower_var_lookup(node, bound, L))
}

#' Evaluate the definitions over their domains and write them beside the
#' index tables
#'
#' One directory `<base>_subst/` with a `manifest.csv` and, per variable,
#' parts of its tuples (`keys`), its terms (`terms`: k, j, coef) and its
#' constants (`const`: k, value), `k` numbering the tuples. Where the
#' variable has a lower bound of 0 the parts are checked to imply it.
#' @keywords internal
#' @noRd
.mx_subst_export <- function(ctx, col_lo, dir, format, ztol, chunk = 4000000L, verbose = FALSE) {
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  codes <- ctx$codes
  man <- list()
  for (x in names(ctx$subst)) {
    t0 <- Sys.time()
    sp <- ctx$subst[[x]]
    def <- .mx_subst_def(x, ctx)
    mat <- if (isTRUE(sp$recursive)) .mx_subst_mat(x, ctx) else NULL
    if (is.null(mat)) {
      ex <- .mx_executor(ctx, def$tables)
      plan <- list(rows = "rows", row_keys = def$xenv, terms = def$terms)
    }
    vdir <- file.path(dir, x)
    dir.create(vdir, showWarnings = FALSE)
    n <- nrow(sp$keys)
    bad <- 0
    nnz <- 0
    for (p in seq_len(max(1L, ceiling(n / chunk)))) {
      r <- seq.int((p - 1L) * chunk + 1L, min(p * chunk, n))
      if (!length(r) || n == 0L) break
      if (is.null(mat)) {
        rt <- c(list(k = r), stats::setNames(lapply(sp$keys, function(v) v[r]), def$xenv))
        o <- mm_block_run(ex, plan, rt)
        tt <- data.table::data.table(k = o$i, j = o$j, coef = o$coef)
        cc <- data.table::data.table(k = o$rhs_i, value = -o$rhs)
      } else {
        # a self-referring definition: its composed columns
        tt <- mat$T[k >= r[1] & k <= r[length(r)]]
        cc <- if (nrow(mat$C)) mat$C[k >= r[1] & k <= r[length(r)]] else data.table::data.table(k = integer(), value = numeric())
      }
      tt <- tt[abs(coef) >= ztol]
      cc <- cc[abs(value) >= ztol]
      if (any(!is.finite(tt$coef)) || any(!is.finite(cc$value))) {
        stop("substitute: the definition of '", x, "' gives a non-finite coefficient.", call. = FALSE)
      }
      if (sp$lo == 0) {
        bad <- bad + sum(tt$coef < 0 | col_lo[tt$j] < 0) + sum(cc$value < 0)
      }
      nnz <- nnz + nrow(tt)
      kk <- data.table::data.table(k = r)
      for (cl in names(sp$keys)) {
        v <- sp$keys[[cl]][r]
        if (format == "csv") v <- codes$levels[v]
        data.table::set(kk, j = cl, value = v)
      }
      tag <- sprintf("%05d", p)
      .save_dataframe(data.table::setDF(kk), file.path(vdir, paste0("keys-", tag)), format = format)
      .save_dataframe(data.table::setDF(tt), file.path(vdir, paste0("terms-", tag)), format = format)
      .save_dataframe(data.table::setDF(cc), file.path(vdir, paste0("const-", tag)), format = format)
    }
    if (bad > 0) {
      stop("substitute: the definition of '", x, "' (", sp$eq, ") does not imply its ",
           "lower bound of 0 (", bad, " negative coefficients or free columns).", call. = FALSE)
    }
    man[[x]] <- data.frame(variable = x, equation = sp$eq, rows = n, nnz = nnz,
                           arity = ncol(sp$keys), stringsAsFactors = FALSE)
    if (verbose) {
      message(sprintf("  substituted %-24s by %-24s %d tuples, %d terms, %.1fs", x, sp$eq, n, nnz,
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }
  }
  utils::write.csv(do.call(rbind, man), file.path(dir, "manifest.csv"), row.names = FALSE)
  invisible(dir)
}

#' Rebuild the substituted variables of a decoded solution
#'
#' @param primal The decoded columns (`j` in row order, `value`).
#' @param dir The `<base>_subst/` directory.
#' @return `primal` with one row per tuple of each substituted variable
#'   appended (`j` NA, reduced cost NA).
#' @keywords internal
#' @noRd
.mx_subst_decode <- function(primal, dir, format) {
  val <- primal$value
  add <- list()
  for (x in .mx_subst_vars(dir)) {
    for (kk in .mx_subst_parts(x, dir, format, val)) {
      data.table::set(kk, j = "j", value = NA_integer_)
      data.table::set(kk, j = "symbol", value = x)
      data.table::set(kk, j = "reduced_cost", value = NA_real_)
      add[[length(add) + 1L]] <- kk
    }
  }
  if (!length(add)) return(primal)
  data.table::rbindlist(c(list(primal), add), use.names = TRUE, fill = TRUE)
}

#' The substituted variables of a bundle, in the order they were written
#' @keywords internal
#' @noRd
.mx_subst_vars <- function(dir) {
  utils::read.csv(file.path(dir, "manifest.csv"), stringsAsFactors = FALSE)$variable
}

#' One substituted variable's values, part by part
#'
#' @param val The solved values of the columns, by `j`.
#' @return A list of data.tables (`i1..iN`, `value`), one per part.
#' @keywords internal
#' @noRd
.mx_subst_parts <- function(x, dir, format, val) {
  ext <- c(parquet = ".parquet", ipc = ".arrow", csv = ".csv")[[format]]
  vdir <- file.path(dir, x)
  tags <- sort(sub(paste0("^keys-(.*)\\", ext, "$"), "\\1",
                   list.files(vdir, pattern = paste0("^keys-.*\\", ext, "$"))))
  lapply(tags, function(tag) {
    kk <- data.table::setDT(.load_dataframe(file.path(vdir, paste0("keys-", tag)), format))
    k0 <- kk$k[1] - 1L
    v <- numeric(nrow(kk))
    tt <- .load_dataframe(file.path(vdir, paste0("terms-", tag)), format)
    if (!is.null(tt) && nrow(tt)) {
      s <- rowsum(tt$coef * val[tt$j], tt$k - k0, reorder = FALSE)
      v[as.integer(rownames(s))] <- v[as.integer(rownames(s))] + s[, 1]
    }
    cc <- .load_dataframe(file.path(vdir, paste0("const-", tag)), format)
    if (!is.null(cc) && nrow(cc)) {
      s <- rowsum(cc$value, cc$k - k0, reorder = FALSE)
      v[as.integer(rownames(s))] <- v[as.integer(rownames(s))] + s[, 1]
    }
    data.table::set(kk, j = "k", value = NULL)
    data.table::set(kk, j = "value", value = v)
    kk
  })
}

#' One substituted variable as a long, dim-named data frame
#'
#' The shape `.solution_by_variable()` gives, built from the parts without
#' the variable's rows ever entering the decoded columns.
#' @keywords internal
#' @noRd
.mx_subst_frame <- function(x, sb, val, dn, lv, nonzero_only) {
  parts <- .mx_subst_parts(x, sb$dir, sb$format, val)
  if (nonzero_only) parts <- lapply(parts, function(p) p[which(p$value != 0)])
  blk <- data.table::rbindlist(parts, use.names = TRUE)
  rm(parts)
  if (!nrow(blk)) return(NULL)
  idx <- paste0("i", seq_along(dn))
  out <- as.data.frame(blk[, idx, with = FALSE])
  names(out) <- dn
  for (cl in dn) {
    if (is.factor(out[[cl]])) out[[cl]] <- as.character(out[[cl]])
    else if (!is.null(lv) && is.integer(out[[cl]])) out[[cl]] <- lv[out[[cl]]]
  }
  for (cl in intersect(c("year", "yearp", "yeare", "yearn", "year2"), dn)) {
    out[[cl]] <- suppressWarnings(as.integer(out[[cl]]))
  }
  out$value <- as.numeric(blk$value)
  out
}

utils::globalVariables(c("k", "kc", "w", "i.k", "i.w"))
