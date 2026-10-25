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
    once <- names(which(table(vn) == 1L))
    cand <- which(vapply(seq_along(vars), function(k) {
      v <- vars[[k]]
      if (!vn[k] %in% once || vn[k] %in% names(spec) || .mx_has_shift(v)) return(FALSE)
      r <- .mx_bound_names(.mx_ref_dims(v), dims, ctx0)
      length(r) == length(dims) && !anyDuplicated(r) && setequal(r, dims)
    }, logical(1)))
    if (length(cand) != 1L) {
      stop("substitute: '", en, "' does not define one variable over its domain ",
           "(candidates: ", if (length(cand)) paste(vn[cand], collapse = ", ") else "none",
           ").", call. = FALSE)
    }
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
    spec[[x]] <- list(var = x, eq = en, keys = keys, lo = b$lo)
  }
  spec
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
    any(vapply(t$steps, function(s) identical(s$op, "var") && identical(s$table, vid), logical(1)))
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
    ex <- .mx_executor(ctx, def$tables)
    plan <- list(rows = "rows", row_keys = def$xenv, terms = def$terms)
    vdir <- file.path(dir, x)
    dir.create(vdir, showWarnings = FALSE)
    n <- nrow(sp$keys)
    bad <- 0
    nnz <- 0
    for (p in seq_len(max(1L, ceiling(n / chunk)))) {
      r <- seq.int((p - 1L) * chunk + 1L, min(p * chunk, n))
      if (!length(r) || n == 0L) break
      rt <- c(list(k = r), stats::setNames(lapply(sp$keys, function(v) v[r]), def$xenv))
      o <- mm_block_run(ex, plan, rt)
      tt <- data.table::data.table(k = o$i, j = o$j, coef = o$coef)
      tt <- tt[abs(coef) >= ztol]
      cc <- data.table::data.table(k = o$rhs_i, value = -o$rhs)
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
