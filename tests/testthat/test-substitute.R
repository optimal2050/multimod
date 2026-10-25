# Definition equations substituted while the streamed matrix is built

.subst_solve <- function(m, sub, format = "parquet", presolve = FALSE) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  f <- file.path(d, "model.mps")
  withr::with_options(list(multimod.engine = "native"),
    write_mps(m, f, stream = TRUE, format = format, substitute = sub, presolve = presolve))
  r <- solve_mps(f, verbose = FALSE)
  list(sol = read_mps_solution(r$sol_file, index_dir = d, format = format), dir = d)
}

# the decoded solution placed on the columns of the LP built without
# substitution: every variable is there, by label
.subst_full_x <- function(lp, p) {
  ci <- lp$col_index
  dn <- attr(ci, "dim_names")
  ic <- grep("^i[0-9]+$", names(ci), value = TRUE)
  key <- function(sym, cols) do.call(paste, c(list(sym), cols, sep = "\r"))
  ar <- vapply(dn, length, 1L)[ci$symbol]
  lab <- vapply(seq_len(nrow(ci)), function(r) {
    paste(c(ci$symbol[r], unlist(ci[r, ic, with = FALSE])[seq_len(ar[r])]), collapse = "\r")
  }, character(1))
  pd <- attr(p, "dim_names")
  x <- rep(NA_real_, nrow(ci))
  for (v in unique(p$symbol)) {
    t <- .solution_by_variable(p, v, pd)
    nm <- if (length(pd[[v]])) key(v, lapply(t[pd[[v]]], as.character)) else v
    x[match(nm, lab)] <- t$value
  }
  x
}

test_that("substituting a definition keeps the optimum and rebuilds the variable", {
  skip_if_not_installed("highs")
  skip_if_not_installed("arrow")
  m <- example_models$energyRt$multimod
  lp <- model_to_lp(m)
  for (pre in c(FALSE, TRUE)) {
    r <- .subst_solve(m, "eqTechActSng", presolve = pre)
    expect_true(file.exists(file.path(r$dir, "model_subst", "manifest.csv")))
    ri <- arrow::read_parquet(file.path(r$dir, "model_row_index.parquet"))
    expect_false("eqTechActSng" %in% ri$symbol)
    ci <- arrow::read_parquet(file.path(r$dir, "model_col_index.parquet"))
    expect_false("vTechOut" %in% ci$symbol)
    x <- .subst_full_x(lp, r$sol$primal)
    expect_false(anyNA(x))
    ax <- as.numeric(lp$A %*% x)
    expect_lt(max(pmax(lp$row_lo - ax, ax - lp$row_up, 0)), 1e-7)
    expect_equal(sum(lp$obj * x), r$sol$objective, tolerance = 1e-9)
  }
  base <- .subst_solve(m, NULL)
  expect_equal(r$sol$objective, base$sol$objective, tolerance = 1e-9)
  expect_false(dir.exists(file.path(base$dir, "model_subst")))
})

test_that("definitions referring to substituted variables are substituted in turn", {
  skip_if_not_installed("highs")
  skip_if_not_installed("arrow")
  m <- example_models$energyRt$multimod
  lp <- model_to_lp(m)
  defs <- c("eqTechActSng", "eqTechOutTot", "eqTechInpTot", "eqStorageInpTot",
            "eqStorageOutTot", "eqSupOutTot", "eqImportTot", "eqExportTot", "eqDemInp")
  r <- .subst_solve(m, defs, presolve = TRUE)
  x <- .subst_full_x(lp, r$sol$primal)
  expect_false(anyNA(x))
  ax <- as.numeric(lp$A %*% x)
  expect_lt(max(pmax(lp$row_lo - ax, ax - lp$row_up, 0)), 1e-7)
  expect_equal(sum(lp$obj * x), r$sol$objective, tolerance = 1e-9)
  # a variable defined through itself (a roll-up over timeslices) is not a definition
  f <- file.path(withr::local_tempdir(), "model.mps")
  expect_error(write_mps(m, f, stream = TRUE, substitute = "eqOutTot"), "does not define one variable")
})

test_that("substitution decodes from every index format", {
  skip_if_not_installed("highs")
  skip_if_not_installed("arrow")
  m <- example_models$energyRt$multimod
  objs <- vapply(c("parquet", "ipc", "csv"), function(fmt) {
    r <- .subst_solve(m, "eqTechActSng", format = fmt)
    out <- .solution_by_variable(r$sol$primal, "vTechOut", attr(r$sol$primal, "dim_names"))
    expect_true(is.character(out$tech))
    expect_equal(nrow(out), nrow(get_data(m, "mvTechOut", type = "mapping")))
    r$sol$objective
  }, numeric(1))
  expect_equal(unname(objs), rep(objs[[1]], 3), tolerance = 1e-12)
})

test_that("deferred substituted variables are written as the expanded ones", {
  skip_if_not_installed("highs")
  skip_if_not_installed("arrow")
  m <- example_models$energyRt$multimod
  defs <- c("eqTechActSng", "eqTechOutTot", "eqTechInpTot", "eqDemInp")
  d <- withr::local_tempdir()
  f <- file.path(d, "model.mps")
  withr::with_options(list(multimod.engine = "native"),
    write_mps(m, f, stream = TRUE, substitute = defs))
  r <- solve_mps(f, verbose = FALSE)
  out <- lapply(c("expand", "defer"), function(how) {
    s <- read_mps_solution(r$sol_file, index_dir = d, substituted = how)
    o <- file.path(d, how)
    write_energyrt_output(s, o, format = "parquet")
    list(s = s, o = o)
  })
  expect_false("vTechOut" %in% out[[2]]$s$primal$symbol)
  fa <- sort(list.files(out[[1]]$o))
  expect_identical(fa, sort(list.files(out[[2]]$o)))
  for (v in intersect(c("vTechOut", "vTechOutTot", "vTechInpTot", "vDemInp", "vTechAct"),
                      sub("[.]parquet$", "", fa))) {
    a <- as.data.frame(arrow::read_parquet(file.path(out[[1]]$o, paste0(v, ".parquet"))))
    b <- as.data.frame(arrow::read_parquet(file.path(out[[2]]$o, paste0(v, ".parquet"))))
    key <- function(x) x[do.call(order, x[setdiff(names(x), "value")]), , drop = FALSE]
    expect_equal(key(a), key(b), ignore_attr = TRUE, info = v)
  }
  rs <- lapply(out, function(x) read.csv(file.path(x$o, "raw_data_set.csv")))
  key <- function(x) x[order(x$set, x$value), ]
  expect_equal(key(rs[[1]]), key(rs[[2]]), ignore_attr = TRUE)
})

test_that("substitution refuses what it cannot substitute", {
  m <- example_models$energyRt$multimod
  f <- file.path(withr::local_tempdir(), "model.mps")
  expect_error(write_mps(m, f, stream = TRUE, substitute = "eqTechAfUp"), "not an equality")
  expect_error(write_mps(m, f, stream = TRUE, substitute = "eqNoSuch"), "no equation")
  expect_error(write_mps(m, f, substitute = "eqTechActSng"), "stream = TRUE")
})
