# Lags and leads. The offset counts positions in the set's member order, not
# arithmetic on the member label, so it means the same on a sparse set as on a
# consecutive one. A linear lag off the end of the set yields no member and the
# term drops out; a circular one wraps. The two are different models, so
# `circular` is always declared rather than inferred.

test_that("a shift records whether it wraps", {
  lin <- ast_shift("y", -1)
  cir <- ast_shift("y", -1, circular = TRUE)
  expect_null(lin$circular)
  expect_true(cir$circular)
  # GAMS doubles the operator for the circular form
  expect_equal(as_gams(lin), "y-1")
  expect_equal(as_gams(cir), "y--1")
  expect_equal(as_gams(ast_shift("y", 2, circular = TRUE)), "y++2")
})

test_that("GAMS round-trips both forms", {
  m <- example_models$toy
  syms <- build_symbols_list(m)
  eq <- parse_gams_equation(
    list(name = "eqL", dims = list(), desc = "",
         gams = "vCost =e= vCap(tech,region)"), syms)
  expect_s3_class(eq, "mm_equation")

  lin <- parse_gams_expr("y-1", syms, character())
  cir <- parse_gams_expr("y--1", syms, character())
  expect_s3_class(lin, "shift")
  expect_s3_class(cir, "shift")
  expect_null(lin$circular)
  expect_true(cir$circular)
  expect_equal(lin$offset, -1L)
  expect_equal(cir$offset, -1L)
})

test_that("a writer without the syntax refuses rather than dropping the wrap", {
  cir <- ast_shift("y", -1, circular = TRUE)
  expect_error(as_gmpl(cir), "no syntax for a circular")
  expect_error(as_jump(cir), "no syntax for a circular")
  expect_error(as_pyomo(cir), "no syntax for a circular")
  # LaTeX and GAMS can both say it
  expect_match(as_latex(cir), "ominus", fixed = TRUE)
  expect_equal(as_gams(cir), "y--1")
  # and the linear form still writes everywhere
  lin <- ast_shift("y", -1)
  expect_equal(as_gmpl(lin), "y-1")
  expect_type(as_pyomo(lin), "character")
})

test_that("the matrix backend resolves a lag by position, and drops it off the end", {
  skip_if_not_installed("highs")
  # x[l] is tied to x of the previous member: with L = a b c, the equation
  # binds b->a and c->b, and the row for `a` has no predecessor so it is not
  # generated. Set members stay in declaration order, which is what the
  # positional offset counts.
  m <- read_gmpl(c(
    "set L;",
    "var x{l in L} >= 0;",
    "s.t. link{l in L}: x[l] >= 0;",
    "end;",
    "data;",
    "set L := a b c;",
    "end;"))
  expect_equal(m$sets$L$data, c("a", "b", "c"))

  # a reference carrying a lag resolves to the earlier member's column
  ci <- build_col_index(m)
  ctx <- new.env(parent = emptyenv())
  ctx$model <- m; ctx$col_index <- ci; ctx$col_cache <- list()
  ctx$codes <- multimod:::.mx_new_codes()
  ctx$param_cache <- list(); ctx$map_cache <- list(); ctx$set_cache <- list()
  # the evaluator binds members as integer codes of the context's dictionary
  env <- data.table::data.table(.k = 1:3,
                                l = multimod:::.mx_code(ctx, c("a", "b", "c")))

  lagged <- ast_variable("x", dims = ast_dims(ast_shift("l", -1)))
  tm <- multimod:::.mx_eval(lagged, env, ctx)
  # `a` has no predecessor, so only two terms survive
  expect_equal(nrow(tm), 2L)
  expect_setequal(tm$.k, c(2L, 3L))

  # the circular form keeps all three, wrapping `a` onto `c`
  wrapped <- ast_variable("x", dims = ast_dims(ast_shift("l", -1, circular = TRUE)))
  tmc <- multimod:::.mx_eval(wrapped, env, ctx)
  expect_equal(nrow(tmc), 3L)
  expect_setequal(tmc$.k, 1:3)
})

test_that("a positional lag works on a sparse set, where arithmetic would not", {
  # milestone years 2025, 2026, 2030: the member before 2030 is 2026, not 2029
  m <- read_gmpl(c(
    "set Y;",
    "var v{y in Y} >= 0;",
    "s.t. e{y in Y}: v[y] >= 0;",
    "end;",
    "data;",
    "set Y := 2025 2026 2030;",
    "end;"))
  ci <- build_col_index(m)
  ctx <- new.env(parent = emptyenv())
  ctx$model <- m; ctx$col_index <- ci; ctx$col_cache <- list()
  ctx$codes <- multimod:::.mx_new_codes()
  ctx$param_cache <- list(); ctx$map_cache <- list(); ctx$set_cache <- list()
  env <- data.table::data.table(.k = 1L, y = multimod:::.mx_code(ctx, "2030"))

  tm <- multimod:::.mx_eval(
    ast_variable("v", dims = ast_dims(ast_shift("y", -1))), env, ctx)
  expect_equal(nrow(tm), 1L)
  # the column it landed on is v for 2026
  expect_equal(ci$i1[ci$j == tm$j[1]], "2026")
})
