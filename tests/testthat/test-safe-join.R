# =============================================================================
# Index-binding joins in sums: bounded intermediates, chunking
# =============================================================================
#
# `sum(tech$mTechOutComm(tech, comm), vTechOut(tech, comm, region, ...)$mvTechOut(...))`
# bound `tech` from the sum's own map, on `comm` alone, and crossed every row
# with every technology of that commodity before the body's region-keyed gate
# filtered it. On a large model that passed 2^31 rows. Body gates now bind
# the index too, the smallest exact join runs first, and a join above
# `multimod.join_rows` runs in blocks.
# =============================================================================

record_joins <- function(model) {
  rec <- new.env()
  rec$n <- numeric()
  rec$out <- numeric()
  orig <- multimod:::.mx_budget_merge
  lp <- testthat::with_mocked_bindings(
    model_to_lp(model),
    .mx_budget_merge = function(cur, plan, rest, resolve, iters, ctx) {
      out <- orig(cur, plan, rest, resolve, iters, ctx)
      rec$n <- c(rec$n, plan$n)
      rec$out <- c(rec$out, nrow(out))
      out
    },
    .package = "multimod")
  list(lp = lp, n = rec$n, out = rec$out)
}

test_that("no binding join builds far more rows than it keeps", {
  skip_on_cran()  # full energyRt model: run locally and in CI
  data(example_models, package = "multimod")
  r <- record_joins(example_models$energyRt$multimod)
  expect_gt(length(r$n), 0)
  expect_true(all(r$out <= r$n))
  expect_lt(max(r$n / pmax(r$out, 1)), 2)
  expect_equal(length(r$lp$A@x), 47476L)
})

test_that("a chunked build gives the identical matrix", {
  skip_on_cran()  # full energyRt model: run locally and in CI
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  lp1 <- model_to_lp(m)
  withr::local_options(multimod.join_rows = 50)
  lp2 <- model_to_lp(m)
  expect_identical(lp1$A, lp2$A)
  expect_identical(lp1$obj, lp2$obj)
  expect_identical(lp1$row_lo, lp2$row_lo)
  expect_identical(lp1$row_up, lp2$row_up)
})

test_that("a body gate binds the index and a summation alias is not a filter", {
  skip_on_cran()  # full energyRt model: run locally and in CI
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  span <- data.table::as.data.table(get_data(m, "mTechSpan", type = "mapping"))
  ctx <- new.env(parent = emptyenv())
  ctx$model <- m
  ctx$codes <- multimod:::.mx_new_codes()
  ctx$map_cache <- list()
  # the evaluator binds members as integer codes of the context's dictionary
  cur <- unique(span[, .(region = as.character(region), year = as.character(year))])
  cur <- multimod:::.mx_code_cols(ctx, cur, c("region", "year"))
  gate <- ast_mapping("mTechSpan", ast_dims(c("tech", "region", "year")))
  pl <- multimod:::.mx_join_plan(cur, gate, iters = "tech", ctx = ctx)
  expect_equal(pl$n, nrow(unique(span)))
  # `yearp` is still to be bound: the map must not be read as a filter on `year`
  alias <- ast_mapping("mTechSpan", ast_dims(c("tech", "region", "yearp")))
  cur2 <- cbind(cur, tech = multimod:::.mx_code(ctx, as.character(span$tech[1])))
  expect_null(multimod:::.mx_semi_join(cur2, alias, ctx, iters = "yearp"))
})
