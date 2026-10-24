# =============================================================================
# Implied bounds: the interval of every variable contains the optimum
# =============================================================================

expect_within <- function(x, lo, up, tol = 1e-6) {
  expect_true(all(x >= lo - tol * pmax(1, abs(lo))), label = "below an implied lower bound")
  expect_true(all(x <= up + tol * pmax(1, abs(up))), label = "above an implied upper bound")
}

test_that("the implied intervals of the toy model contain its optimum and tighten", {
  skip_if_not_installed("highs")
  lp <- model_to_lp(example_models$toy, verbose = FALSE)
  s <- solve_highs(example_models$toy, lp = lp, attach = FALSE)
  b <- lp_bounds(lp)
  ci <- b$col_index
  expect_length(b$bounds$infeasible, 0L)
  expect_gt(b$bounds$tightened, 0L)
  expect_true(all(ci$lo_impl >= lp$col_lo & ci$up_impl <= lp$col_up))
  expect_within(s$primal$value, ci$lo_impl, ci$up_impl)
  # a second propagation from the implied bounds moves nothing
  lp2 <- lp; lp2$col_lo <- ci$lo_impl; lp2$col_up <- ci$up_impl
  b2 <- lp_bounds(lp2)
  expect_identical(b2$bounds$passes, 1L)
  expect_equal(b2$col_index$lo_impl, ci$lo_impl)
  expect_equal(b2$col_index$up_impl, ci$up_impl)
  # the objective cut at the optimum closes the open ends the rows leave
  bo <- lp_bounds(lp, objective = s$objective)
  expect_within(s$primal$value, bo$col_index$lo_impl, bo$col_index$up_impl)
  expect_lte(bo$bounds$open, b$bounds$open)
  expect_true(all(bo$col_index$up_impl <= ci$up_impl + 1e-9))
})

test_that("the implied intervals of the energyRt model contain its optimum, before and after presolve", {
  skip_on_cran()
  skip_if_not_installed("highs")
  m <- example_models$energyRt$multimod
  lp <- model_to_lp(m, verbose = FALSE)
  s <- solve_highs(m, lp = lp, attach = FALSE)
  b <- lp_bounds(lp)
  expect_length(b$bounds$infeasible, 0L)
  expect_gt(b$bounds$tightened, 0L)
  expect_within(s$primal$value, b$col_index$lo_impl, b$col_index$up_impl)
  bo <- lp_bounds(lp, objective = s$objective)
  expect_within(s$primal$value, bo$col_index$lo_impl, bo$col_index$up_impl)
  expect_lt(bo$bounds$open, b$bounds$open)
  # the reduced LP: the kept columns' intervals contain the reduced optimum
  red <- lp_presolve(lp, verbose = FALSE)
  br <- lp_bounds(red)
  expect_length(br$bounds$infeasible, 0L)
  kept <- !is.na(red$col_index$jred)
  expect_true(all(is.na(br$col_index$lo_impl[!kept])))
  expect_within(s$primal$value[kept], br$col_index$lo_impl[kept], br$col_index$up_impl[kept])
})

test_that("a row with one unbounded variable bounds it, and a contradiction is reported", {
  # x + y <= 5, x >= 0, y free, y >= -1 through the second row
  A <- Matrix::sparseMatrix(i = c(1, 1, 2), j = c(1, 2, 2), x = c(1, 1, 1), dims = c(2, 2))
  lp <- list(A = A, obj = c(1, 1), row_lo = c(-Inf, -1), row_up = c(5, Inf),
             col_lo = c(0, -Inf), col_up = c(Inf, Inf), maximum = FALSE,
             col_index = data.frame(j = 1:2, symbol = c("x", "y")))
  b <- lp_bounds(lp)
  expect_equal(b$col_index$lo_impl, c(0, -1))
  expect_equal(b$col_index$up_impl, c(6, 5))
  # the same with x >= 7: the first row cannot hold (and the contradiction
  # reaches the second through y)
  lp$col_lo <- c(7, -Inf)
  expect_true(1L %in% lp_bounds(lp)$bounds$infeasible)
})
