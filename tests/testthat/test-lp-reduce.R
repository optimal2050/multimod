# =============================================================================
# lp_presolve(): exact removal of x = c * y equality rows
# =============================================================================

test_that("a chain of doubletons is eliminated and expanded exactly", {
  # min x1 + 2 x2 + 3 x3   s.t.  x1 - 2 x2 = 0,  x2 - x3 = 0,  x1 + x3 >= 3
  A <- Matrix::sparseMatrix(i = c(1, 1, 2, 2, 3, 3), j = c(1, 2, 2, 3, 1, 3),
                            x = c(1, -2, 1, -1, 1, 1), dims = c(3, 3))
  lp <- list(A = A, obj = c(1, 2, 3), row_lo = c(0, 0, 3), row_up = c(0, 0, Inf),
             col_lo = c(0, 0, 0), col_up = c(Inf, 10, Inf), maximum = FALSE,
             col_index = data.table::data.table(j = 1:3, symbol = "v"),
             row_index = data.table::data.table(i = 1:3, symbol = "e"))
  r <- lp_presolve(lp)
  expect_equal(ncol(r$A), 1L)
  expect_equal(nrow(r$A), 1L)
  expect_equal(nrow(r$eliminated), 2L)
  # the reduced LP: one column z with x1 = 2 z, x2 = z, x3 = z
  z <- 1
  full <- multimod:::.expand_eliminated(z, r$col_index$jred, r$eliminated)
  expect_equal(as.numeric(A %*% full)[1:2], c(0, 0))
  expect_equal(sum(r$obj * z), sum(lp$obj * full))
  # x2 <= 10 carries over to the kept column
  expect_lte(max(r$col_up), 10)
})

test_that("integer columns are never eliminated", {
  A <- Matrix::sparseMatrix(i = c(1, 1), j = c(1, 2), x = c(1, -1), dims = c(1, 2))
  lp <- list(A = A, obj = c(1, 1), row_lo = 0, row_up = 0, col_lo = c(0, 0),
             col_up = c(5, 5), maximum = FALSE,
             col_index = data.table::data.table(j = 1:2, vtype = c("integer", "integer")),
             row_index = data.table::data.table(i = 1L))
  r <- lp_presolve(lp)
  expect_equal(ncol(r$A), 2L)
  expect_equal(nrow(r$eliminated), 0L)
})

test_that("the fixture solves to the same optimum through an eliminated MPS", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  lp <- model_to_lp(m)
  d <- withr::local_tempdir()
  write_mps(m, file.path(d, "model.mps"), lp = lp, presolve = TRUE)
  expect_true(file.exists(file.path(d, "model_eliminated.parquet")))
  r <- solve_mps(file.path(d, "model.mps"))
  s <- read_mps_solution(file.path(d, "model.sol"), index_dir = d, solver = "highs")
  x <- s$primal$value
  expect_equal(length(x), ncol(lp$A))
  expect_equal(s$objective, 46398.4234209742, tolerance = 1e-6)
  expect_equal(sum(lp$obj * x), 46398.4234209742, tolerance = 1e-6)
  ax <- as.numeric(lp$A %*% x)
  expect_lt(max(pmax(lp$row_lo - ax, ax - lp$row_up, 0)), 1e-6)
  expect_lt(max(pmax(lp$col_lo - x, x - lp$col_up, 0)), 1e-9)

  # a later run without elimination removes the stale table
  write_mps(m, file.path(d, "model.mps"), lp = lp, presolve = FALSE)
  expect_false(file.exists(file.path(d, "model_eliminated.parquet")))
})
