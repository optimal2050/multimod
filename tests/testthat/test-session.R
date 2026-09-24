# =============================================================================
# MultimodSession - the R6 wrapper over the matrix route
# =============================================================================
#
# The session must be a convenience layer and nothing more: every method has to
# produce exactly what the functional call produces, because that is the path
# validated against GLPK. These tests pin that equivalence rather than the
# session's own bookkeeping.
#
# They also pin the reference-semantics behaviour, which is the actual risk of
# introducing R6 here: `s2 <- s1` aliases, and R6's own `$clone(deep = TRUE)`
# does NOT copy a data.table field - measured - so the class carries an
# explicit deep_clone.
# =============================================================================

skip_if_no_session <- function() {
  skip_if_not_installed("R6")
  skip_if_not_installed("highs")
}

fixture <- function() {
  data(example_models, package = "multimod")
  example_models$energyRt$multimod
}

test_that("a session builds the same LP as model_to_lp()", {
  skip_if_no_session()
  m <- fixture()
  ref <- model_to_lp(m)

  s <- mm_session(m)
  s$build_lp()

  expect_equal(dim(s$lp$A), dim(ref$A))
  expect_equal(length(s$lp$A@x), length(ref$A@x))
  expect_equal(s$lp$A@x, ref$A@x)
  expect_equal(s$lp$row_lo, ref$row_lo)
  expect_equal(s$lp$row_up, ref$row_up)
  expect_equal(nrow(s$lp$col_index), nrow(ref$col_index))
})

test_that("a session solves to the same objective as solve_highs()", {
  skip_if_no_session()
  m <- fixture()
  ref <- solve_highs(m, attach = FALSE)

  s <- mm_session(m)$build_lp()$solve()

  expect_equal(s$solution$objective, ref$objective, tolerance = 1e-10)
  expect_equal(s$solution$primal$value, ref$primal$value, tolerance = 1e-9)
})

test_that("build_lp() reuses the LP unless forced", {
  skip_if_no_session()
  s <- mm_session(fixture())$build_lp()
  first <- s$lp
  s$build_lp()
  # same object, not rebuilt
  expect_true(identical(first, s$lp))
  s$build_lp(force = TRUE)
  expect_equal(dim(s$lp$A), dim(first$A))
})

test_that("methods chain and return the session invisibly", {
  skip_if_no_session()
  s <- mm_session(fixture())
  out <- s$build_lp()
  expect_identical(out, s)
  expect_s3_class(s, "MultimodSession")
  expect_invisible(s$build_lp())
})

test_that("read_solution() refuses without the index tables", {
  skip_if_no_session()
  s <- mm_session(fixture())
  expect_error(s$read_solution("nowhere.sol"), "index tables")
})

test_that("write_output() refuses without a solution", {
  skip_if_no_session()
  s <- mm_session(fixture())
  expect_error(s$write_output(tempdir()), "No solution")
})

test_that("an empty session refuses to build", {
  skip_if_not_installed("R6")
  expect_error(mm_session()$build_lp(), "No model")
})

# ---------------------------------------------------------------------------
# Reference semantics - the reason this is a wrapper and not a replacement
# ---------------------------------------------------------------------------

test_that("a session is a reference, and assignment aliases it", {
  skip_if_no_session()
  s1 <- mm_session(fixture(), verbose = FALSE)
  s2 <- s1
  s2$opts$verbose <- TRUE
  # documented behaviour, pinned so it cannot change silently
  expect_true(s1$opts$verbose)
})

test_that("clone(deep = TRUE) isolates data.table fields", {
  skip_if_no_session()
  s1 <- mm_session(fixture())$build_lp()
  s2 <- s1$clone(deep = TRUE)

  # R6's own deep clone recurses into R6 fields only, so without the class's
  # deep_clone method these two would share one data.table and this set()
  # would mutate s1's index as well.
  data.table::set(s2$lp$col_index, j = "probe", value = 1L)

  expect_false("probe" %in% names(s1$lp$col_index))
  expect_true("probe" %in% names(s2$lp$col_index))
})

test_that("the functional API is unchanged by the session", {
  skip_if_no_session()
  m <- fixture()
  before <- length(m$equations)
  s <- mm_session(m)$build_lp()$solve()
  # the session holds its own reference; the caller's model is untouched
  expect_equal(length(m$equations), before)
  expect_null(m$variables[[1]]$solution)
})
