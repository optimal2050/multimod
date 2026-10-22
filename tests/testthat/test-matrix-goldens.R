# =============================================================================
# Coefficient-level goldens of the matrix route (specs/goldens/matrix/)
# =============================================================================
#
# model_to_lp() and lp_presolve() must reproduce the stored tables exactly in
# structure and to 1e-12 in values. The goldens are written by
# `Rscript specs/goldens/generate.R`; they are not in the built package, so the
# tests skip when the directory is absent.

golden_root <- testthat::test_path("..", "..", "specs", "goldens")

skip_if_no_goldens <- function() {
  skip_if_not(dir.exists(file.path(golden_root, "matrix")),
              "specs/goldens/matrix not available (source checkout only)")
}

expect_golden_tables <- function(lp, dir, tol = 1e-12) {
  source(file.path(golden_root, "matrix-tables.R"), local = TRUE)
  got <- golden_matrix_tables(lp)
  want <- golden_read_tables(dir)

  expect_identical(got$meta$nrow, want$meta$nrow)
  expect_identical(got$meta$ncol, want$meta$ncol)
  expect_identical(got$meta$nnz, want$meta$nnz)
  expect_identical(got$meta$objective_symbol, want$meta$objective_symbol)

  # structure: positions, labels, senses, nonzero pattern
  expect_identical(got$rows$i, want$rows$i)
  expect_identical(got$rows$name, want$rows$name)
  expect_identical(got$rows$sense, want$rows$sense)
  expect_identical(got$cols$j, want$cols$j)
  expect_identical(got$cols$name, want$cols$name)
  expect_identical(got$triplets$i, want$triplets$i)
  expect_identical(got$triplets$j, want$triplets$j)
  if (!is.null(want$rows$full)) {
    expect_identical(got$rows$full, want$rows$full)
    expect_identical(got$cols$full, want$cols$full)
  }

  # values
  expect_equal(got$triplets$coef, want$triplets$coef, tolerance = tol)
  expect_equal(got$rows$lo, want$rows$lo, tolerance = tol)
  expect_equal(got$rows$up, want$rows$up, tolerance = tol)
  expect_equal(got$cols$lo, want$cols$lo, tolerance = tol)
  expect_equal(got$cols$up, want$cols$up, tolerance = tol)
  expect_equal(got$cols$obj, want$cols$obj, tolerance = tol)

  if (!is.null(want$eliminated)) {
    expect_identical(got$eliminated$col, want$eliminated$col)
    expect_identical(got$eliminated$ref, want$eliminated$ref)
    expect_identical(got$eliminated$pass, want$eliminated$pass)
    expect_equal(got$eliminated$coef, want$eliminated$coef, tolerance = tol)
  }
  invisible(want)
}

test_that("the toy model reproduces its matrix golden", {
  skip_if_no_goldens()
  lp <- model_to_lp(example_models$toy, verbose = FALSE)
  expect_golden_tables(lp, file.path(golden_root, "matrix", "toy"))
  expect_golden_tables(lp_presolve(lp, verbose = FALSE),
                       file.path(golden_root, "matrix", "toy", "presolve"))
})

test_that("the energyRt model reproduces its matrix golden", {
  skip_on_cran()
  skip_if_no_goldens()
  lp <- model_to_lp(example_models$energyRt$multimod, verbose = FALSE)
  want <- expect_golden_tables(lp, file.path(golden_root, "matrix", "utopia"))
  red <- lp_presolve(lp, verbose = FALSE)
  expect_golden_tables(red, file.path(golden_root, "matrix", "utopia", "presolve"))
  expect_gt(nrow(red$eliminated), 0L)
  expect_equal(want$meta$optimum, 46398.4234209742)
})

test_that("a local large model reproduces its golden when configured", {
  skip_on_cran()
  skip_if_no_goldens()
  dir <- Sys.getenv("MULTIMOD_GOLDEN_MODEL_DIR", "")
  skip_if(!nzchar(dir) || !dir.exists(file.path(dir, "goldens", "matrix")),
          "MULTIMOD_GOLDEN_MODEL_DIR not set or without goldens")
  m <- load_model(dir)
  lp <- model_to_lp(m, verbose = FALSE)
  expect_golden_tables(lp, file.path(dir, "goldens", "matrix"))
  expect_golden_tables(lp_presolve(lp, verbose = FALSE),
                       file.path(dir, "goldens", "matrix", "presolve"))
})
