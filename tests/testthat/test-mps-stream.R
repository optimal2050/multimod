# =============================================================================
# Streamed MPS output: equivalent to the HiGHS-written file
# =============================================================================
#
# write_mps(stream = TRUE) spools the blocks to bucket files and writes the
# MPS itself. Both files are read back with HiGHS and written out again by
# HiGHS, so the comparison is between two renderings of the same reader's
# model: identical text means identical LPs.

normalised_mps <- function(file) {
  s <- highs::hi_new_solver(highs::highs_model(L = 0, lower = 0, upper = 0))
  highs::hi_solver_read_model(s, file)
  out <- tempfile(fileext = ".mps")
  highs::hi_solver_write_model(s, out)
  l <- readLines(out)
  l[!startsWith(l, "NAME")]   # HiGHS names the model after the file
}

expect_same_mps <- function(model, engine = "r") {
  skip_if_not_installed("highs")
  dir <- tempfile("mps-")
  dir.create(dir)
  a <- file.path(dir, "a.mps")
  b <- file.path(dir, "b.mps")
  write_mps(model, a, format = "csv")
  withr::with_options(list(multimod.engine = engine),
                      write_mps(model, b, format = "csv", stream = TRUE))
  expect_false(dir.exists(paste0(b, ".parts")))
  expect_identical(normalised_mps(b), normalised_mps(a))
  # the index tables of the two bundles are the same
  ia <- read.csv(file.path(dir, "a_col_index.csv")); ib <- read.csv(file.path(dir, "b_col_index.csv"))
  expect_equal(ib[, setdiff(names(ib), "rhs")], ia[, setdiff(names(ia), "rhs")])
  ra <- read.csv(file.path(dir, "a_row_index.csv")); rb <- read.csv(file.path(dir, "b_row_index.csv"))
  expect_equal(rb, ra)
  invisible(b)
}

test_that("the streamed MPS of the toy model equals the HiGHS-written one", {
  expect_same_mps(example_models$toy, "r")
  expect_same_mps(example_models$toy, "native")
})

test_that("the streamed MPS of the energyRt model equals the HiGHS-written one", {
  skip_on_cran()
  expect_same_mps(example_models$energyRt$multimod, "r")
  expect_same_mps(example_models$energyRt$multimod, "native")
})

expect_same_presolve <- function(model, engine = "r") {
  skip_if_not_installed("highs")
  dir <- tempfile("mps-")
  dir.create(dir)
  lp <- lp_presolve(model_to_lp(model))
  b <- file.path(dir, "b.mps")
  withr::with_options(list(multimod.engine = engine),
                      write_mps(model, b, format = "csv", stream = TRUE, presolve = TRUE))
  # the same substitution as lp_presolve(): same eliminated table, same kept columns
  elf <- file.path(dir, "b_eliminated.csv")
  el <- if (file.exists(elf)) read.csv(elf) else
    data.frame(col = integer(), ref = integer(), coef = numeric(), pass = integer())
  want <- as.data.frame(lp$eliminated)
  expect_equal(unname(as.list(el[order(el$pass, el$col), c("col", "ref", "coef", "pass")])),
               unname(as.list(want[order(want$pass, want$col), c("col", "ref", "coef", "pass")])))
  ci <- read.csv(file.path(dir, "b_col_index.csv"))
  if (nrow(want)) expect_identical(is.na(ci$jred), is.na(lp$col_index$jred))
  # the reduced LP solves to the same optimum, and the decoder restores the
  # full solution from the streamed bundle
  r <- solve_mps(b)
  s <- solve_highs(model, attach = FALSE)
  expect_equal(r$objective, s$objective, tolerance = 1e-9)
  sol <- read_mps_solution(r$sol_file, index_dir = dir, format = "csv")
  expect_equal(nrow(sol$primal), nrow(lp$col_index))
  expect_false(anyNA(sol$primal$value))
  invisible(b)
}

test_that("a streamed, presolved file matches lp_presolve()", {
  skip_on_cran()
  expect_same_presolve(example_models$energyRt$multimod, "r")
  expect_same_presolve(example_models$energyRt$multimod, "native")
  expect_same_presolve(example_models$toy, "native")   # no doubletons: nothing eliminated
})

test_that("a streamed file solves to the known optimum", {
  skip_if_not_installed("highs")
  f <- file.path(tempfile("mps-"), "toy.mps")
  write_mps(example_models$toy, f, format = "csv", stream = TRUE)
  r <- solve_mps(f)
  expect_equal(r$objective, 198000, tolerance = 1e-9)
})

test_that("a streamed parquet bundle decodes to the same members as a classic one", {
  skip_if_not_installed("highs")
  skip_if_not_installed("arrow")
  model <- example_models$toy
  dir <- tempfile("mps-")
  dir.create(dir)
  a <- file.path(dir, "a.mps")
  b <- file.path(dir, "b.mps")
  write_mps(model, a, format = "parquet")
  write_mps(model, b, format = "parquet", stream = TRUE)
  # the streamed table stores the members dictionary-encoded
  cb <- arrow::read_parquet(file.path(dir, "b_col_index.parquet"), mmap = FALSE)
  expect_true(is.factor(cb$i1))
  ca <- arrow::read_parquet(file.path(dir, "a_col_index.parquet"), mmap = FALSE)
  expect_identical(as.character(cb$i1), as.character(ca$i1))
  # the decoded solution is the same, member for member
  r <- solve_mps(b)
  sa <- read_mps_solution(r$sol_file, index_dir = dir, base = "a", format = "parquet")
  sb <- read_mps_solution(r$sol_file, index_dir = dir, base = "b", format = "parquet")
  da <- file.path(dir, "out_a"); db <- file.path(dir, "out_b")
  write_solution(sa, da, format = "csv"); write_solution(sb, db, format = "csv")
  for (f in list.files(da)) {
    expect_equal(read.csv(file.path(db, f)), read.csv(file.path(da, f)), info = f)
  }
})

test_that("a sliced expansion gives the same file", {
  skip_if_not_installed("highs")
  # a slice budget of a few rows forces every expansion through the slicer
  withr::with_options(list(multimod.slice_rows = 50), {
    expect_same_mps(example_models$toy, "native")
    expect_same_mps(example_models$energyRt$multimod, "native")
    expect_same_presolve(example_models$energyRt$multimod, "native")
  })
})
