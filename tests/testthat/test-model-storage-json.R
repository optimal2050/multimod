# save_model(structure = "json") writes a directory without model.rds;
# load_model() reads it back, and canonical = TRUE fixes the order of the
# matrix whatever the order the producer wrote.

toy_model <- function() {
  data(example_models, package = "multimod", envir = environment())
  example_models$toy
}

TOY_OBJECTIVE <- 198000

test_that("save_model(structure = 'json') writes model.json and load_model reads it back", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("highs")
  m <- toy_model()
  d <- withr::local_tempdir()
  save_model(m, d, structure = "json", verbose = FALSE)
  expect_false(file.exists(file.path(d, "model.rds")))
  expect_true(file.exists(file.path(d, "model.json")))
  meta <- jsonlite::fromJSON(file.path(d, "metadata.json"))
  expect_identical(meta$structure, "json")

  m2 <- load_model(d, verbose = FALSE)
  expect_s3_class(m2, "mm_model")
  expect_identical(names(m2$sets), names(m$sets))
  for (s in names(m$sets)) {
    expect_setequal(m2$sets[[s]]$data, m$sets[[s]]$data)
  }
  expect_identical(names(m2$equations), names(m$equations))
  expect_identical(names(m2$variables), names(m$variables))
  for (p in names(m$parameters)) {
    expect_equal(nrow(m2$parameters[[p]]$data), nrow(m$parameters[[p]]$data),
                 label = p)
  }
  for (k in names(m$mappings)) {
    expect_equal(nrow(m2$mappings[[k]]$data), nrow(m$mappings[[k]]$data),
                 label = k)
  }
  ref <- solve_highs(m, attach = FALSE)
  got <- solve_highs(m2, attach = FALSE)
  expect_equal(got$objective, ref$objective)
})

test_that("structure = 'both' writes model.rds and model.json; rds is read first", {
  skip_if_not_installed("arrow")
  m <- toy_model()
  d <- withr::local_tempdir()
  save_model(m, d, structure = "both", verbose = FALSE)
  expect_true(file.exists(file.path(d, "model.rds")))
  expect_true(file.exists(file.path(d, "model.json")))
  m2 <- load_model(d, verbose = FALSE)
  expect_identical(m2$sets$tech$data, m$sets$tech$data)
})

test_that("load_model refuses a directory with neither structure file", {
  d <- withr::local_tempdir()
  expect_error(load_model(d, verbose = FALSE), "neither model.rds nor model.json")
})

test_that("canonical = TRUE gives the same matrix whatever the producer's order", {
  skip_if_not_installed("arrow")
  m <- toy_model()
  a <- withr::local_tempdir()
  b <- withr::local_tempdir()
  save_model(m, a, structure = "json", verbose = FALSE)
  mb <- m
  for (s in names(mb$sets)) mb$sets[[s]]$data <- rev(mb$sets[[s]]$data)
  for (k in names(mb$mappings)) {
    d <- mb$mappings[[k]]$data
    mb$mappings[[k]]$data <- d[rev(seq_len(nrow(d))), , drop = FALSE]
  }
  save_model(mb, b, structure = "json", verbose = FALSE)

  ma <- load_model(a, canonical = TRUE, verbose = FALSE)
  mb2 <- load_model(b, canonical = TRUE, verbose = FALSE)
  for (s in names(m$sets)) {
    expect_identical(ma$sets[[s]]$data, sort(m$sets[[s]]$data, method = "radix"))
    expect_identical(mb2$sets[[s]]$data, ma$sets[[s]]$data)
  }
  for (k in names(m$mappings)) {
    expect_equal(mb2$mappings[[k]]$data, ma$mappings[[k]]$data,
                 ignore_attr = TRUE, label = k)
  }
  fa <- file.path(a, "model.mps")
  fb <- file.path(b, "model.mps")
  write_mps(ma, fa, format = "parquet", stream = TRUE)
  write_mps(mb2, fb, format = "parquet", stream = TRUE)
  expect_identical(readLines(fa), readLines(fb))
})
