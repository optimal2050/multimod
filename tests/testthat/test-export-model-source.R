# export_model_source(): the energyRt example as a solver workspace

test_that("export_model_source() writes a workspace with the requested backends", {
  skip_on_cran()
  d <- file.path(withr::local_tempdir(), "ws")
  files <- export_model_source("energyrt", d, format = "gmpl", verbose = FALSE)
  rel <- sub(paste0("^", normalizePath(d, winslash = "/"), "/"), "",
             normalizePath(files, winslash = "/"))
  expect_setequal(rel, c("model.rds", "README.txt", "solvers/gmpl/model.mod",
                         "solvers/gmpl/data.dat"))
  expect_gt(length(readLines(file.path(d, "solvers", "gmpl", "data.dat"))), 100)

  # a second backend joins the same workspace; the first is kept
  files <- export_model_source("energyrt", d, format = "gams", verbose = FALSE)
  expect_identical(basename(files), "model.gms")
  expect_true(file.exists(file.path(d, "solvers", "gmpl", "model.mod")))
  expect_match(readLines(file.path(d, "solvers", "gams", "model.gms"), n = 1), "energyRt")

  # an existing backend is skipped unless overwrite = TRUE
  msgs <- testthat::capture_messages(
    files <- export_model_source("energyrt", d, format = "gams"))
  expect_true(any(grepl("Skipped", msgs)))
  expect_length(files, 0)
  files <- export_model_source("energyrt", d, format = "gams", overwrite = TRUE, verbose = FALSE)
  expect_setequal(basename(files), c("README.txt", "model.gms"))
})

test_that("export_model_source() renders JuMP and Pyomo from the parsed model", {
  skip_on_cran()
  d <- file.path(withr::local_tempdir(), "ws")
  export_model_source("energyrt", d, format = "all", verbose = FALSE)
  expect_true(all(file.exists(file.path(d, "solvers",
    c("gams/model.gms", "gmpl/model.mod", "gmpl/data.dat", "jump/model.jl",
      "jump/data.jl", "pyomo/model.py", "pyomo/data.py")))))
  jl <- paste(readLines(file.path(d, "solvers", "jump", "model.jl")), collapse = "\n")
  expect_match(jl, "@constraint", fixed = TRUE)
})

test_that("export_model_source() refuses a non-empty directory without a workspace", {
  d <- withr::local_tempdir()
  writeLines("x", file.path(d, "notes.txt"))
  expect_error(export_model_source("energyrt", d, format = "gams", verbose = FALSE), "not empty")
})
