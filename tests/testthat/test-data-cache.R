# =============================================================================
# The memo of tables read from disk, and models whose tables are references
# =============================================================================

local_cache <- function() {
  clear_data_cache()
  withr::defer(clear_data_cache(), envir = parent.frame())
}

test_that("get_data() reads a workspace table once per session", {
  skip_if_not_installed("arrow")
  local_cache()
  dir <- withr::local_tempdir()
  m <- save_model(example_models$toy, dir, format = "parquet", keep_in_memory = FALSE,
                  verbose = FALSE)
  nm <- names(m$mappings)[1]
  a <- get_data(m, nm, type = "mapping")
  expect_true(nrow(a) > 0)
  info <- data_cache_info()
  expect_true(any(grepl(nm, info$key, fixed = TRUE)))
  # the file can go: the memo answers
  unlink(file.path(dir, "mappings", nm), recursive = TRUE)
  expect_equal(get_data(m, nm, type = "mapping"), a)
  expect_equal(clear_data_cache(), nrow(info))
  expect_equal(nrow(data_cache_info()), 0L)
})

test_that("the memo keeps within its budget, dropping the least recently used", {
  skip_if_not_installed("arrow")
  local_cache()
  dir <- withr::local_tempdir()
  m <- save_model(example_models$energyRt$multimod, dir, format = "parquet",
                  keep_in_memory = FALSE, verbose = FALSE)
  maps <- names(m$mappings)[vapply(names(m$mappings), function(nm)
    file.exists(file.path(dir, "mappings", nm, "data.parquet")), logical(1))]
  skip_if(length(maps) < 3L)
  # a budget of a few kilobytes: one or two small tables fit, the oldest goes
  withr::local_options(multimod.data_cache_gb = 4e3 / 2^30)
  for (nm in maps[1:3]) get_data(m, nm, type = "mapping")
  info <- data_cache_info()
  expect_true(sum(info$bytes) <= 4e3)
  expect_false(any(grepl(maps[1], info$key, fixed = TRUE)) && nrow(info) == 3L)
})

test_that("a symbol with a reference reads through the memo, keyed by its hash", {
  skip_if_not_installed("arrow")
  local_cache()
  dir <- withr::local_tempdir()
  m <- example_models$toy
  nm <- names(m$parameters)[vapply(m$parameters, function(p) NROW(p$data) > 0, logical(1))][1]
  tbl <- m$parameters[[nm]]$data
  # the table in a directory of its own, as a store entry would hold it
  entry <- file.path(dir, "entry")
  arrow::write_dataset(tbl, file.path(entry, "data"), format = "parquet")
  m$parameters[[nm]]$data <- NULL
  m$parameters[[nm]]$misc <- list(inMemory = FALSE, path = entry,
                                  ref = list(hash = "abc123", nrow = nrow(tbl)))
  got <- get_data(m, nm, type = "parameter")
  expect_equal(as.data.frame(got)[order(got[[1]]), ], as.data.frame(tbl)[order(tbl[[1]]), ],
               ignore_attr = TRUE)
  expect_true("hash:abc123" %in% data_cache_info()$key)
  # replacing the table drops the reference and the memo entry
  m2 <- update_parameter(m, nm, tbl)
  expect_null(m2$parameters[[nm]]$misc$ref)
  expect_false("hash:abc123" %in% data_cache_info()$key)
})

test_that("a referenced model renders and folds like the one holding its tables", {
  skip_if_not_installed("arrow")
  local_cache()
  dir <- withr::local_tempdir()
  full <- example_models$toy
  lazy <- save_model(full, dir, format = "parquet", keep_in_memory = FALSE, verbose = FALSE)
  loaded <- load_model_data(lazy)
  for (nm in names(full$mappings)) {
    expect_equal(nrow(loaded$mappings[[nm]]$data), NROW(full$mappings[[nm]]$data), info = nm)
  }
  g1 <- withr::local_tempfile(fileext = ".gms"); g2 <- withr::local_tempfile(fileext = ".gms")
  write_gams(full, g1); write_gams(lazy, g2)
  expect_identical(readLines(g2), readLines(g1))
  d1 <- withr::local_tempdir(); d2 <- withr::local_tempdir()
  write_jump(full, model_dir = d1, cleanup = FALSE)
  write_jump(lazy, model_dir = d2, cleanup = FALSE)
  j1 <- list.files(d1, pattern = "[.]jl$", recursive = TRUE, full.names = TRUE)
  j2 <- list.files(d2, pattern = "[.]jl$", recursive = TRUE, full.names = TRUE)
  expect_true(length(j1) >= 1L)
  expect_identical(readLines(j2[1]), readLines(j1[1]))
})

test_that("a formula parameter is not re-evaluated when its table is a reference", {
  skip_if_not_installed("arrow")
  local_cache()
  dir <- withr::local_tempdir()
  m <- save_model(example_models$toy, dir, format = "parquet", keep_in_memory = FALSE,
                  verbose = FALSE)
  ns <- asNamespace("multimod")
  # nothing pending: every table is on disk and counted as data
  out <- ns$.mx_materialise_formulas(m)
  expect_identical(names(out$parameters), names(m$parameters))
})

test_that("a directory of parquet parts is detected and read as a dataset", {
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  tbl <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
  arrow::write_dataset(tbl, dir, format = "parquet")
  expect_identical(detect_arrow_format(dir), "parquet")
  got <- load_arrow_data(dir, collect = TRUE)
  expect_equal(as.data.frame(got)[order(got$a), ], tbl, ignore_attr = TRUE)
})
