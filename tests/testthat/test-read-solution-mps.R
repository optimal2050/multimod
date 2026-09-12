# =============================================================================
# Reading a solved MPS back into a model
# =============================================================================
#
# The return leg of the cloud workflow. write_mps() emits positional names
# (c0, r0, ...), so a solution file is only interpretable together with the
# col_index / row_index tables written beside it.
#
# The round-trip tests fabricate a HiGHS-format solution file from solve_highs()'s
# own answer rather than shelling out to a solver, so they need no external
# toolchain. The format itself was verified against highspy 1.15.1
# (Highs.writeSolution(path, 0)).
# =============================================================================

EXPECTED_OBJECTIVE <- 46398.4234209742

# Write a HiGHS raw-format solution file from known values.
fake_sol <- function(path, col_primal, col_dual, row_primal, row_dual,
                     objective, status = "Optimal") {
  nc <- length(col_primal)
  nr <- length(row_primal)
  writeLines(c(
    "Model status", status, "",
    "# Primal solution values", "Feasible",
    paste("Objective", format(objective, digits = 17)),
    paste("# Columns", nc), paste0("c", seq_len(nc) - 1L, " ", col_primal),
    paste("# Rows", nr), paste0("r", seq_len(nr) - 1L, " ", row_primal), "",
    "# Dual solution values", "Feasible",
    paste("# Columns", nc), paste0("c", seq_len(nc) - 1L, " ", col_dual),
    paste("# Rows", nr), paste0("r", seq_len(nr) - 1L, " ", row_dual)
  ), path)
  path
}

test_that("read_highs_solution parses the raw format", {
  f <- withr::local_tempfile(fileext = ".sol")
  fake_sol(f, c(1.5, 0), c(0, 2), c(10, 1), c(0.25, 0), objective = 1.5)

  s <- read_highs_solution(f)

  expect_equal(s$status, "Optimal")
  expect_equal(s$objective, 1.5)
  expect_equal(s$columns$name, c("c0", "c1"))
  expect_equal(s$columns$primal, c(1.5, 0))
  expect_equal(s$columns$dual, c(0, 2))
  expect_equal(s$rows$primal, c(10, 1))
  expect_equal(s$rows$dual, c(0.25, 0))
})

test_that("a non-optimal status is refused unless asked for", {
  f <- withr::local_tempfile(fileext = ".sol")
  fake_sol(f, 1, 0, 1, 0, objective = 1, status = "Infeasible")

  expect_error(read_highs_solution(f), "not optimal")
  expect_equal(read_highs_solution(f, require_optimal = FALSE)$status, "Infeasible")
})

test_that("a truncated solution file errors rather than reading partially", {
  f <- withr::local_tempfile(fileext = ".sol")
  fake_sol(f, c(1, 2, 3), c(0, 0, 0), c(1, 2), c(0, 0), objective = 1)
  ln <- readLines(f)
  # drop the last two column entries but leave the declared count at 3
  writeLines(ln[-c(9, 10)], f)

  # either the count check or the primal/dual length check must catch it
  expect_error(read_highs_solution(f))
})

test_that("a solution for a different model is rejected", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  lp <- model_to_lp(m)

  f <- withr::local_tempfile(fileext = ".sol")
  # deliberately one column short of the index
  n <- nrow(lp$col_index) - 1L
  fake_sol(f, rep(0, n), rep(0, n),
           rep(0, nrow(lp$row_index)), rep(0, nrow(lp$row_index)), objective = 0)

  expect_error(
    read_mps_solution(f, col_index = lp$col_index, row_index = lp$row_index),
    "must describe the same model"
  )
})

test_that("read_mps_solution reproduces the in-process solve exactly", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  lp <- model_to_lp(m)
  ref <- solve_highs(m, lp = lp, attach = FALSE)

  f <- withr::local_tempfile(fileext = ".sol")
  fake_sol(f, ref$primal$value, ref$primal$reduced_cost,
           ref$dual$value, ref$dual$marginal, objective = ref$objective)

  s <- read_mps_solution(f, col_index = lp$col_index, row_index = lp$row_index)

  expect_equal(s$objective, EXPECTED_OBJECTIVE, tolerance = 1e-8)
  expect_equal(s$primal$j, ref$primal$j)
  expect_equal(s$primal$value, ref$primal$value, tolerance = 1e-9)
  expect_equal(s$dual$marginal, ref$dual$marginal, tolerance = 1e-9)
  # marginals are the reason for carrying the dual section at all
  expect_true(any(s$dual$marginal != 0))
})

test_that("index tables round-trip through write_mps and are found again", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  d <- withr::local_tempdir()
  lp <- model_to_lp(m)
  write_mps(m, file.path(d, "model.mps"), lp = lp)

  # the dim-name sidecar keeps the bundle self-describing even for csv, which
  # does not preserve R attributes
  expect_true(file.exists(file.path(d, "model_dim_names.csv")))

  idx <- multimod:::.load_mps_index(d, NULL, "parquet")
  expect_equal(nrow(idx$col_index), nrow(lp$col_index))
  expect_equal(nrow(idx$row_index), nrow(lp$row_index))
  expect_equal(attr(idx$col_index, "dim_names")$vTechAct,
               c("tech", "region", "year", "timeslice"))
})

test_that("write_energyrt_output writes one file per variable, nonzeros only", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  lp <- model_to_lp(m)
  r <- solve_highs(m, lp = lp, attach = FALSE)
  d <- withr::local_tempdir()
  written <- write_energyrt_output(r, d, format = "arrow")

  vl <- readLines(file.path(d, "variable_list.csv"))
  expect_equal(vl[1], "value")                       # energyRt's header
  expect_true(length(vl) > 1)
  # One .arrow per listed variable, plus the three files energyRt's
  # read_solution() requires. Asserted by name rather than by a bare count:
  # the count silently drifts whenever the contract gains a file, which is
  # exactly what happened when raw_data_set.csv and log.csv were added.
  n_vars <- length(vl) - 1L                          # minus the "value" header
  expect_equal(sum(grepl("[.]arrow$", written)), n_vars)
  expect_setequal(
    basename(written[!grepl("[.]arrow$", written)]),
    c("variable_list.csv", "raw_data_set.csv", "log.csv"))

  # each file carries that variable's own dimension names, not i1..iN
  act <- arrow::read_feather(file.path(d, "vTechAct.arrow"))
  expect_equal(names(act), c("tech", "region", "year", "timeslice", "value"))
  expect_type(act$year, "integer")                   # energyRt stores year as int
  expect_true(all(act$value != 0))                   # nonzeros only

  # every variable's block must be its own rows - a name collision with the
  # `symbol` column previously gave every file the full nonzero set
  expect_true(nrow(act) < sum(r$primal$value != 0))
  expect_true(all(act$tech %in% unlist(m$sets$tech$data)))
})
