# =============================================================================
# The cloud return leg: cuOpt's solution format, and the full energyRt contract
# =============================================================================
#
# The GPU path solves with cuOpt, which writes a different shape from HiGHS --
# two comment lines and `name value` pairs, with no row section at all. The
# format below was taken from a real run (UTOPIA R3 on a T4, objective
# 36880.2354212843493).
#
# These tests close the gap the earlier ones left: the written index and the
# solution reading were each covered, but never joined, so nothing exercised
# write_mps() -> disk -> read_mps_solution(index_dir=) -> write_energyrt_output().
# =============================================================================

EXPECTED_OBJECTIVE <- 46398.4234209742

# A cuOpt solution file, in the exact layout cuopt_cli writes.
fake_cuopt <- function(path, values, objective, status = "Optimal",
                       names_vec = NULL) {
  if (is.null(names_vec)) names_vec <- paste0("c", seq_along(values) - 1L)
  writeLines(c(
    paste0("# Status: ", status),
    paste0("# Objective value: ", format(objective, digits = 17)),
    paste0(names_vec, " ", format(values, scientific = FALSE))
  ), path)
  path
}

# --- the parser ---------------------------------------------------------------

test_that("read_cuopt_solution parses status, objective and values", {
  f <- withr::local_tempfile(fileext = ".sol")
  fake_cuopt(f, c(0, 1.5, 42), objective = 36880.2354212843493)

  s <- read_cuopt_solution(f)
  expect_identical(s$status, "Optimal")
  expect_equal(s$objective, 36880.2354212843493, tolerance = 1e-12)
  expect_identical(s$columns$name, c("c0", "c1", "c2"))
  expect_equal(s$columns$primal, c(0, 1.5, 42))
  # cuOpt writes primals only; there is no dual information to be had
  expect_true(all(is.na(s$columns$dual)))
  expect_null(s$rows)
})

test_that("a non-optimal cuOpt status is refused unless asked for", {
  f <- withr::local_tempfile(fileext = ".sol")
  fake_cuopt(f, c(1, 2), objective = 5, status = "TimeLimit")
  expect_error(read_cuopt_solution(f), "not optimal")
  expect_identical(read_cuopt_solution(f, require_optimal = FALSE)$status,
                   "TimeLimit")
})

test_that("a malformed cuOpt line errors rather than reading partially", {
  f <- withr::local_tempfile(fileext = ".sol")
  writeLines(c("# Status: Optimal", "# Objective value: 1", "c0 1", "c1"), f)
  expect_error(read_cuopt_solution(f), "malformed")
})

test_that("the reader is chosen by sniffing the file", {
  cu <- withr::local_tempfile(fileext = ".sol")
  fake_cuopt(cu, c(1, 2), objective = 3)
  expect_identical(read_solver_solution(cu)$status, "Optimal")

  hi <- withr::local_tempfile(fileext = ".sol")
  writeLines(c("Model status", "Optimal", "",
               "# Primal solution values", "Feasible", "Objective 3",
               "# Columns 2", "c0 1", "c1 2", "# Rows 1", "r0 3"), hi)
  expect_equal(read_solver_solution(hi)$objective, 3)

  junk <- withr::local_tempfile(fileext = ".sol")
  writeLines(c("nothing", "recognisable"), junk)
  expect_error(read_solver_solution(junk), "Cannot tell which solver")
})

# --- placement by position, not by line order ---------------------------------

test_that("values are placed by their parsed index, not by line order", {
  # A solver that emits its columns out of order must still land each value in
  # the right slot; zipping by line order would silently transpose them.
  f <- withr::local_tempfile(fileext = ".sol")
  fake_cuopt(f, c(30, 10, 20), objective = 1,
             names_vec = c("c2", "c0", "c1"))
  s <- read_cuopt_solution(f)
  placed <- multimod:::.place_by_position(s$columns$name, s$columns$primal,
                                          3L, "c", "column")
  expect_equal(placed, c(10, 20, 30))
})

test_that("a duplicate or out-of-range position is an error", {
  expect_error(
    multimod:::.place_by_position(c("c0", "c0"), c(1, 2), 2L, "c", "column"),
    "more than once")
  expect_error(
    multimod:::.place_by_position(c("c0", "c9"), c(1, 2), 2L, "c", "column"),
    "not a c<position>")
  expect_error(
    multimod:::.place_by_position(c("c0"), 1, 2L, "c", "column"),
    "must describe the same model")
})

# --- the full chain, through disk ---------------------------------------------

test_that("write_mps -> disk -> read_mps_solution -> write_energyrt_output", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  lp <- model_to_lp(m)
  ref <- solve_highs(m, lp = lp, attach = FALSE)

  d <- withr::local_tempdir()
  write_mps(m, file.path(d, "model.mps"), lp = lp, format = "parquet")

  # cuOpt's answer to the same model, in cuOpt's format
  f <- file.path(d, "model.sol")
  fake_cuopt(f, ref$primal$value, objective = ref$objective)

  # index read back from disk -- the link the earlier tests never exercised
  s <- read_mps_solution(f, index_dir = d, format = "parquet")
  expect_equal(s$objective, EXPECTED_OBJECTIVE, tolerance = 1e-8)
  expect_equal(s$primal$value, ref$primal$value, tolerance = 1e-9)
  expect_identical(s$primal$symbol, ref$primal$symbol)
  # no duals came back, and that must be visible rather than fabricated
  expect_true(all(is.na(s$dual$marginal)))

  out <- file.path(d, "output")
  write_energyrt_output(s, out)

  # the four files energyRt's read_solution() requires
  expect_true(file.exists(file.path(out, "variable_list.csv")))
  expect_true(file.exists(file.path(out, "raw_data_set.csv")))
  expect_true(file.exists(file.path(out, "log.csv")))
  expect_true(length(list.files(out, pattern = "\\.arrow$")) > 0)

  vl <- readLines(file.path(out, "variable_list.csv"))
  expect_identical(vl[1], "value")     # the literal header energyRt looks for

  lg <- readLines(file.path(out, "log.csv"))
  expect_identical(lg[1], "parameter,value,time")
  expect_true(any(grepl('"solution status",1', lg, fixed = TRUE)))
  expect_true(any(grepl('"done"', lg, fixed = TRUE)))

  rds <- utils::read.csv(file.path(out, "raw_data_set.csv"),
                         stringsAsFactors = FALSE)
  expect_identical(names(rds), c("set", "value"))
  # every dimension a written variable uses must have its set listed, or that
  # column reads back as all NA without complaint
  dn <- attr(s$primal, "dim_names")
  used <- unique(unlist(dn[intersect(names(dn), unique(s$primal$symbol))]))
  expect_true(all(used %in% rds$set))
})

test_that("a non-optimal solution is not logged as solved", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  lp <- model_to_lp(m)
  ref <- solve_highs(m, lp = lp, attach = FALSE)

  d <- withr::local_tempdir()
  f <- file.path(d, "capped.sol")
  fake_cuopt(f, ref$primal$value, objective = ref$objective,
             status = "TimeLimit")

  s <- read_mps_solution(f, col_index = lp$col_index, row_index = lp$row_index,
                         require_optimal = FALSE)
  out <- file.path(d, "output")
  write_energyrt_output(s, out)

  lg <- readLines(file.path(out, "log.csv"))
  # a feasible-but-not-optimal primal reported as solved is the worst failure
  # available here: plausible numbers, presented as the optimum
  expect_true(any(grepl('"solution status",0', lg, fixed = TRUE)))
  expect_false(any(grepl('"solution status",1', lg, fixed = TRUE)))
})
