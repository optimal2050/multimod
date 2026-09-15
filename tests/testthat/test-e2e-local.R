# =============================================================================
# Fully local end-to-end of the MPS route
# =============================================================================
#
# The cloud workflow's only untested joint was the real solver leg: every
# prior round-trip either solved in-process (solve_highs) or fabricated the
# solution file (fake_sol). This file drives the WHOLE route on one machine:
#
#   model_to_lp -> write_mps -> solve_mps (HiGHS on the written file, real
#   .sol) -> read_mps_solution (positional decode via the index tables)
#   -> write_energyrt_output
#
# and compares VALUES, not just the objective, against the in-process
# solve. Reference constants come from data-raw/build_energyRt_fixture.R.

EXPECTED_OBJECTIVE <- 46398.4234209742

test_that("the written MPS solves locally and decodes to the same solution", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  d <- withr::local_tempdir()
  f <- file.path(d, "model.mps")
  lp <- model_to_lp(m)
  write_mps(m, f, lp = lp, format = "parquet")

  ref <- solve_highs(m, lp = lp, attach = FALSE)

  r <- solve_mps(f)
  expect_match(r$status_message, "Optimal")
  expect_equal(r$objective, EXPECTED_OBJECTIVE, tolerance = 1e-8)
  expect_true(file.exists(r$sol_file))

  s <- read_mps_solution(r$sol_file, index_dir = d, format = "parquet")
  expect_equal(s$objective, ref$objective, tolerance = 1e-8)

  # value-level: every column agrees with the in-process solve (same solver,
  # same matrix -- exact up to solver determinism)
  expect_equal(s$primal$j, ref$primal$j)
  expect_equal(s$primal$value, ref$primal$value, tolerance = 1e-6)
  # duals travel too
  expect_equal(s$dual$marginal, ref$dual$marginal, tolerance = 1e-6)
  expect_true(any(s$dual$marginal != 0))

  # decode to the energyRt exchange layout and compare per-variable values
  out_e2e <- file.path(d, "out_e2e")
  out_ref <- file.path(d, "out_ref")
  write_energyrt_output(s, out_e2e, format = "arrow")
  write_energyrt_output(ref, out_ref, format = "arrow")

  files <- list.files(out_e2e, pattern = "[.]arrow$")
  expect_true(length(files) > 0)
  expect_setequal(files, list.files(out_ref, pattern = "[.]arrow$"))
  for (fn in files) {
    a <- as.data.frame(arrow::read_ipc_file(file.path(out_e2e, fn)))
    b <- as.data.frame(arrow::read_ipc_file(file.path(out_ref, fn)))
    keys <- setdiff(names(a), "value")
    a <- a[do.call(order, a[keys]), , drop = FALSE]
    b <- b[do.call(order, b[keys]), , drop = FALSE]
    expect_equal(a[keys], b[keys], ignore_attr = TRUE, label = fn)
    expect_equal(a$value, b$value, tolerance = 1e-6, label = fn)
  }
})

test_that("solve_mps refuses a non-optimal result unless told otherwise", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  d <- withr::local_tempdir()
  f <- file.path(d, "model.mps")
  lp <- model_to_lp(m)
  # make it infeasible: an impossible bound on the first column
  lp2 <- lp
  lp2$col_lo[1] <- 10
  lp2$col_up[1] <- 5
  write_mps(m, f, lp = lp2, format = "csv")

  expect_error(solve_mps(f), "not optimal")
  r <- solve_mps(f, require_optimal = FALSE)
  expect_false(grepl("Optimal", r$status_message))
})
