# =============================================================================
# LP assembly, HiGHS solve, and MPS export
# =============================================================================
#
# Gates come from an INDEPENDENT oracle: energyRt generates and solves its own
# GMPL via glpsol, never touching multimod.
#
#   energyRt 0.89.5.9001, UTOPIA R7 kit, utopia_seasons calendar, base horizon
#     columns   : 18,298
#     rows      : 18,163   (energyRt reports 18,164; GLPK adds one objective row)
#     nonzeros  : 43,612   (energyRt's own matrix has 43,613; the difference is
#                           vObjective, which energyRt materialises and
#                           multimod's GMPL substitutes away)
#     objective : 46398.4234209742
#
# The coefficients, right-hand sides, senses and column bounds were additionally
# compared entry-by-entry against GLPK: zero differences.
#
# If the fixture is rebuilt against a different energyRt these numbers move.
# Re-derive them with data-raw/build_energyRt_fixture.R, not by hand.
# =============================================================================

EXPECTED_OBJECTIVE <- 46398.4234209742

test_that("model_to_lp assembles a matrix of the right shape", {
  data(example_models, package = "multimod")
  lp <- model_to_lp(example_models$energyRt$multimod)

  expect_s4_class(lp$A, "dgCMatrix")
  expect_equal(nrow(lp$A), 18163L)
  expect_equal(ncol(lp$A), 18298L)
  expect_equal(length(lp$A@x), 43612L)

  # one bound pair per row/column
  expect_length(lp$row_lo, nrow(lp$A))
  expect_length(lp$row_up, nrow(lp$A))
  expect_length(lp$col_lo, ncol(lp$A))
  expect_length(lp$col_up, ncol(lp$A))
  expect_true(all(lp$row_lo <= lp$row_up))
  expect_true(all(lp$col_lo <= lp$col_up))
})

test_that("row bounds encode the constraint sense", {
  data(example_models, package = "multimod")
  lp <- model_to_lp(example_models$energyRt$multimod)
  s <- lp$row_index$sense

  # equality: both bounds equal; <=: no lower bound; >=: no upper bound
  expect_true(all(lp$row_lo[s == "=="] == lp$row_up[s == "=="]))
  expect_true(all(is.infinite(lp$row_lo[s == "<="]) & lp$row_lo[s == "<="] < 0))
  expect_true(all(is.infinite(lp$row_up[s == ">="]) & lp$row_up[s == ">="] > 0))
})

test_that("the objective is an indicator on the declared objective variable", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  lp <- model_to_lp(m)

  expect_equal(sum(lp$obj != 0), 1L)
  j <- which(lp$obj != 0)
  expect_equal(lp$col_index$symbol[j], m$objectives[[1]]$variable)
  expect_false(lp$maximum)                       # energyRt minimises
})

test_that("solve_highs reproduces the reference objective", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")

  r <- solve_highs(example_models$energyRt$multimod, attach = FALSE)

  expect_equal(r$status_message, "Optimal")
  expect_equal(r$objective, EXPECTED_OBJECTIVE, tolerance = 1e-8)
})

test_that("duals are returned, not just primal values", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")

  r <- solve_highs(example_models$energyRt$multimod, attach = FALSE)

  # marginals are the reason for using the solver-object path over highs_solve()
  expect_true(any(!is.na(r$dual$marginal)))
  expect_true(any(r$dual$marginal != 0))
  expect_equal(nrow(r$primal), nrow(r$lp$col_index))
  expect_equal(nrow(r$dual), nrow(r$lp$row_index))
})

test_that("primal values attach back with real index columns", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")

  r <- solve_highs(example_models$energyRt$multimod, attach = TRUE)
  sol <- r$model$variables$vTechAct$solution

  expect_true(is.data.frame(sol))
  # named index columns, not i1..iN - downstream readers expect the set names
  expect_equal(names(sol), c("tech", "region", "year", "timeslice", "value"))
  expect_true(nrow(sol) > 0)
})

test_that("write_mps round-trips through HiGHS to the same objective", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  d <- withr::local_tempdir()
  f <- file.path(d, "model.mps")
  paths <- write_mps(m, f, format = "csv")

  expect_true(file.exists(paths$mps))
  # the index tables must travel with the .mps: HiGHS drops symbolic names
  expect_true(length(list.files(d, pattern = "col_index")) > 0)
  expect_true(length(list.files(d, pattern = "row_index")) > 0)

  s <- highs::hi_new_solver(highs::highs_model(L = 0, lower = 0, upper = 0))
  highs::hi_solver_read_model(s, f)
  expect_equal(highs::hi_solver_get_num_row(s), 18163L)
  expect_equal(highs::hi_solver_get_num_col(s), 18298L)

  highs::hi_solver_run(s)
  expect_equal(highs::hi_solver_status_message(s), "Optimal")

  lp <- model_to_lp(m)
  obj <- sum(lp$obj * highs::hi_solver_get_solution(s)$col_value)
  expect_equal(obj, EXPECTED_OBJECTIVE, tolerance = 1e-8)
})


# =============================================================================
# Numerical health of the assembled matrix
# =============================================================================
#
# The report is computed from the triplets and the index tables, not from a
# written .mps - our own file carries positional names (c0, r0), so a
# file-based scan collapses every entry into one "family" and says nothing
# actionable about which equation or variable is responsible.
# =============================================================================

test_that("check_matrix_numbers reports each section's magnitude spectrum", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  lp <- model_to_lp(example_models$energyRt$multimod)

  res <- check_matrix_numbers(lp, verbose = FALSE)

  expect_setequal(res$summary$section,
                  c("COLUMNS", "OBJECTIVE", "RHS", "BOUNDS"))
  cols <- res$summary[res$summary$section == "COLUMNS", ]
  expect_equal(cols$entries, length(lp$A@x))
  expect_equal(cols$min_abs, min(abs(lp$A@x[lp$A@x != 0])))
  expect_equal(cols$max_abs, max(abs(lp$A@x)))

  # the objective is an indicator on one column
  expect_equal(res$summary$entries[res$summary$section == "OBJECTIVE"], 1L)
})

test_that("offenders are attributed to real equation and variable names", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  lp <- model_to_lp(example_models$energyRt$multimod)

  # a threshold above the smallest coefficients, so there is something to find
  cut <- stats::quantile(abs(lp$A@x[lp$A@x != 0]), 0.05)
  res <- check_matrix_numbers(lp, small = cut, verbose = FALSE)

  expect_gt(nrow(res$offenders_row), 0)
  expect_true(all(res$offenders_row$family %in% names(example_models$energyRt$multimod$equations)))
  expect_true(all(res$offenders_col$family %in% names(example_models$energyRt$multimod$variables)))
  # every offending entry is accounted for in both tallies
  n_small <- sum(abs(lp$A@x) > 0 & abs(lp$A@x) < cut)
  expect_lte(sum(res$offenders_row$n_small), n_small)
  expect_gt(sum(res$offenders_row$n_small), 0)
})

test_that("a matrix with no small entries yields empty offender tables", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  lp <- model_to_lp(example_models$energyRt$multimod)

  res <- check_matrix_numbers(lp, small = 1e-300, verbose = FALSE)
  expect_equal(nrow(res$offenders_row), 0L)
  expect_equal(sum(res$summary$below_small), 0L)
})


# =============================================================================
# Infinite parameter values mean "no restriction", not an infinite coefficient
# =============================================================================
#
# Every energyRt backend drops parameter rows whose value is Inf and lets the
# default apply, and maps an infinite DEFAULT to 0:
#
#   write_pyomo.R:493  data <- data[data$value != Inf & data$value != def, ]
#   write_jump.R:287   (same)
#   write_glpk.R:237   fl <- obj@data[["value"]] != Inf ; if (dd == Inf) dd <- 0
#   write_gams.R:513   dtt[dtt$value != 0 & dtt$value != Inf, ]
#
# Carrying the rows through instead put 26,280 -Inf coefficients into the
# IB_2050 matrix (eqStorageInpUp x vStorageOutCap, from pStorageInp2outUp) and
# HiGHS refused the model outright.
# =============================================================================

test_that("an infinite parameter value falls back to the default", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  # a parameter that carries data and is used as a coefficient
  ref <- model_to_lp(m)
  pname <- names(Filter(function(p) {
    !is.null(p$data) && is.data.frame(p$data) && nrow(p$data) > 1 &&
      "value" %in% names(p$data) &&
      !is.null(p$defVal) && is.numeric(p$defVal) && is.finite(p$defVal[1])
  }, m$parameters))[1]
  skip_if(is.na(pname) || is.null(pname))

  poisoned <- m
  poisoned$parameters[[pname]]$data$value[1] <- Inf
  lp <- model_to_lp(poisoned)

  expect_true(all(is.finite(lp$A@x)))
  expect_equal(check_matrix_numbers(lp, verbose = FALSE)$summary$nonfinite[1], 0L)
  # the matrix keeps its shape - a dropped row changes a value, not the sparsity
  expect_equal(dim(lp$A), dim(ref$A))
})

test_that("an infinite default contributes 0, as the writers do", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  pname <- names(Filter(function(p) {
    !is.null(p$data) && is.data.frame(p$data) && nrow(p$data) > 0 &&
      "value" %in% names(p$data)
  }, m$parameters))[1]
  skip_if(is.na(pname) || is.null(pname))

  # strip the table entirely and declare an infinite default: every lookup now
  # takes the default, which must be 0 rather than Inf
  poisoned <- m
  poisoned$parameters[[pname]]$data <- poisoned$parameters[[pname]]$data[0, ]
  poisoned$parameters[[pname]]$defVal <- Inf
  poisoned$parameters[[pname]]$misc <- list(inMemory = TRUE)

  lp <- model_to_lp(poisoned)
  expect_true(all(is.finite(lp$A@x)))
})
