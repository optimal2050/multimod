# =============================================================================
# LP index tables - build_col_index() / build_row_index()
# =============================================================================
#
# The counts below are gates, not guesses. They were established against an
# INDEPENDENT oracle: energyRt generates and solves its own GMPL via glpsol,
# never touching multimod.
#
#   energyRt 0.89.5.9001, UTOPIA R7 kit, utopia_seasons calendar, base horizon
#     columns : 18,298   (matches energyRt's GMPL exactly)
#     rows    : 18,163   (energyRt reports 18,164: GLPK adds one objective row,
#                         vObjective2, on top of the equation rows)
#
# If the fixture is rebuilt against a different energyRt version these numbers
# will move. Re-derive them with data-raw/build_energyRt_fixture.R rather than
# adjusting them by hand - see check_against_energyRt() in that file.
#
# Note: do NOT use energyRt::model_size() as the row gate. It counts constraints
# per *map*, and 9 maps gate more than one equation, so it undercounts.
# =============================================================================

EXPECTED_COLS <- 18298L
EXPECTED_ROWS <- 18163L

test_that("build_col_index reproduces the LP column count", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  ci <- build_col_index(m)

  expect_s3_class(ci, "data.table")
  expect_equal(nrow(ci), EXPECTED_COLS)
  expect_equal(ci$j, seq_len(nrow(ci)))
  expect_false(anyDuplicated(ci$name) > 0)
})

test_that("build_row_index reproduces the LP row count", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  ri <- build_row_index(m)

  expect_s3_class(ri, "data.table")
  expect_equal(nrow(ri), EXPECTED_ROWS)
  expect_equal(ri$i, seq_len(nrow(ri)))
  expect_false(anyDuplicated(ri$name) > 0)
})

test_that("every constraint has a valid sense", {
  data(example_models, package = "multimod")
  ri <- build_row_index(example_models$energyRt$multimod)

  expect_true(all(ri$sense %in% c("==", "<=", ">=")))
  expect_false(any(is.na(ri$sense)))
  # rhs is a placeholder until coefficient extraction fills it
  expect_true(all(is.na(ri$rhs)))
})

test_that("column bounds are finite-or-infinite numerics consistent with vtype", {
  data(example_models, package = "multimod")
  ci <- build_col_index(example_models$energyRt$multimod)

  expect_type(ci$lo, "double")
  expect_type(ci$up, "double")
  expect_false(any(is.na(ci$lo)))
  expect_false(any(is.na(ci$up)))
  expect_true(all(ci$lo <= ci$up))
  # energyRt is an LP: every variable is continuous, and positive vars start at 0
  expect_true(all(ci$vtype %in% c("continuous", "positive")))
  expect_true(all(ci$lo[ci$vtype == "positive"] == 0))
})

test_that("ungated scalars contribute exactly one column and one row", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  ci <- build_col_index(m)
  ri <- build_row_index(m)

  # vObjective / eqObjective have no domain mapping
  expect_equal(sum(ci$symbol == "vObjective"), 1L)
  expect_equal(sum(ri$symbol == "eqObjective"), 1L)
  expect_true(is.na(ci$i1[ci$symbol == "vObjective"]))
  expect_true(is.na(ri$i1[ri$symbol == "eqObjective"]))
})

test_that("symbols gated by an empty mapping contribute nothing", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  ci <- build_col_index(m)

  # An empty domain map means the symbol does not exist in this scenario at
  # all. That is different from having no domain (a scalar), which contributes
  # one column - conflating the two silently inflates the LP.
  empty <- vapply(names(m$variables), function(nm) {
    d <- m$variables[[nm]]$domain
    dn <- if (is.character(d)) d[1] else if (!is.null(d$name)) d$name else NA_character_
    if (is.na(dn) || is.null(m$mappings[[dn]])) return(FALSE)
    dd <- m$mappings[[dn]]$data
    is.null(dd) || nrow(as.data.frame(dd)) == 0L
  }, logical(1))

  expect_gt(sum(empty), 0)                       # the fixture does have some
  expect_false(any(names(which(empty)) %in% ci$symbol))
})

test_that("dim_names records the original set names per symbol", {
  data(example_models, package = "multimod")
  ci <- build_col_index(example_models$energyRt$multimod)

  dn <- attr(ci, "dim_names")
  expect_type(dn, "list")
  # must be the model's set names, not the internal i1..iN placeholders
  expect_equal(dn$vTechAct, c("tech", "region", "year", "timeslice"))
  expect_false(any(grepl("^i[0-9]+$", unlist(dn))))
})
