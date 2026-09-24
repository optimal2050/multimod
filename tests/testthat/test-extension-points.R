# =============================================================================
# The seams another package builds on
# =============================================================================
#
# energyRt is to own its own integration code and call multimod, rather than
# multimod owning energyRt-shaped code. These are the surfaces that move
# becomes possible through. They are exported and load-bearing for a package
# multimod cannot see, so each one is pinned here: an accidental
# un-export or signature change would only surface in the other repository.
# =============================================================================

test_that("the GAMS parsing primitives a consumer needs are exported", {
  # energyRt's newConstraint() IR arrives as GAMS text whose headers look like
  # eqCnsX(region, year)$mCnsForEachX(region, year).. - a consumer has to parse
  # exactly what read_gams() parses, or the two disagree
  for (fn in c("parse_equation_header", "remove_ontext_offtext")) {
    expect_true(fn %in% getNamespaceExports("multimod"), info = fn)
  }
})

test_that("remove_ontext_offtext strips GAMS block comments", {
  got <- remove_ontext_offtext(c("a;", "$ontext", "note", "$offtext", "b;"))
  expect_false(any(grepl("note", got)))
  expect_true(any(grepl("a;", got)))
  expect_true(any(grepl("b;", got)))
})

test_that("parse_equation_header splits a header and declines a non-header", {
  h <- parse_equation_header("eqCost(region, year).. ")
  expect_false(is.null(h))
  expect_null(parse_equation_header("vTechAct(tech) =l= 5;"))
})

# ---------------------------------------------------------------------------
# fold: one implementation, not two
# ---------------------------------------------------------------------------

test_that("create_fold_spec and its energyRt alias agree", {
  # Until 2026-09-24 `create_fold_spec_energyRt` was defined TWICE in fold.R.
  # The first definition delegated to create_fold_spec(), which delegated
  # straight back - infinite mutual recursion, avoided only because the second
  # definition silently overrode the first. If that ever returns, this test
  # stops rather than hangs, because the alias must produce a real answer.
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  a <- create_fold_spec(m, fold_dims = "timeslice", verbose = FALSE)
  b <- create_fold_spec_energyRt(m, fold_dims = "timeslice", verbose = FALSE)

  expect_s3_class(a, "data.frame")
  expect_gt(nrow(a), 0)
  expect_identical(a, b)
})

test_that("create_fold_spec does not call anything energyRt-named", {
  # the seam itself: core folding must not reach for a consumer-specific name
  src <- deparse(body(create_fold_spec))
  expect_false(any(grepl("energyRt", src)))
})

# ---------------------------------------------------------------------------
# solution output: multimod's own layout, and the energyRt one built on it
# ---------------------------------------------------------------------------

test_that("write_solution writes one table per variable with real set names", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  r <- solve_highs(example_models$energyRt$multimod, attach = FALSE)

  d <- withr::local_tempdir()
  paths <- write_solution(r, d, format = "csv")

  expect_gt(length(paths), 0)
  expect_true(all(file.exists(paths)))
  # the point of the layout: named index columns, not i1..iN
  one <- utils::read.csv(grep("vTechAct", paths, value = TRUE)[1])
  expect_true("value" %in% names(one))
  expect_false(any(grepl("^i[0-9]+$", names(one))))
})

test_that("write_energyrt_output adds its scaffolding on top of write_solution", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  r <- solve_highs(example_models$energyRt$multimod, attach = FALSE)

  d1 <- withr::local_tempdir()
  d2 <- withr::local_tempdir()
  plain <- write_solution(r, d1, format = "csv")
  ert   <- write_energyrt_output(r, d2, format = "csv")

  # energyRt's layout is a superset: the same per-variable tables plus the
  # three files energyRt::read_solution() additionally requires
  expect_gt(length(ert), length(plain))
  expect_true(file.exists(file.path(d2, "variable_list.csv")))
  expect_false(file.exists(file.path(d1, "variable_list.csv")))
})

test_that("the session's write_output takes a writer and defaults to multimod's", {
  skip_if_not_installed("R6")
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")

  s <- mm_session(example_models$energyRt$multimod)$build_lp()$solve()
  d <- withr::local_tempdir()
  s$write_output(d, format = "csv")
  # default is multimod's own layout - no energyRt scaffolding
  expect_false(file.exists(file.path(d, "variable_list.csv")))

  d2 <- withr::local_tempdir()
  s$write_output(d2, writer = write_energyrt_output, format = "csv")
  expect_true(file.exists(file.path(d2, "variable_list.csv")))
})

test_that("the session does not name energyRt anywhere in write_output", {
  skip_if_not_installed("R6")
  s <- mm_session()
  expect_false(grepl("energyrt|energyRt",
                     paste(deparse(s$write_output), collapse = " ")))
})

# ---------------------------------------------------------------------------
# fold_info$mode: the encoding has to be self-describing
# ---------------------------------------------------------------------------

test_that("fold_model records mode = 'drop' in fold_info", {
  # Both packages compute the same whole-column fold but serialise it
  # differently: energyRt keeps the column with NA ("wildcard"), multimod
  # removes it ("drop"). Without the field a reader cannot tell which it has,
  # and a wildcard must be expanded before it reaches a matrix.
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  spec <- create_fold_spec(m, fold_dims = "timeslice", verbose = FALSE)
  skip_if(sum(spec$can_fold) == 0, "nothing foldable in the fixture")

  folded <- fold_model(m, fold_spec = spec, verbose = FALSE)
  infos <- Filter(Negate(is.null),
                  lapply(folded$parameters, function(p) p$misc$fold_info))
  infos <- Filter(function(fi) isTRUE(fi$folded), infos)
  skip_if(length(infos) == 0, "no parameter was folded")

  expect_true(all(vapply(infos, function(fi) identical(fi$mode, "drop"),
                         logical(1))))
  # and the key naming the folded dims, which energyRt calls wildcard_dims
  expect_true(all(vapply(infos, function(fi) !is.null(fi$removed_dims),
                         logical(1))))
})

# The companion test - that .scenario_is_folded() only fires for a WILDCARD
# fold, since a "drop" fold has no column left to expand - moved to energyRt
# on 2026-09-24 with .scenario_is_folded() itself. See
# energyRt/tests/testthat/test-multimod-import.R.
