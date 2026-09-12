# =============================================================================
# GMPL rendering of GAMS ord() / card()
# =============================================================================
#
# GAMS `ord(s)` is the 1-based position of a member within its set. MathProg has
# no such function, so it must be routed through a position parameter that the
# model supplies (energyRt declares `ordYear(year)` "used in GLPK-MathProg" for
# exactly this reason).
#
# Regression: energyRt's own GAMS template is inconsistent - most equations use
# the ordYear parameter, but the storage capacity equations added later use the
# raw ord() function. Rendering those verbatim produced GMPL that glpsol
# rejected with "function ord unknown", which silently blocked the whole GMPL
# backend on the current model.
# =============================================================================

test_that("ord() renders as the model's position parameter, not a bare call", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  for (eq in c("eqStorageInpCap", "eqStorageStgCap")) {
    skip_if(is.null(m$equations[[eq]]), paste0(eq, " not in fixture"))
    out <- as_gmpl(m$equations[[eq]], model = m)

    # no bare ord( survives - MathProg would reject it
    expect_false(grepl("ord(", out, fixed = TRUE))
    # it became the position parameter, indexed
    expect_true(grepl("ordYear[", out, fixed = TRUE))
  }
})

test_that("ord() uses the iterator, not the raw set name", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  skip_if(is.null(m$equations$eqStorageInpCap))

  out <- as_gmpl(m$equations$eqStorageInpCap, model = m)

  # inside `sum{yp in year: ...}` the alias yearp must resolve to the iterator
  expect_false(grepl("ordYear[year]", out, fixed = TRUE))
  expect_false(grepl("ordYear[yearp]", out, fixed = TRUE))
  expect_true(grepl("ordYear[yp]", out, fixed = TRUE))
})

test_that("alias sets resolve to the base set's position parameter", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  skip_if(is.null(m$equations$eqStorageInpCap))

  out <- as_gmpl(m$equations$eqStorageInpCap, model = m)

  # yearp and yeare are aliases of year, so all three share ordYear -
  # ordYearp / ordYeare would not exist as parameters
  expect_false(grepl("ordYearp", out, fixed = TRUE))
  expect_false(grepl("ordYeare", out, fixed = TRUE))
})

test_that("a missing position parameter fails loudly, not silently", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  skip_if(is.null(m$equations$eqStorageInpCap))

  # drop the parameter the renderer depends on
  broken <- m
  broken$parameters$ordYear <- NULL

  # must error naming the parameter, rather than emitting invalid GMPL
  expect_error(
    as_gmpl(broken$equations$eqStorageInpCap, model = broken),
    "ordYear"
  )
})
