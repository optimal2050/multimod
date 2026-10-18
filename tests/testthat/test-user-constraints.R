# =============================================================================
# Matrix assembly around gating maps, wildcards and tuple indices
# =============================================================================
#
# What remains here after 2026-09-24: the parts that exercise multimod's own
# matrix path - gating-map emptiness, undeclared maps, wildcard refusal, tuple
# index parsing and numeric literals.
#
# The user-constraint IR tests (`.split_gams_statement`, `.parse_cns_equation`,
# `.has_wildcard`, and the `fixture_with_constraint()` helper that drives them)
# moved to energyRt with the code they test - see
# energyRt/tests/testthat/test-multimod-constraints.R.
# =============================================================================

# ---------------------------------------------------------------------------

mx_ctx <- function(model) {
  ctx <- new.env(parent = emptyenv())
  ctx$model <- model
  ctx$map_cache <- list()
  ctx
}

test_that("an empty gating map ends the sum instead of expanding it densely", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  # declared, but holding no tuples - and, as an unlinked map does, carrying no
  # columns either. Arity cannot be checked here, which is exactly why the
  # emptiness test has to come first.
  m$mappings$mGateEmpty <- new_mapping(
    "mGateEmpty", desc = "empty gate", dims = c("tech", "region", "year"),
    active_dims = c("tech", "region", "year"), data = data.frame())

  cond <- ast_mapping("mGateEmpty",
                      ast_dims(c("tech", "region", "year")))
  cur <- data.table::data.table(region = "R1", year = "2030", .outer = 1L)

  res <- multimod:::.mx_join_cond(cur, cond, iters = "tech", ctx = mx_ctx(m))

  # NULL would mean "no map constrains this index", and the caller would then
  # expand the iterator over every member of `tech`.
  expect_false(is.null(res))
  expect_equal(nrow(res), 0L)
})

test_that("a map the model never declared is an error, not an open index", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  expect_error(multimod:::.mx_map_data("mNoSuchMap", mx_ctx(m)),
               "not declared")
})

test_that("a populated gating map still binds the summation index", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod
  cond <- ast_mapping("mTechSpan", ast_dims(c("tech", "region", "year")))
  span <- as.data.frame(get_data(m, "mTechSpan", type = "mapping"))
  reg <- as.character(span$region[1])
  yr <- as.character(span$year[1])
  cur <- data.table::data.table(region = reg, year = yr, .outer = 1L)

  res <- multimod:::.mx_join_cond(cur, cond, iters = "tech", ctx = mx_ctx(m))
  expect_true("tech" %in% names(res))
  expect_equal(
    sort(res$tech),
    sort(as.character(span$tech[as.character(span$region) == reg &
                                  as.character(span$year) == yr])))
})

test_that("a wildcard (NA) index is looked up on its explicit columns", {
  # energyRt writes NA for "every member" when interpolating with fold = TRUE.
  # Joining on the NA key would match nothing and silently take the default;
  # the lookup instead matches wildcard rows on their explicit columns, most
  # explicit first.
  lk <- data.table::data.table(
    region = c("R1", NA, NA),
    year   = c("2030", "2030", NA),
    value  = c(5, 7, 9))
  env <- data.table::data.table(
    .k = 1:4,
    region = c("R1", "R2", "R1", "R2"),
    year   = c("2030", "2030", "2040", "2040"))
  ctx <- new.env(parent = emptyenv())
  tm <- multimod:::.mx_param_wildcard(lk, c("region", "year"), env, defv = 0,
                                      nm = "pX", ctx = ctx)
  # R1/2030 explicit; R2/2030 from the year-only row; 2040 from the all-NA row
  expect_equal(tm$coef, c(5, 7, 9, 9))

  # without the all-wildcard row the unmatched tuples take the default
  tm2 <- multimod:::.mx_param_wildcard(lk[1:2], c("region", "year"), env,
                                       defv = -1, nm = "pX", ctx = ctx)
  expect_equal(tm2$coef, c(5, 7, -1, -1))

  # two different values for the same wildcard key are ambiguous
  amb <- data.table::data.table(region = c(NA, NA), year = c("2030", "2030"),
                                value = c(1, 2))
  expect_error(multimod:::.mx_param_wildcard(amb, c("region", "year"), env, 0,
                                             "pX", ctx), "ambiguous")
})

test_that("a tuple index list parses without the separator's whitespace", {
  n <- multimod:::parse_gams_expr("region, timeslice")
  expect_s3_class(n, "dims")
  expect_identical(
    vapply(n, function(x) x$name, character(1), USE.NAMES = FALSE),
    c("region", "timeslice"))

  # the spacing energyRt actually emits, and a wider tuple
  n3 <- multimod:::parse_gams_expr("tech,  region ,year")
  expect_identical(
    vapply(n3, function(x) x$name, character(1), USE.NAMES = FALSE),
    c("tech", "region", "year"))
})

test_that("numeric literals in scientific notation parse whole", {
  # the exponent's sign is not a top-level operator; splitting there left "1e"
  for (lit in c("1e-20", "1E-20", "1.0e-20", "1e+20", "3.6888e+08", "2.5e-3")) {
    n <- multimod:::parse_gams_expr(lit)
    expect_s3_class(n, "constant")
    expect_equal(n$value, as.numeric(lit), tolerance = 0)
  }
  # a zero-carbon cap is written exactly this way
  n <- multimod:::parse_gams_expr("1e-20")
  expect_equal(n$value, 1e-20)
})
