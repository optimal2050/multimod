# =============================================================================
# energyRt user constraints, and the gating-map ordering they depend on
# =============================================================================
#
# `newConstraint()` objects reach a backend only as a compiled GAMS string on
# `modInp@user_constraints`, together with the mCns*/pCns* parameters that
# interpolation materialised. multimod parses that string with the GAMS parser
# it already has. The string below is the shape energyRt actually emits, taken
# verbatim from IB_2050-PYPSA_IB_RT-d365_h24 with the names shortened.
#
# Building an S4 energyRt scenario here would pull in the whole package, so the
# tests exercise the parse and evaluation path directly and let the real
# scenario be the integration check.
# =============================================================================

IR <- paste0(
  "eqCnsTEST(region, year)$mCnsForEachTEST(region, year)..   ",
  "sum(tech$(mCnsTEST_1(tech) and mTechSpan(tech, region, year)), ",
  "1 * pCnsMultTEST_1(tech) * vTechCap(tech, region, year)) ",
  "=l= pCnsRhsTEST(region, year);"
)

# The R7 fixture, plus the symbols a user constraint brings with it.
fixture_with_constraint <- function(mult = c(EBIO = 2, ECOA = 3),
                                    rhs_value = 100) {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  span <- as.data.frame(get_data(m, "mTechSpan", type = "mapping"))
  for_each <- unique(span[, c("region", "year")])
  techs <- names(mult)

  m$mappings$mCnsForEachTEST <- new_mapping(
    "mCnsForEachTEST", desc = "row set", dims = c("region", "year"),
    active_dims = c("region", "year"), data = for_each)
  m$mappings$mCnsTEST_1 <- new_mapping(
    "mCnsTEST_1", desc = "for.sum restriction", dims = "tech",
    active_dims = "tech", data = data.frame(tech = techs))
  m$parameters$pCnsMultTEST_1 <- new_parameter(
    "pCnsMultTEST_1", desc = "coefficients", dims = "tech",
    active_dims = "tech",
    data = data.frame(tech = techs, value = unname(mult)), defVal = 1)
  m$parameters$pCnsRhsTEST <- new_parameter(
    "pCnsRhsTEST", desc = "rhs", dims = c("region", "year"),
    active_dims = c("region", "year"),
    data = cbind(for_each, value = rhs_value), defVal = Inf)

  eq <- multimod:::.parse_cns_equation(IR, build_symbols_list(m), desc = "test")
  m$equations[[eq$name]] <- eq
  list(model = m, for_each = for_each, span = span, mult = mult,
       rhs_value = rhs_value)
}

test_that("a GAMS statement splits at the first '..'", {
  s <- multimod:::.split_gams_statement("eqX(r)$mY(r)..  a =l= b;")
  expect_equal(s$header, "eqX(r)$mY(r)")
  expect_equal(s$body, "a =l= b")

  # a decimal point must not be mistaken for the separator
  s2 <- multimod:::.split_gams_statement("eqX(r).. 1.5 * a =e= 0.25;")
  expect_equal(s2$header, "eqX(r)")
  expect_equal(s2$body, "1.5 * a =e= 0.25")

  expect_error(multimod:::.split_gams_statement("eqX(r) a =l= b;"), "no '\\.\\.'")
})

test_that("a user-constraint IR string parses into an equation", {
  data(example_models, package = "multimod")
  m <- fixture_with_constraint()$model
  eq <- m$equations$eqCnsTEST

  expect_s3_class(eq, "equation")
  expect_equal(eq$name, "eqCnsTEST")
  expect_equal(eq$relation, "<=")
  # the $-condition of the header becomes the equation's domain mapping, which
  # is what build_row_index() reads to size the row block
  expect_equal(eq$domain$name, "mCnsForEachTEST")
  expect_equal(vapply(eq$dims, dim_binding_name, character(1)),
               c("region", "year"))
  expect_equal(eq$rhs$name, "pCnsRhsTEST")
})

test_that("the constraint contributes exactly its domain's rows", {
  data(example_models, package = "multimod")
  f <- fixture_with_constraint()
  base <- build_row_index(example_models$energyRt$multimod)
  ri <- build_row_index(f$model)

  expect_equal(nrow(ri) - nrow(base), nrow(f$for_each))
  expect_equal(sum(ri$symbol == "eqCnsTEST"), nrow(f$for_each))
  expect_true(all(ri$sense[ri$symbol == "eqCnsTEST"] == "<="))
})

test_that("coefficients and rhs match the constraint's own tables", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  f <- fixture_with_constraint()
  lp <- model_to_lp(f$model)

  ri <- lp$row_index
  ci <- lp$col_index
  i <- ri$i[ri$symbol == "eqCnsTEST"][1]
  reg <- ri$i1[ri$i == i]
  yr <- ri$i2[ri$i == i]

  # which technologies should appear: in the for.sum restriction AND in the
  # variable's own gating map for this (region, year)
  span <- f$span
  want <- intersect(
    names(f$mult),
    span$tech[as.character(span$region) == reg & as.character(span$year) == yr])
  expect_gt(length(want), 0)

  got <- Matrix::which(lp$A[i, ] != 0)
  expect_equal(sort(ci$i1[match(got, ci$j)]), sort(want))
  expect_true(all(ci$symbol[match(got, ci$j)] == "vTechCap"))
  expect_equal(unname(lp$A[i, got]), unname(f$mult[ci$i1[match(got, ci$j)]]))

  # `=l=` gives an upper bound only, at the rhs parameter's value
  expect_equal(lp$row_up[i], f$rhs_value)
  expect_equal(lp$row_lo[i], -Inf)
})

# ---------------------------------------------------------------------------
# The gating-map ordering these constraints depend on
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


# =============================================================================
# Wildcard (folded) parameter data
# =============================================================================
#
# `interp_mod(fold = TRUE)` writes NA into an index column to mean "every
# member of this dimension". multimod joins on index columns, so an NA matches
# nothing and the tuple silently takes the parameter's default. Measured on the
# UTOPIA R7 kit before the fix: same LP shape, 42,010 nonzeros instead of
# 43,612, and an objective of 0 instead of 46,398.42 - with no error.
#
# Wildcards are expanded at import (R/energyrt_unfold.R). These tests cover the
# backstop that makes a missed one impossible to solve past; the end-to-end
# folded-vs-plain comparison needs an energyRt interpolation and lives in
# the fold probe script.
# =============================================================================

test_that(".has_wildcard looks at index columns only", {
  expect_false(multimod:::.has_wildcard(
    data.frame(tech = "A", region = "R1", value = 1)))
  expect_true(multimod:::.has_wildcard(
    data.frame(tech = "A", region = NA_character_, value = 1)))
  # a missing VALUE is not a wildcard - only an index can stand for "all"
  expect_false(multimod:::.has_wildcard(
    data.frame(tech = "A", region = "R1", value = NA_real_)))
  expect_false(multimod:::.has_wildcard(data.frame()))
  expect_false(multimod:::.has_wildcard(NULL))
})

test_that("a wildcard reaching the matrix is an error, not a default", {
  skip_if_not_installed("highs")
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  # the parameter has to be one an equation actually references, or the
  # evaluator never looks at it
  used <- character()
  walk <- function(n) {
    if (is.null(n) || !is.list(n)) return(invisible())
    if (inherits(n, "parameter") && !is.null(n$name)) used <<- c(used, n$name)
    for (el in n) if (is.list(el)) walk(el)
  }
  for (eq in m$equations) walk(eq)

  cand <- Filter(function(nm) {
    p <- m$parameters[[nm]]
    !is.null(p) && !is.null(p$data) && is.data.frame(p$data) &&
      nrow(p$data) > 1 && "value" %in% names(p$data) &&
      length(setdiff(names(p$data), "value")) > 0
  }, unique(used))
  skip_if(length(cand) == 0)
  pname <- cand[1]

  idx <- setdiff(names(m$parameters[[pname]]$data), "value")[1]
  m$parameters[[pname]]$data[[idx]][1] <- NA

  # silently substituting defVal here is exactly how the folded model solved
  # to a wrong answer, so this must refuse rather than proceed
  expect_error(model_to_lp(m), "wildcard")
  expect_error(model_to_lp(m), pname, fixed = TRUE)
})

# =============================================================================
# Tuple index lists: sum((a, b)$map, ...)
# =============================================================================
#
# energyRt emits this shape whenever a term sums over MORE THAN ONE free index
# -- a constraint whose term names no `for.sum` and whose variable carries dims
# the equation's `for.each` does not (class-constraint.R:1163). Every test
# above uses the single-iterator form `sum(tech$..., ...)`, which has no comma.
#
# The comma is the whole story: the operand split left the separator's
# whitespace attached, so the second iterator parsed as " timeslice", matched
# no set, and the sum bound nothing. The row was still emitted, with its
# correct RHS and no coefficients -- `0 <= cap`, which every solver reports as
# Optimal. That is how EU41_N10_TRANS solved to 2.602e12 while ignoring its
# own CO2 path (2026-09-14).

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

test_that("a sum over a tuple of free indices carries coefficients", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  # sum over (tech, region) -- two free indices -- gated by mTechSpan, with the
  # equation indexed by year alone. No for.sum, no pCnsMult, literal RHS: the
  # bare shape energyRt emits for `term1 = list(variable = "vTechCap")`.
  span <- as.data.frame(get_data(m, "mTechSpan", type = "mapping"))
  yrs <- unique(span[, "year", drop = FALSE])
  m$mappings$mCnsForEachTUP <- new_mapping(
    "mCnsForEachTUP", desc = "row set", dims = "year",
    active_dims = "year", data = yrs)

  ir <- paste0(
    "eqCnsTUP(year)$mCnsForEachTUP(year)..   ",
    "sum((tech, region)$mTechSpan(tech, region, year), ",
    "vTechCap(tech, region, year)) =l= 100;")
  eq <- multimod:::.parse_cns_equation(ir, build_symbols_list(m), desc = "tuple")
  m$equations[[eq$name]] <- eq

  lp <- model_to_lp(m)
  rows <- which(lp$row_index$symbol == "eqCnsTUP")
  expect_gt(length(rows), 0)

  # the defect: rows present, RHS right, matrix empty
  nz <- Matrix::rowSums(abs(lp$A[rows, , drop = FALSE]) > 0)
  expect_true(all(nz > 0))
  expect_true(all(lp$row_up[rows] == 100))

  # and the package's own detector agrees
  expect_false("eqCnsTUP" %in%
                 check_matrix_numbers(lp, verbose = FALSE)$empty_row_symbols)
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

# =============================================================================
# The guardrail: an empty constraint row must not pass silently
# =============================================================================

test_that("model_to_lp refuses a user constraint that binds nothing", {
  data(example_models, package = "multimod")
  m <- example_models$energyRt$multimod

  span <- as.data.frame(get_data(m, "mTechSpan", type = "mapping"))
  yrs <- unique(span[, "year", drop = FALSE])
  m$mappings$mCnsForEachVOID <- new_mapping(
    "mCnsForEachVOID", desc = "row set", dims = "year",
    active_dims = "year", data = yrs)
  # a gate map with no rows: the sum binds nothing, the row is still declared
  m$mappings$mVoidGate <- new_mapping(
    "mVoidGate", desc = "empty gate", dims = c("tech", "region", "year"),
    active_dims = c("tech", "region", "year"),
    data = span[0, c("tech", "region", "year")])

  ir <- paste0(
    "eqCnsVOID(year)$mCnsForEachVOID(year)..   ",
    "sum((tech, region)$mVoidGate(tech, region, year), ",
    "vTechCap(tech, region, year)) =l= 100;")
  eq <- multimod:::.parse_cns_equation(ir, build_symbols_list(m), desc = "void")
  m$equations[[eq$name]] <- eq

  expect_error(model_to_lp(m), "eqCnsVOID")
  expect_error(model_to_lp(m), "constrain nothing")

  # the escape hatches still assemble, so a caller can inspect the matrix
  expect_warning(lp <- model_to_lp(m, on_empty_row = "warn"), "eqCnsVOID")
  expect_silent(lp2 <- model_to_lp(m, on_empty_row = "ignore"))
  expect_true("eqCnsVOID" %in%
                check_matrix_numbers(lp2, verbose = FALSE)$empty_row_symbols)
})
