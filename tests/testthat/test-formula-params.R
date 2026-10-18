# Parameters defined by a formula rather than data. The text backends hand the
# formula to the target language; the matrix backend has to compute it, and a
# parameter left uncomputed would quietly fall back to its scalar default.

fml_model <- c(
  "set L;",
  "param base{l in L};",
  "param derived{l in L} := base[l] * 2;",
  "var x{l in L} >= 0;",
  "maximize obj: sum{l in L} derived[l] * x[l];",
  "s.t. budget: sum{l in L} x[l] <= 1;",
  "data;",
  "set L := a b c;",
  "param base := a 1 b 2 c 5;",
  "end;"
)

test_that("the matrix backend computes a formula parameter itself", {
  skip_if_not_installed("highs")
  m <- read_gmpl(fml_model)
  expect_equal(NROW(m$parameters$derived$data), 0L)   # formula only, no data

  # spend the budget on the largest derived value: 2 * 5 = 10
  expect_equal(solve_highs(m, attach = FALSE)$objective, 10, tolerance = 1e-9)

  # and the caller's model is left as it was
  expect_equal(NROW(m$parameters$derived$data), 0L)
})

test_that("a formula over an empty index set is not refused", {
  # evaluate_parameters() clears a formula once computed, and over an empty set
  # it computes to no rows - which is correct, not a failure to evaluate
  m <- read_gmpl(c(
    "set L;",
    "param base{l in L};",
    "param derived{l in L} := base[l] * 2;",
    "var x{l in L} >= 0;",
    "s.t. budget: sum{l in L} x[l] <= 1;",
    "data;",
    "set L := ;",
    "end;"))
  expect_equal(length(m$sets$L$data), 0L)
  expect_silent(multimod:::.mx_materialise_formulas(m))
})

test_that("computing a formula clears it, which is what the backstop reads", {
  m <- read_gmpl(fml_model)
  expect_true(!is.null(m$parameters$derived$formula))
  e <- evaluate_parameters(m)
  expect_null(e$parameters$derived$formula)
  expect_equal(NROW(e$parameters$derived$data), 3L)
  expect_equal(sort(e$parameters$derived$data$value), c(2, 4, 10))
})

test_that("GAMS writes a computed parameter as an assignment", {
  m <- read_gmpl(fml_model)
  f <- m$parameters$derived$formula
  skip_if(is.null(f), "reader kept no formula")
  out <- as_gams(if (inherits(f, "ast_formula")) f else
    new_ast_formula(target = "derived", field = "value", expr = f,
                    index_vars = get_dim_names(m$parameters$derived$dims)))
  # GAMS has no inline `:=`; a computed parameter is declared then assigned
  expect_match(out, "^derived\\(")
  expect_match(out, "=")
  expect_match(out, ";$")
})

test_that("the GAMS writer emits a computed-parameters section", {
  m <- read_gmpl(fml_model)
  src <- write_gams(m)
  expect_true(any(grepl("computed parameters", src, fixed = TRUE)))
  expect_true(any(grepl("^derived\\(", src)))
  # the assignment comes after the declarations, so the names already exist
  expect_gt(grep("computed parameters", src, fixed = TRUE)[1],
            grep("^parameters$", src)[1])
})
