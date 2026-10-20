# =============================================================================
# Pyomo: conditions and rule iterators
# =============================================================================
#
# Three defects here produced a model that either crashed indexing a variable
# outside its domain, or constructed and solved to a WRONG answer:
#
#   * a gated sum lost its condition unless the condition was a bare mapping,
#     so the sum ranged over the whole set;
#   * a mapping in condition position rendered as `model.mX`, which Pyomo
#     evaluates as a truthy Set, so the gate always passed;
#   * rule iterators were named from the equation's base sets while the body
#     resolved the domain mapping's dims, leaving a name unbound
#     (`eqImportTot` bound `r` and used `rd`).
#
# The oracle is MathProg, which writes the gate explicitly:
#   sum{yp in year: (h,r,yp) in mTechNew and ordYear[y] >= ordYear[yp]}
# =============================================================================

pyomo_fixture_source <- function() {
  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod
  model_dir <- tempfile("pyomo_cond_")
  dir.create(model_dir, recursive = TRUE)
  on.exit(unlink(model_dir, recursive = TRUE), add = TRUE)
  save_model(model, model_dir, format = "csv", verbose = FALSE)
  write_pyomo(model, model_dir = model_dir)
  readLines(file.path(model_dir, "solvers", "pyomo", "model.py"), warn = FALSE)
}

test_that("a gated sum keeps its condition", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  src <- pyomo_fixture_source()

  cap <- grep("vTechNewCap[h, r, yp]", src, value = TRUE, fixed = TRUE)
  expect_gt(length(cap), 0)
  # The sum over years must be filtered, not ranged over the whole set.
  expect_true(all(grepl("for yp in model.year if ", cap, fixed = TRUE)))
  expect_true(all(grepl("in model.mTechNew", cap, fixed = TRUE)))
})

test_that("a mapping in condition position is a membership test", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  src <- pyomo_fixture_source()

  # `if model.mX else` / `if model.mX and` treat a Set object as a boolean.
  truthy <- grep("if model[.]m[A-Za-z0-9_]+( else| and | or |[)])", src, value = TRUE)
  expect_length(truthy, 0L)

  # ord() goes through the position parameter, as in MathProg.
  expect_gt(length(grep("model.ordYear[", src, fixed = TRUE)), 0)
})

test_that("rule iterators match the names the body resolves", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  src <- pyomo_fixture_source()

  # eqImportTot is indexed by mImport(comm, dst, year, timeslice), so the
  # signature binds the dst iterator rather than the base region one.
  header <- grep("^def _eqImportTot_rule[(]", src, value = TRUE)
  expect_length(header, 1L)
  expect_match(header, "[(, ]rd[,)]")

  body_start <- grep("^def _eqImportTot_rule[(]", src)
  body <- src[seq(body_start + 1L, min(body_start + 8L, length(src)))]
  body <- body[!grepl("^def ", body)]
  expect_true(any(grepl("rd", body, fixed = TRUE)))
  # The old signature bound `r`, which the body never used.
  expect_false(grepl("[(, ]r[,)]", header))
})
