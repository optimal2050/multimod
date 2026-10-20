# =============================================================================
# Subset declarations across the script writers
# =============================================================================
#
# energyRt declares its process subsets as `sup(process)`, `tech(process)`, ...
# A single-dimension declaration over a known set is written exactly like a
# 1-dim mapping (`mUpComm(comm)`), so read_gams registers the name on both
# sides and resolves each occurrence by usage. Both registrations name ONE
# symbol, so each writer must declare it once, in the form that backend uses -
# the subset form for GAMS, a flat set for MathProg and Pyomo.
#
# Symptoms when this regresses: GAMS "symbol declared twice"; a MathProg data
# section with two `set sup :=` entries ("sup already defined"); Pyomo
# "Implicitly replacing the Component attribute sup", keeping an empty set.
# =============================================================================

skip_unless_fixture <- function() {
  skip_on_cran()
  skip_if_not_installed("arrow")
}

fixture_model <- function() {
  data(example_models, package = "multimod")
  example_models$energyRt$multimod
}

test_that("dual_registered_symbols() finds subset declarations only", {
  skip_unless_fixture()
  model <- fixture_model()
  dual <- dual_registered_symbols(model)

  expect_gt(length(dual), 0)
  expect_true(all(dual %in% names(model$sets)))
  expect_true(all(dual %in% names(model$mappings)))

  # Every hit is one dimension over a set the model declares.
  for (nm in dual) {
    dims <- model$mappings[[nm]]$dims
    expect_length(dims, 1L)
    expect_true((dims[[1]]$name %||% dims[[1]]) %in% names(model$sets), info = nm)
  }

  # A genuine multi-dimension mapping is not a subset declaration.
  expect_false("mSupComm" %in% dual)
})

test_that("GAMS declares each subset once, keeping its parent", {
  skip_unless_fixture()
  model <- fixture_model()
  dual <- dual_registered_symbols(model)

  f <- tempfile(fileext = ".gms")
  on.exit(unlink(f), add = TRUE)
  write_gams(model, file = f)
  src <- readLines(f, warn = FALSE)

  for (nm in dual) {
    decl <- grep(sprintf("^[[:space:]]*%s([^[:alnum:]_]|$)", nm), src, value = TRUE)
    expect_length(decl, 1L)
    # GAMS keeps the subset form, so the surviving declaration is the indexed one.
    expect_match(decl, sprintf("^[[:space:]]*%s[[(]", nm), info = nm)
  }
})

test_that("MathProg declares and populates each subset once", {
  skip_unless_fixture()
  model <- fixture_model()
  dual <- dual_registered_symbols(model)

  model_dir <- tempfile("gmpl_subsets_")
  dir.create(model_dir, recursive = TRUE)
  on.exit(unlink(model_dir, recursive = TRUE), add = TRUE)

  save_model(model, model_dir, format = "csv", verbose = FALSE)
  expect_no_error(write_gmpl(model, model_dir = model_dir))

  mod <- readLines(file.path(model_dir, "solvers", "gmpl", "model.mod"), warn = FALSE)
  dat <- readLines(file.path(model_dir, "solvers", "gmpl", "data.dat"), warn = FALSE)

  for (nm in dual) {
    decl <- grep(sprintf("^[[:space:]]*set[[:space:]]+%s([^[:alnum:]_]|$)", nm), mod)
    expect_length(decl, 1L)
    assign <- grep(sprintf("^[[:space:]]*set[[:space:]]+%s[[:space:]]*:=", nm), dat)
    expect_lte(length(assign), 1L)
  }
})

test_that("Pyomo declares each subset as one component", {
  skip_unless_fixture()
  model <- fixture_model()
  dual <- dual_registered_symbols(model)

  model_dir <- tempfile("pyomo_subsets_")
  dir.create(model_dir, recursive = TRUE)
  on.exit(unlink(model_dir, recursive = TRUE), add = TRUE)

  save_model(model, model_dir, format = "csv", verbose = FALSE)
  write_pyomo(model, model_dir = model_dir)
  src <- readLines(file.path(model_dir, "solvers", "pyomo", "model.py"), warn = FALSE)

  for (nm in dual) {
    expect_length(grep(sprintf("^[[:space:]]*model[.]%s[[:space:]]*=", nm), src), 1L)
  }
})

test_that("iterator variables stay unique once subsets become sets", {
  skip_unless_fixture()
  model <- fixture_model()
  ia <- unlist(model$index_aliases)
  expect_false(any(duplicated(ia)))

  # A partial regeneration must not reissue an iterator already in use.
  regen <- grep("^m[A-Z]", dual_registered_symbols(model), value = TRUE)
  skip_if(length(regen) == 0)
  m2 <- model
  m2$index_aliases <- ia[setdiff(names(ia), regen)]
  m2 <- suppressWarnings(add_index_aliases(m2, index_aliases = m2$index_aliases,
                                           overwrite = TRUE))
  expect_false(any(duplicated(unlist(m2$index_aliases))))
})
