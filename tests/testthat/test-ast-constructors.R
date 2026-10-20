# =============================================================================
# Cross-language constructor names, and construction-time schema validation
# =============================================================================
#
# Two things are pinned here.
#
# 1. The `Set()` spellings are ALIASES. If one ever stops returning exactly
#    what the original constructor returns, user code that mixes the two
#    silently diverges - so the tests compare with identical(), not by shape.
#
# 2. A node that does not match `ast_schema()` is refused where it is BUILT.
#    Without that, a malformed node travels until some renderer declines to
#    dispatch on it and emits nothing at all - the trap named in CLAUDE.md.
# =============================================================================

# --- the alias layer ---------------------------------------------------------

test_that("cross-language names return exactly what the originals return", {
  expect_identical(Set("tech", desc = "t"), new_set("tech", desc = "t"))
  expect_identical(SetRef("tech"),          ast_set("tech"))
  expect_identical(Symbol("a"),             ast_symbol("a"))
  expect_identical(Constant(3),             ast_constant(3))
  expect_identical(Expression("*", ast_symbol("a"), ast_symbol("b")),
                   ast_expression("*", ast_symbol("a"), ast_symbol("b")))
  expect_identical(Dims(ast_set("a")),      ast_dims(ast_set("a")))
})

test_that("the bare name is the declaration and Ref is the reference", {
  expect_equal(schema_type(Set("tech")),    "set_decl")
  expect_equal(schema_type(SetRef("tech")), "set_ref")
  expect_true(is_declaration(Set("tech")))
  expect_false(is_declaration(SetRef("tech")))
  expect_false(is_declaration(Symbol("a")))
  expect_false(is_declaration(42))
})

test_that("Set() does not collide with data.table::set() or the class 'set'", {
  skip_if_not_installed("data.table")
  d <- data.table::data.table(a = 1:3)
  data.table::set(d, j = "b", value = 4:6)     # the generic `set`, untouched
  expect_equal(names(d), c("a", "b"))
  # and the S3 class is still lowercase, so dispatch is unaffected
  expect_equal(class(Set("tech"))[1], "mm_set")
  expect_true(inherits(SetRef("tech"), "mm_set"))
})

# --- construction-time validation --------------------------------------------

test_that("a node missing a required field is refused where it is built", {
  expect_error(new_ast("expression", op = "*", lhs = ast_symbol("a")),
               "required field 'rhs' is missing")
  expect_error(new_ast("when", condition = ast_symbol("a")),
               "required field 'then' is missing")
})

test_that("a node carrying an undeclared field is refused", {
  expect_error(new_ast("symbol", name = "a", bogus = 1),
               "undeclared field")
  expect_error(new_ast("symbol", name = "a", bogus = 1), "bogus")
})

test_that("a valid node is built without complaint", {
  expect_silent(new_ast("symbol", name = "a"))
  expect_silent(ast_expression("*", ast_symbol("a"), ast_symbol("b")))
  expect_silent(new_set("tech"))
  expect_s3_class(new_ast("symbol", name = "a"), "ast")
})

test_that("validation can be switched off for a hot loop", {
  withr::local_options(multimod.validate = FALSE)
  expect_silent(bad <- new_ast("expression", op = "*", lhs = ast_symbol("a")))
  expect_null(bad$rhs)
  # and the explicit checker still reports it
  expect_match(validate_ast(bad), "required field 'rhs'", all = FALSE)
})

test_that("the error names the schema so it is actionable", {
  err <- tryCatch(new_ast("symbol", name = "a", bogus = 1),
                  error = function(e) conditionMessage(e))
  expect_match(err, "ast_schema")
})

# --- the recursive checker ---------------------------------------------------

test_that("validate_ast recurses by default and can be told not to", {
  outer <- ast_expression("*", ast_symbol("a"), ast_symbol("b"))
  withr::local_options(multimod.validate = FALSE)
  outer$rhs <- new_ast("expression", op = "+", lhs = ast_symbol("c"))

  expect_match(validate_ast(outer), "required field 'rhs'", all = FALSE)
  expect_equal(validate_ast(outer, recursive = FALSE), character())
})

test_that("validate_ast accepts the packaged model", {
  data(example_models, package = "multimod")
  expect_equal(validate_ast(example_models$energyRt$multimod), character())
})

test_that("validate.ast reports schema problems through the existing generic", {
  withr::local_options(multimod.validate = FALSE)
  bad <- new_ast("expression", op = "*", lhs = ast_symbol("a"))
  res <- validate(bad, stop_on_error = FALSE)
  expect_false(isTRUE(res$valid))
  expect_match(res$errors, "required field 'rhs'", all = FALSE)
})
