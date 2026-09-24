# =============================================================================
# Language-neutral AST serialisation
# =============================================================================
#
# The point of this format is that something other than R can read the model.
# So the tests that matter are not "does it produce JSON" but:
#
#   1. does the schema actually cover the AST as it exists, and
#   2. does a model survive the round trip well enough to produce the SAME
#      matrix - byte for byte, not approximately.
#
# (2) is checked against the same independent oracle as test-highs-solve.R:
# energyRt's own GMPL, solved by glpsol, never touching multimod.
# =============================================================================

EXPECTED_OBJECTIVE <- 46398.4234209742

fixture <- function() {
  data(example_models, package = "multimod")
  example_models$energyRt$multimod
}

# walk every AST node in an object
each_node <- function(x, f) {
  if (!is.list(x) || is.data.frame(x)) return(invisible())
  if (inherits(x, "ast")) f(x)
  for (e in x) each_node(e, f)
  invisible()
}

# a true round trip: through JSON TEXT, not just the intermediate list
json_rt <- function(x, data = "inline") {
  txt <- jsonlite::toJSON(ast_to_json(x, data = data), auto_unbox = TRUE,
                          null = "null", digits = I(17))
  ast_from_json(jsonlite::fromJSON(txt, simplifyVector = FALSE))
}

# --- the schema describes the AST that actually exists -----------------------

test_that("every node in the packaged model resolves to a schema type", {
  unresolved <- character()
  each_node(fixture(), function(n) {
    if (is.na(schema_type(n))) {
      unresolved <<- c(unresolved, paste(class(n), collapse = "/"))
    }
  })
  expect_equal(unique(unresolved), character())
})

test_that("no node carries a field the schema does not declare", {
  sc <- ast_schema()$nodes            # hoisted; rebuilding it per node is slow
  probs <- character()
  each_node(fixture(), function(n) {
    sp <- sc[[schema_type(n)]]
    if (isTRUE(sp$unnamed)) return()
    extra <- setdiff(names(n), names(sp$fields))
    if (length(extra)) {
      probs <<- c(probs, paste0(schema_type(n), "$", extra))
    }
  })
  expect_equal(unique(probs), character())
})

test_that("declaration and reference are distinguishable", {
  # the whole reason the schema splits them: the S3 class name does not
  expect_equal(class(ast_set("tech"))[1], class(new_set("tech"))[1])
  expect_equal(schema_type(ast_set("tech")), "set_ref")
  expect_equal(schema_type(new_set("tech")), "set_decl")
  # and the marker that separates them is exactly `multimod`
  expect_false(inherits(ast_set("tech"), "multimod"))
  expect_true(inherits(new_set("tech"), "multimod"))
})

# --- round trip --------------------------------------------------------------

test_that("every node in the packaged model round-trips identically", {
  n <- 0L
  bad <- character()
  each_node(fixture(), function(node) {
    n <<- n + 1L
    if (!identical(node, json_rt(node))) {
      bad <<- c(bad, schema_type(node))
    }
  })
  expect_gt(n, 10000L)                 # the walk really did visit the model
  expect_equal(unique(bad), character())
})

test_that("a whole model round-trips identically, slot by slot", {
  m <- fixture()
  m2 <- model_from_json(model_to_json(m, data = "inline", pretty = FALSE))
  for (sl in c("sets", "mappings", "parameters", "variables", "equations")) {
    expect_identical(m2[[sl]], m[[sl]], info = sl)
  }
  expect_identical(names(m2), names(m))
})

test_that("the round-tripped model assembles the identical matrix", {
  m <- fixture()
  m2 <- model_from_json(model_to_json(m, data = "inline", pretty = FALSE))
  lp1 <- model_to_lp(m)
  lp2 <- model_to_lp(m2)

  expect_equal(dim(lp2$A), dim(lp1$A))
  expect_equal(length(lp2$A@x), length(lp1$A@x))
  expect_identical(lp2$A@x, lp1$A@x)          # identical, not all.equal
  expect_identical(lp2$row_lo, lp1$row_lo)
  expect_identical(lp2$row_up, lp1$row_up)
  expect_identical(lp2$col_lo, lp1$col_lo)
  expect_identical(lp2$col_up, lp1$col_up)
})

test_that("the round-tripped model solves to the reference objective", {
  skip_if_not_installed("highs")
  m2 <- model_from_json(model_to_json(fixture(), data = "inline", pretty = FALSE))
  r <- solve_highs(m2, attach = FALSE)
  expect_equal(r$status_message, "Optimal")
  expect_equal(r$objective, EXPECTED_OBJECTIVE, tolerance = 1e-8)
})

# --- the things that were actually lost the first time -----------------------

test_that("model slots the schema does not declare are refused, not dropped", {
  # `aliases` went missing this way and cost eqEmsFuelTot 672 of its 700
  # nonzeros - a matrix of the right shape with the wrong contents
  m <- fixture()
  m$some_new_slot <- list(a = 1)
  expect_error(model_to_json(m), "does not declare")
  expect_error(model_to_json(m), "some_new_slot")
})

test_that("aliases survive, because the matrix depends on them", {
  m <- fixture()
  skip_if(is.null(m$aliases))
  m2 <- model_from_json(model_to_json(m, data = "inline", pretty = FALSE))
  expect_identical(m2$aliases, m$aliases)
  expect_identical(m2$index_aliases, m$index_aliases)
})

test_that("doubles survive the text round trip bit-for-bit", {
  # jsonlite's `digits = NA` loses ~3e-15 relative; 17 significant digits is
  # the IEEE 754 guarantee
  x <- c(1/3, pi, 143.13413443434343, 1e-17, 123456789.123456789)
  # a DECLARATION - only declarations carry data, which is the whole point of
  # the split; `ast_parameter()` makes a reference and is rightly refused
  p <- new_parameter("p", dims = ast_dims(ast_set("i")),
                     data = data.frame(i = seq_along(x), value = x))
  expect_identical(json_rt(p)$data$value, x)
})

test_that("a classed atomic survives (POSIXct, named vectors)", {
  p <- new_parameter("p", dims = ast_dims(ast_set("i")))
  p$misc <- list(stamp = as.POSIXct("2026-09-08 21:53:42", tz = "UTC"),
                 named = c(a = 1L, b = 2L),
                 flag  = TRUE,
                 empty = character(0))
  q <- json_rt(p)
  expect_identical(q$misc$stamp, p$misc$stamp)
  expect_identical(q$misc$named, p$misc$named)
  expect_identical(q$misc$empty, p$misc$empty)
})

test_that("present-but-NULL is distinguished from absent", {
  # equation$comment is present and NULL on all 152 equations; if the round
  # trip dropped it the node would no longer be identical()
  n <- ast_symbol("x")
  n$extra_absent <- NULL              # assigning NULL removes it
  expect_false("extra_absent" %in% names(n))

  e <- fixture()$equations[[1]]
  expect_true("comment" %in% names(e))
  expect_null(e$comment)
  e2 <- json_rt(e)
  expect_true("comment" %in% names(e2))
  expect_null(e2$comment)
})

test_that("row.names of a subset table survive", {
  d <- data.frame(a = 1:10, value = as.numeric(1:10))
  d <- d[4:8, ]                        # row.names become 4..8
  p <- new_parameter("p", dims = ast_dims(ast_set("a")), data = d)
  expect_identical(json_rt(p)$data, d)
})

# --- data placement ----------------------------------------------------------

test_that("data is omitted by default and kept on request", {
  m <- fixture()
  nm <- names(Filter(function(p) is.data.frame(p$data) && nrow(p$data) > 0,
                     m$parameters))[1]
  skip_if(is.na(nm))
  p <- m$parameters[[nm]]

  expect_null(ast_to_json(p)$data)                     # default: omit
  expect_false(is.null(ast_to_json(p, data = "inline")$data))

  # structure is unaffected by the choice
  expect_identical(json_rt(p, data = "omit")$dims, p$dims)
  expect_identical(json_rt(p, data = "omit")$name, p$name)
})

# --- validation and the emitted schema ---------------------------------------

test_that("validate_ast accepts the packaged model and rejects a broken node", {
  expect_equal(validate_ast(fixture()), character())

  broken <- ast_expression("*", ast_symbol("a"), ast_symbol("b"))
  broken$rhs <- NULL
  expect_match(validate_ast(broken), "required field 'rhs'", all = FALSE)

  broken2 <- ast_symbol("a")
  broken2$not_a_field <- 1
  expect_match(validate_ast(broken2), "undeclared field", all = FALSE)
})

test_that("ast_json_schema emits parseable JSON Schema covering every type", {
  js <- ast_json_schema()
  parsed <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  expect_equal(parsed$properties$multimod_schema$const, ast_schema()$version)
  for (ty in schema_node_types()) {
    expect_false(is.null(parsed$`$defs`[[ty]]), info = ty)
  }
})

test_that("an unknown node type is refused with a useful message", {
  expect_error(ast_from_json(list(node = "no_such_node")), "unknown node type")
  expect_error(ast_from_json(list(name = "x")), "no 'node' tag")
})

test_that("a document from a different schema version warns", {
  js <- model_to_json(fixture(), pretty = FALSE)
  js2 <- sub('"multimod_schema":"[^"]*"', '"multimod_schema":"0.1"', js)
  expect_warning(model_from_json(js2), "schema")
})

test_that("model_to_json writes a file that reads back", {
  m <- fixture()
  f <- withr::local_tempfile(fileext = ".json")
  model_to_json(m, path = f, data = "inline")
  expect_true(file.exists(f))
  m2 <- model_from_json(f)
  expect_identical(m2$equations, m$equations)
})
