gmpl_cap <- "eqCapLim{(tech,region) in mTR}:
               vGen[tech,region] <= pAvail[tech,region] * vCap[tech,region]"
gams_cap <- "eqCapLim(tech,region)$mTR(tech,region) ..
               vGen(tech,region) =l= pAvail(tech,region) * vCap(tech,region)"

test_that("MathProg is the default notation", {
  m <- example_models$toy
  eq <- mm_equation(m, gmpl_cap, desc = "capacity limit")

  expect_s3_class(eq, "mm_equation")
  expect_equal(eq$name, "eqCapLim")
  expect_equal(eq$relation, "<=")
  expect_equal(eq$desc, "capacity limit")
  expect_equal(eq$domain$name, "mTR")
})

test_that("the two notations describe the same equation", {
  m <- example_models$toy
  g <- mm_equation(m, gmpl_cap, lang = "gmpl")
  a <- mm_equation(m, gams_cap, lang = "gams")

  expect_equal(g$relation, a$relation)
  expect_equal(as_gams(g$lhs), as_gams(a$lhs))
  expect_equal(as_gams(g$rhs), as_gams(a$rhs))
  expect_equal(g$domain$name, a$domain$name)
})

test_that("the text route and the constructors agree on what they render", {
  m <- example_models$toy
  # The AST is not node-for-node identical: the parser represents sum() as a
  # `func` node and the constructors as a `sum` node - read_gams() has always
  # done the same. What must agree is the model the two describe.
  ref <- m$equations$eqCapLim
  got <- mm_equation(m, gmpl_cap, desc = ref$desc)
  expect_equal(got$relation, ref$relation)
  expect_equal(as_gams(got$lhs), as_gams(ref$lhs))
  expect_equal(as_gams(got$rhs), as_gams(ref$rhs))
})

test_that("a model authored through mm_equation solves to the same optimum", {
  skip_if_not_installed("highs")
  m <- example_models$toy
  ref <- solve_highs(m, attach = FALSE)

  m2 <- m
  m2$equations$eqCapLim <- mm_equation(m, gmpl_cap)
  m2$equations$eqDemand <- mm_equation(m,
    "eqDemand{region in mR}: sum{tech} vGen[tech,region] >= pDemand[region]")
  m2$equations$eqCost <- mm_equation(m,
    "eqCost: vCost = sum{(tech,region) in mTR}
               (pCapCost[tech] * vCap[tech,region]
              + pVarCost[tech] * vGen[tech,region])")

  expect_true(validate(m2, stop_on_error = FALSE)$valid)
  expect_equal(solve_highs(m2, attach = FALSE)$objective, ref$objective,
               tolerance = 1e-9)
})

test_that("the statement is accepted with s.t., across lines, and with a semicolon", {
  m <- example_models$toy
  one  <- mm_equation(m, "s.t. eqD{region in mR}: sum{tech} vGen[tech,region] >= pDemand[region];")
  many <- mm_equation(m, "eqD{region in mR}:
                            sum{tech} vGen[tech,region]
                              >= pDemand[region]")
  expect_equal(as_gams(one$lhs), as_gams(many$lhs))
  expect_equal(one$relation, ">=")
})

test_that("a malformed statement is refused", {
  m <- example_models$toy
  expect_error(mm_equation(m, "vGen <= vCap", lang = "gams"), "between header and body")
  expect_error(mm_equation(m, "eqX{region in mR}: vGen[tech,region]"), "relational operator")
  expect_error(mm_equation(m, ""), "non-empty")
  expect_error(mm_equation(list(), "eqX: a = b"), "multimod model")
})
