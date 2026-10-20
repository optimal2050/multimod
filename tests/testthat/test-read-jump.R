jump_cap <- "@constraint(model, eqCapLim[(h, r) in mTR],
               vGen[(h, r)] <= get(pAvail, (h, r), pAvailDef) * vCap[(h, r)])"

test_that("what write_jump() emits reads back unchanged", {
  m <- example_models$toy
  for (nm in names(m$equations)) {
    ref <- m$equations[[nm]]
    got <- mm_equation(m, as_jump(ref, model = m), lang = "jump")
    expect_equal(got$name, ref$name, info = nm)
    expect_equal(got$relation, ref$relation, info = nm)
    expect_equal(as_gams(got$lhs), as_gams(ref$lhs), info = nm)
    expect_equal(as_gams(got$rhs), as_gams(ref$rhs), info = nm)
    expect_equal(get_dim_names(got$dims), get_dim_names(ref$dims), info = nm)
    expect_equal(got$domain$name, ref$domain$name, info = nm)
  }
})

test_that("JuMP's short index names come back as the model's own", {
  m <- example_models$toy
  eq <- mm_equation(m, jump_cap, lang = "jump")
  # the text says `h` and `r`; the model says `tech` and `region`
  expect_equal(get_dim_names(eq$dims), c("tech", "region"))
  expect_equal(as_gams(eq$lhs), "vGen[tech,region]")
  expect_equal(eq$domain$name, "mTR")
})

test_that("get(p, i, default) reads back as a plain parameter reference", {
  m <- example_models$toy
  eq <- mm_equation(m, jump_cap, lang = "jump")
  expect_s3_class(eq$rhs$lhs, "mm_parameter")
  expect_equal(eq$rhs$lhs$name, "pAvail")
})

test_that("the three notations describe the same equation", {
  m <- example_models$toy
  j <- mm_equation(m, jump_cap, lang = "jump")
  g <- mm_equation(m, "eqCapLim{(tech,region) in mTR}:
                     vGen[tech,region] <= pAvail[tech,region] * vCap[tech,region]")
  a <- mm_equation(m, "eqCapLim(tech,region)$mTR(tech,region) ..
                     vGen(tech,region) =l= pAvail(tech,region) * vCap(tech,region)",
                   lang = "gams")
  expect_equal(as_gams(j$lhs), as_gams(g$lhs))
  expect_equal(as_gams(j$rhs), as_gams(g$rhs))
  expect_equal(as_gams(j$lhs), as_gams(a$lhs))
  expect_equal(as_gams(j$rhs), as_gams(a$rhs))
  expect_equal(j$relation, g$relation)
  expect_equal(j$relation, a$relation)
})

test_that("a model authored in JuMP solves to the same optimum", {
  skip_if_not_installed("highs")
  m <- example_models$toy
  ref <- solve_highs(m, attach = FALSE)

  m2 <- m
  m2$equations$eqCapLim <- mm_equation(m, jump_cap, lang = "jump")
  m2$equations$eqDemand <- mm_equation(m,
    "@constraint(model, eqDemand[r in mR],
       sum(vGen[t, r] for t in tech) >= pDemand[r])", lang = "jump")
  m2$equations$eqCost <- mm_equation(m,
    "@constraint(model, eqCost,
       vCost == sum(pCapCost[t] * vCap[t, r] + pVarCost[t] * vGen[t, r]
                    for t in tech, r in region))", lang = "jump")

  expect_true(validate(m2, stop_on_error = FALSE)$valid)
  expect_equal(solve_highs(m2, attach = FALSE)$objective, ref$objective,
               tolerance = 1e-9)
})

test_that("generators take several iterators, a tuple and an if filter", {
  m <- example_models$toy
  one <- mm_equation(m, "@constraint(model, eqA,
    vCost == sum(vGen[t, r] for t in tech, r in region))", lang = "jump")
  expect_equal(get_dim_names(one$rhs$index), c("tech", "region"))

  tup <- mm_equation(m, "@constraint(model, eqB,
    vCost == sum(vGen[(t, r)] for (t, r) in mTR))", lang = "jump")
  # a tuple over a mapping restricts the sum, as `sum((i,j)$S(i,j), ...)` does
  expect_s3_class(tup$rhs$index, "when")
  expect_equal(tup$rhs$index$condition$name, "mTR")

  flt <- mm_equation(m, "@constraint(model, eqC,
    vCost == sum(vGen[t, r] for t in tech, r in region if pCapCost[t] > 0))",
    lang = "jump")
  expect_s3_class(flt$rhs$value, "when")
})

test_that("an operator inside an index does not split the expression", {
  m <- example_models$toy
  # the `-` in `[t, r]`-style subscripts is inside brackets, so the top-level
  # split must not see it; get_expression_level() counts only parentheses
  eq <- mm_equation(m, "@constraint(model, eqD[r in mR],
    sum(vGen[t, r] for t in tech) >= pDemand[r])", lang = "jump")
  expect_equal(eq$relation, ">=")
  expect_s3_class(eq$lhs, "sum")
})

test_that("brackets, unary minus and calls parse", {
  m <- example_models$toy
  eq <- mm_equation(m, "@constraint(model, eqE[(h, r) in mTR],
    vGen[(h, r)] <= -2 * (vCap[(h, r)] + 1))", lang = "jump")
  expect_equal(eq$relation, "<=")
  expect_match(as_gams(eq$rhs), "vCap[tech,region]", fixed = TRUE)

  eq2 <- mm_equation(m, "@constraint(model, eqF[(h, r) in mTR],
    vGen[(h, r)] <= log(vCap[(h, r)]))", lang = "jump")
  expect_equal(as_gams(eq2$rhs), "log(vCap[tech,region])")
})

test_that("an anonymous constraint is refused, since an equation needs a name", {
  m <- example_models$toy
  expect_error(mm_equation(m, "@constraint(model, vCost >= 0)", lang = "jump"),
               "needs one")
})

test_that("a malformed statement is refused", {
  m <- example_models$toy
  expect_error(mm_equation(m, "vGen <= vCap", lang = "jump"), "@constraint")
  expect_error(mm_equation(m, "@constraint(model, eqX[r in mR], vGen[t, r])",
                           lang = "jump"), "relational operator")
  expect_error(mm_equation(m, "@constraint(model)", lang = "jump"),
               "takes the model")
})
