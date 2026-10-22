# Small models read from text and rendered to every language: the routes the
# Get started vignette relies on.

small_gmpl <- c(
  "set REGION;",
  "set TECH;",
  "param Demand{r in REGION};",
  "param Cost{t in TECH};",
  "var gen{t in TECH, r in REGION} >= 0;",
  "minimize total: sum{t in TECH, r in REGION} Cost[t] * gen[t,r];",
  "s.t. meet{r in REGION}: sum{t in TECH} gen[t,r] >= Demand[r];",
  "data;",
  "set REGION := north south;",
  "set TECH := coal wind;",
  "param Demand := north 10 south 5;",
  "param Cost := coal 3 wind 1;",
  "end;"
)

small_gams <- c(
  "set tech, region;",
  "parameter demand(region);",
  "positive variable gen(tech, region);",
  "variable cost;",
  "equation eqDemand(region), eqCost;",
  "eqDemand(region).. sum(tech, gen(tech, region)) =g= demand(region);",
  "eqCost.. cost =e= sum((tech, region), gen(tech, region));"
)

test_that("a reference built with character dims stores a dims node", {
  v <- ast_variable("vCost", dims = character(0))
  expect_s3_class(v$dims, "dims")
  expect_length(v$dims, 0)
  p <- ast_parameter("pCost", dims = c("tech", "region"))
  expect_s3_class(p$dims, "dims")
  expect_length(p$dims, 2)
})

test_that("ast_dims() returns a dims node passed to it unchanged", {
  d <- ast_dims("region")
  expect_identical(ast_dims(d), d)
  expect_identical(ast_dims(d, symbols = NULL), d)
})

test_that("the toy model round-trips through JSON", {
  toy <- example_models$toy
  back <- model_from_json(model_to_json(toy))
  expect_named(back$equations, names(toy$equations))
  expect_s3_class(back$equations$eqCost$lhs$dims, "dims")
})

test_that("as_gams() renders MathProg iterators as aliases of their sets", {
  m <- read_gmpl(small_gmpl)
  meet <- as_gams(m$equations$meet)
  expect_match(meet, "meet[r]..", fixed = TRUE)
  expect_match(meet, "sum(t, gen[t,r])", fixed = TRUE)

  gms <- write_gams(m)
  expect_true("alias (REGION, r);" %in% gms)
  expect_true("alias (TECH, t);" %in% gms)
  expect_true(any(startsWith(gms, "equation meet[REGION]")))
  # no empty block for a model without mappings
  expect_false(any(gms == "sets" & c(gms[-1], "") == ";"))
})

test_that("write_gams() declares each equation over its header's sets", {
  gms <- write_gams(example_models$toy)
  expect_true(any(startsWith(gms, "equation eqCapLim[tech,region]")))
  expect_true(any(startsWith(gms, "equation eqDemand[region]")))
  expect_true(any(startsWith(gms, "equation eqCost ")))
})

test_that("read_gams() reads several symbols declared on one line", {
  s <- read_gams(small_gams)
  expect_setequal(names(s$sets), c("tech", "region"))
  expect_setequal(names(s$equations), c("eqDemand", "eqCost"))
})

test_that("a GAMS equation without a domain condition renders in every language", {
  m <- as_multimod(read_gams(small_gams))
  eq <- m$equations$eqDemand
  expect_s3_class(eq$dims[[1]], "symbol")

  expect_match(as_gams(eq), "sum(tech, gen[tech,region])", fixed = TRUE)
  expect_match(as_gmpl(eq, model = m),
               "s.t. eqDemand{r in region}: sum{t in tech}(gen[t,r])", fixed = TRUE)
  expect_match(as_jump(eq, model = m), "eqDemand[r in region]", fixed = TRUE)
  expect_match(as_pyomo(eq, model = m), "Constraint(", fixed = TRUE)
})

test_that("as_pyomo() renders an equation as a rule and a constraint", {
  toy <- example_models$toy
  out <- as_pyomo(toy$equations$eqDemand, model = toy)
  expect_type(out, "character")
  expect_length(out, 1)
  expect_match(out, "def _eqDemand_rule(model", fixed = TRUE)
  expect_match(out, "model.eqDemand = Constraint(", fixed = TRUE)
})

test_that("a saved model round-trips through JSON", {
  ws <- tempfile("toy-")
  on.exit(unlink(ws, recursive = TRUE), add = TRUE)
  toy <- save_model(example_models$toy, ws, format = "csv", verbose = FALSE)
  back <- model_from_json(model_to_json(toy))
  expect_equal(back$storage$format, "csv")
  expect_named(back$equations, names(toy$equations))
})

test_that("write_latex() renders a whole model as one LaTeX document", {
  toy <- example_models$toy
  tex <- write_latex(toy, file = NULL)
  expect_type(tex, "character")
  expect_match(tex, "\\begin{document}", fixed = TRUE)
  expect_match(tex, "eqDemand", fixed = TRUE)
  expect_match(tex, "\\end{document}", fixed = TRUE)
})
