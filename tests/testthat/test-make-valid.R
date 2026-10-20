test_model_with_symbols <- function(lhs_symbol = "RateOfDemand",
                                     rhs_symbol = "SpecifiedAnnualDemand") {
  sets <- list(
    REGION = new_set("REGION"),
    FUEL = new_set("FUEL"),
    YEAR = new_set("YEAR")
  )

  param <- new_parameter(
    name = "SpecifiedAnnualDemand",
    dims = c("REGION", "FUEL", "YEAR"),
    data = NULL
  )

  var <- new_variable(
    name = "RateOfDemand",
    dims = c("REGION", "FUEL", "YEAR"),
    domain = NULL
  )

  symbol_table <- list(
    sets = names(sets),
    aliases = character(0),
    mappings = character(0),
    parameters = names(list(SpecifiedAnnualDemand = TRUE)),
    variables = names(list(RateOfDemand = TRUE)),
    language = "gmpl"
  )

  lhs <- parse_gmpl_expr(sprintf("%s[r,f,y]", lhs_symbol), symbols = symbol_table)
  rhs <- parse_gmpl_expr(sprintf("%s[r,f,y]", rhs_symbol), symbols = symbol_table)

  eq <- new_equation(
    name = "EQ_Demand",
    dims = c("REGION", "FUEL", "YEAR"),
    lhs = lhs,
    rhs = rhs,
    relation = "==",
    dims_index_aliases = c(REGION = "r", FUEL = "f", YEAR = "y")
  )

  model <- new_model(
    name = "demo",
    sets = sets,
    parameters = list(SpecifiedAnnualDemand = param),
    variables = list(RateOfDemand = var),
    equations = list(EQ_Demand = eq)
  )
  model$index_aliases <- c(REGION = "r", FUEL = "f", YEAR = "y")
  model
}


test_that("parse_gmpl_expr emits typed nodes when symbols are declared", {
  model <- test_model_with_symbols(lhs_symbol = "rateofdemand", rhs_symbol = "specifiedannualdemand")
  expect_identical(node_type(model$equations$EQ_Demand$lhs), "variable")
  expect_identical(model$equations$EQ_Demand$lhs$name, "RateOfDemand")
  expect_identical(node_type(model$equations$EQ_Demand$rhs), "parameter")
  expect_identical(model$equations$EQ_Demand$rhs$name, "SpecifiedAnnualDemand")
})


test_that("make_valid preserves already-typed GMPL expressions", {
  model <- test_model_with_symbols(lhs_symbol = "rateofdemand", rhs_symbol = "specifiedannualdemand")
  fixed <- make_valid(model, revalidate = FALSE)
  expect_identical(node_type(fixed$equations$EQ_Demand$lhs), "variable")
  expect_identical(fixed$equations$EQ_Demand$lhs$name, "RateOfDemand")
  expect_identical(node_type(fixed$equations$EQ_Demand$rhs), "parameter")
  expect_identical(fixed$equations$EQ_Demand$rhs$name, "SpecifiedAnnualDemand")
})


test_that("validate flags undefined symbols", {
  model <- test_model_with_symbols(rhs_symbol = "MissingInput")
  validation <- validate(model, stop_on_error = FALSE)
  expect_false(validation$valid)
  expect_gt(length(validation$errors), 0)
})

test_that("validate uses global index aliases for iterators outside dims", {
  sets <- list(
    REGION = new_set("REGION"),
    TIMESLICE = new_set("TIMESLICE"),
    YEAR = new_set("YEAR")
  )

  var <- new_variable(
    name = "RateOfDemand",
    dims = c("REGION", "TIMESLICE", "YEAR"),
    domain = NULL
  )

  symbol_table <- list(
    sets = names(sets),
    aliases = character(0),
    mappings = character(0),
    parameters = character(0),
    variables = names(list(RateOfDemand = TRUE)),
    language = "gmpl"
  )

  lhs <- parse_gmpl_expr("RateOfDemand[r,l,y]", symbols = symbol_table)
  rhs <- parse_gmpl_expr("0", symbols = symbol_table)

  eq <- new_equation(
    name = "EQ_TimeSliceBalance",
    dims = c("REGION", "YEAR"),
    lhs = lhs,
    rhs = rhs,
    relation = "==",
    dims_index_aliases = c(REGION = "r", YEAR = "y")
  )

  model <- new_model(
    name = "demo_global_alias",
    sets = sets,
    variables = list(RateOfDemand = var),
    equations = list(EQ_TimeSliceBalance = eq)
  )
  model$index_aliases <- c(REGION = "r", YEAR = "y", TIMESLICE = "l")
  model$objectives <- list(list(variable = "RateOfDemand", sense = "minimize"))

  validation <- validate(model, stop_on_error = FALSE)
  expect_true(validation$valid)
})

