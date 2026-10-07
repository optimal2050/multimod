# Builds `example_models$toy`: a small, fully populated capacity-expansion LP
# written with the multimod DSL. Every parameter carries data, so the model
# goes end to end through the matrix route (model_to_lp() / solve_highs()) in
# well under a second; it is what the DSL vignette and the solver examples use.
#
# Run from the package root after `data-raw/DATASET.R` (which creates the
# energyRt entry):   Rscript data-raw/toy_model.R

pkgload::load_all(".", quiet = TRUE)

techs   <- c("coal", "gas", "wind")
regions <- c("north", "south")

sets <- list(
  tech   = new_set("tech",   desc = "generation technologies", data = techs),
  region = new_set("region", desc = "model regions",           data = regions)
)

# The matrix route enumerates variable columns and equation rows from domain
# mappings, so every indexed symbol names one.
tuples <- expand.grid(tech = techs, region = regions, stringsAsFactors = FALSE)
mappings <- list(
  mTR = new_mapping("mTR", desc = "technology x region pairs",
                    dims = c("tech", "region"), data = tuples),
  mR  = new_mapping("mR",  desc = "regions",
                    dims = "region", data = data.frame(region = regions))
)

parameters <- list(
  pCapCost = new_parameter("pCapCost", desc = "annualised capacity cost, EUR/kW",
    dims = "tech",
    data = data.frame(tech = techs, value = c(120, 70, 90))),
  pVarCost = new_parameter("pVarCost", desc = "variable cost, EUR/MWh",
    dims = "tech",
    data = data.frame(tech = techs, value = c(30, 55, 0))),
  pAvail = new_parameter("pAvail", desc = "availability factor",
    dims = c("tech", "region"),
    data = expand.grid(tech = techs, region = regions, stringsAsFactors = FALSE) |>
      transform(value = c(0.85, 0.85, 0.90, 0.90, 0.35, 0.25))),
  pDemand = new_parameter("pDemand", desc = "annual demand, MWh",
    dims = "region",
    data = data.frame(region = regions, value = c(1000, 600)))
)

variables <- list(
  vCap = new_variable("vCap", desc = "installed capacity, kW",
                      dims = c("tech", "region"), domain = "mTR",
                      vtype = "positive", bounds = list(lo = 0)),
  vGen = new_variable("vGen", desc = "generation, MWh",
                      dims = c("tech", "region"), domain = "mTR",
                      vtype = "positive", bounds = list(lo = 0)),
  vCost = new_variable("vCost", desc = "total system cost, EUR", dims = character(0))
)

# vGen[tech, region] <= pAvail[tech, region] * vCap[tech, region]
eqCapLim <- new_equation("eqCapLim", desc = "generation is limited by available capacity",
  dims = c("tech", "region"),
  lhs = ast_variable("vGen", dims = ast_dims("tech", "region")),
  rhs = ast_expression("*",
    ast_parameter("pAvail", dims = ast_dims("tech", "region")),
    ast_variable("vCap", dims = ast_dims("tech", "region"))),
  relation = "<=", domain = ast_mapping("mTR", dims = ast_dims("tech", "region")))

# sum(tech, vGen[tech, region]) >= pDemand[region]
eqDemand <- new_equation("eqDemand", desc = "demand is met in every region",
  dims = "region",
  lhs = ast_sum(ast_dims("tech"), ast_variable("vGen", dims = ast_dims("tech", "region"))),
  rhs = ast_parameter("pDemand", dims = ast_dims("region")),
  relation = ">=", domain = ast_mapping("mR", dims = ast_dims("region")))

# vCost == sum((tech, region), pCapCost[tech] * vCap + pVarCost[tech] * vGen)
eqCost <- new_equation("eqCost", desc = "total cost",
  dims = character(0),
  lhs = ast_variable("vCost", dims = character(0)),
  rhs = ast_sum(ast_dims("tech", "region"),
    ast_expression("+",
      ast_expression("*", ast_parameter("pCapCost", dims = ast_dims("tech")),
                          ast_variable("vCap", dims = ast_dims("tech", "region"))),
      ast_expression("*", ast_parameter("pVarCost", dims = ast_dims("tech")),
                          ast_variable("vGen", dims = ast_dims("tech", "region"))))),
  relation = "==")

toy <- new_model(
  name = "toy", desc = "two-region capacity expansion toy model",
  sets = sets, mappings = mappings, parameters = parameters, variables = variables,
  equations = list(eqCapLim = eqCapLim, eqDemand = eqDemand, eqCost = eqCost),
  objectives = list(list(equation = "eqCost", variable = "vCost",
                         sense = "minimize", model = "toy",
                         optimization_problem = "LP")),
  metadata = list(source = "data-raw/toy_model.R", language = "multimod")
)

# short iterator names (t, r, ...) for the text backends (GMPL, JuMP, Pyomo)
toy <- add_index_aliases(toy)

v <- validate(toy, stop_on_error = TRUE)
lp <- model_to_lp(toy)
cat("LP:", nrow(lp$A), "rows x", ncol(lp$A), "cols\n")
if (requireNamespace("highs", quietly = TRUE)) {
  r <- solve_highs(toy, attach = FALSE)
  cat("objective:", r$objective, "status:", r$status, "\n")
}

# Write only when asked: MULTIMOD_WRITE_TOY=1 Rscript data-raw/toy_model.R
if (nzchar(Sys.getenv("MULTIMOD_WRITE_TOY"))) {
  e <- new.env(); load("data/example_models.rda", envir = e)
  example_models <- e$example_models
  example_models$toy <- toy
  save(example_models, file = "data/example_models.rda", compress = "bzip2", version = 2)
  cat("wrote data/example_models.rda with $toy\n")
}
