# read_gmpl(): MathProg models. A small inline model runs everywhere; the
# OSeMOSYS test case runs when MULTIMOD_OSEMOSYS_DIR points at a clone of
# github.com/OSeMOSYS/OSeMOSYS_GNU_MathProg (nothing of it is shipped).

small_model <- c(
  "set TECH;",
  "set REGION;",
  "param cost{t in TECH};",
  "param demand{r in REGION};",
  "param avail{t in TECH, r in REGION} default 1;",
  "var gen{t in TECH, r in REGION} >= 0;",
  "s.t. meet{r in REGION}: sum{t in TECH} gen[t,r] >= demand[r];",
  "s.t. cap{t in TECH, r in REGION}: gen[t,r] <= 100 * (avail[t,r]);",
  "minimize total: sum{t in TECH, r in REGION} cost[t] * gen[t,r];",
  "end;"
)
small_data <- c(
  "data;",
  "set TECH := coal wind;",
  "set REGION := north south;",
  "param cost := coal 30 wind 5;",
  "param demand := north 50 south 20;",
  "end;"
)

test_that("read_gmpl() parses a small MathProg model with data under schema validation", {
  m <- read_gmpl(small_model, small_data)
  expect_s3_class(m, "mm_model")
  expect_setequal(names(m$sets), c("TECH", "REGION"))
  expect_setequal(names(m$parameters), c("cost", "demand", "avail"))
  expect_true("gen" %in% names(m$variables))   # plus the objective variable
  expect_true(all(c("meet", "cap") %in% names(m$equations)))
  expect_identical(get_dim_names(m$variables$gen$dims), c("TECH", "REGION"))
  expect_equal(nrow(m$parameters$cost$data), 2)
  v <- validate(m, stop_on_error = FALSE)
  expect_true(isTRUE(v$valid))
  # a parenthesised reference used to fail validation (brackets field)
  expect_match(as_gams(m$equations$cap$rhs), "avail")
  expect_match(as_jump(m$equations$meet$lhs, model = m), "for t in TECH")
})

osemosys_dir <- Sys.getenv("MULTIMOD_OSEMOSYS_DIR", unset = "")
skip_if_no_osemosys <- function() {
  testthat::skip_if(!nzchar(osemosys_dir) || !dir.exists(osemosys_dir),
                    "MULTIMOD_OSEMOSYS_DIR not set")
  testthat::skip_if_not(file.exists(file.path(osemosys_dir, "src", "osemosys.txt")),
                        "OSeMOSYS clone has no src/osemosys.txt")
}

read_osemosys <- function() {
  suppressWarnings(read_gmpl(file.path(osemosys_dir, "src", "osemosys.txt"),
                             file.path(osemosys_dir, "tests", "utopia.txt")))
}

test_that("OSeMOSYS (UTOPIA) parses, validates and renders", {
  skip_on_cran()
  skip_if_no_osemosys()
  m <- read_osemosys()
  expect_length(m$sets, 11)
  expect_length(m$parameters, 61)
  expect_length(m$variables, 68)
  expect_length(m$equations, 95)
  expect_true(isTRUE(validate(m, stop_on_error = FALSE)$valid))
  expect_identical(m$objectives[[1]]$sense, "minimize")

  tex <- write_latex(m)
  expect_true(any(grepl("TotalDiscountedCost", tex, fixed = TRUE)))

  d <- withr::local_tempdir()
  jl <- write_jump(m, model_dir = d)
  expect_true(file.exists(file.path(d, "solvers", "jump", "data.jl")))
  code <- paste(jl, collapse = "\n")
  # the sum iterator over REGION must shadow the outer one, not alias it
  expect_match(code, "get(TradeRoute, (r, rr, f, y)", fixed = TRUE)
  data_jl <- readLines(file.path(d, "solvers", "jump", "data.jl"))
  # formula parameters are keyed by the declared iterators and a parametric
  # default is rendered inline
  expect_true(any(grepl("get(DiscountRate, r, DiscountRateDef)", data_jl, fixed = TRUE)))
  expect_true(any(grepl("get(DiscountRateIdv, (r, t), get(DiscountRate, r, DiscountRateDef))",
                        data_jl, fixed = TRUE)))
})

test_that("OSeMOSYS (UTOPIA) solves through JuMP to the published objective", {
  skip_on_cran()
  skip_if_no_osemosys()
  skip_if(!nzchar(Sys.which(get_multimod_julia())), "Julia not available")
  m <- read_osemosys()
  d <- withr::local_tempdir()
  write_jump(m, model_dir = d)
  r <- solve_jump(m, d, verbose = FALSE)
  expect_true(r$success)
  expect_equal(r$objective, 29446.86, tolerance = 1e-6)
})
