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

test_that("an iterated operator keeps `*` and `/` inside the summation", {
  # Checked against glpsol 5.0 on
  #   set L; param w{l in L}; var x{l in L} >= 0;
  #   maximize obj: <form>;   s.t. budget: sum{l in L} x[l] <= 1;
  #   data; set L := a b c;   param w := a 1 b 2 c 10;
  # which is optimal at 10 for `x[l] * w[l]` (the sum of the products) and at
  # 1 for `x[l] / w[l]`, and which glpsol rejects for `x[l] + w[l]` because
  # `l` is unbound once outside the sum. So `*` and `/` are part of the
  # integrand, and `+` and `-` end its scope.
  decl <- c("set L;", "param w{l in L};", "var x{l in L} >= 0;")
  lhs_of <- function(form) {
    m <- suppressWarnings(as_multimod(read_gmpl(
      c(decl, paste0("s.t. e: ", form, " <= 1;"), "end;"))))
    as_gams(m$equations$e$lhs)
  }

  expect_equal(lhs_of("sum{l in L} x[l] * w[l]"),   "sum(L, x[L] * w[L])")
  expect_equal(lhs_of("sum{l in L} (x[l] * w[l])"), "sum(L, x[L] * w[L])")
  expect_equal(lhs_of("sum{l in L} w[l] * x[l]"),   "sum(L, w[L] * x[L])")
  expect_equal(lhs_of("sum{l in L} x[l] / w[l]"),   "sum(L, x[L] / w[L])")
  expect_equal(lhs_of("prod{l in L} x[l] * w[l]"),  "prod(L, x[L] * w[L])")

  # `+` and `-` end the scope, so the trailing term stays outside
  expect_equal(lhs_of("sum{l in L} x[l] + w[l]"),   "sum(L, x[L]) + w[L]")
  expect_equal(lhs_of("sum{l in L} x[l] - w[l]"),   "sum(L, x[L]) - w[L]")

  # a factor in front of the sum is not swallowed by it
  expect_equal(lhs_of("2 * sum{l in L} x[l]"),      "2 * sum(L, x[L])")
})

test_that("commas separate data items, as MathProg allows", {
  m <- read_gmpl(c(
    "set L;", "set M;",
    "param w{l in L};",
    "param z{l in L, m in M};",
    "var x{l in L} >= 0;",
    "s.t. budget: sum{l in L} x[l] <= 1;",
    "data;",
    "set L := a, b, c;",
    "set M := p, q;",
    "param w := a 1, b 2, c 10;",
    "param z : p, q :=",
    "  a  1, 2",
    "  b  3, 4",
    "  c  5, 6;",
    "end;"))
  expect_equal(m$sets$L$data, c("a", "b", "c"))
  expect_equal(m$sets$M$data, c("p", "q"))
  expect_equal(m$parameters$w$data$value, c(1, 2, 10))
  expect_equal(nrow(m$parameters$z$data), 6L)
  expect_equal(sort(m$parameters$z$data$value), as.numeric(1:6))
})

test_that("a model file may carry its own data section", {
  m <- read_gmpl(c(
    "set L;",
    "param w{l in L};",
    "var x{l in L} >= 0;",
    "maximize obj: sum{l in L} x[l] * w[l];",
    "s.t. budget: sum{l in L} x[l] <= 1;",
    "data;",
    "set L := a b c;",
    "param w := a 1 b 2 c 10;",
    "end;"))
  # an inline data section stands in for `data_file`, so the result is a model
  expect_s3_class(m, "mm_model")
  expect_equal(m$sets$L$data, c("a", "b", "c"))
  expect_equal(m$parameters$w$data$value, c(1, 2, 10))
})

test_that("a bare index set keeps its dimensions", {
  m <- suppressWarnings(as_multimod(read_gmpl(c(
    "set L;", "set M;",
    "param w{L};",
    "param z{L, M};",
    "var x{L} >= 0;",
    "s.t. budget: sum{l in L} x[l] <= 1;",
    "end;"))))
  # `param w{L}` is `param w{l in L}` with the index left implicit
  expect_equal(get_dim_names(m$parameters$w$dims), "L")
  expect_equal(get_dim_names(m$parameters$z$dims), c("L", "M"))
  expect_equal(get_dim_names(m$variables$x$dims), "L")
  # and the set is not recorded as an alias of itself
  expect_null(m$aliases[["L"]])
})

test_that("a model declared over plain sets reaches the matrix backend", {
  skip_if_not_installed("highs")
  # glpsol's optimum for this model is 10: spend the budget on the member with
  # the largest weight. Every variable here is declared over a set rather than
  # a mapping, which used to collapse to a single scalar column.
  m <- read_gmpl(c(
    "set L;",
    "param w{l in L};",
    "var x{l in L} >= 0;",
    "maximize obj: sum{l in L} x[l] * w[l];",
    "s.t. budget: sum{l in L} x[l] <= 1;",
    "data;",
    "set L := a b c;",
    "param w := a 1 b 2 c 10;",
    "end;"))
  ci <- build_col_index(m)
  expect_equal(nrow(ci[ci$symbol == "x", ]), 3L)
  expect_equal(solve_highs(m, attach = FALSE)$objective, 10, tolerance = 1e-9)
})

test_that("an equation restricted by a condition keeps only the rows that pass", {
  m <- read_gmpl(c(
    "set L;",
    "param w{l in L};",
    "var x{l in L} >= 0;",
    "s.t. cap{l in L: w[l] > 1}: x[l] <= 1;",
    "data;",
    "set L := a b c;",
    "param w := a 1 b 2 c 10;",
    "end;"))
  # the condition holds for b and c only, so the equation contributes 2 rows
  ri <- build_row_index(m)
  expect_equal(sum(ri$symbol == "cap"), 2L)
  expect_setequal(ri$i1[ri$symbol == "cap"], c("b", "c"))
})
