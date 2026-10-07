# example_models$toy: a fully populated DSL-built LP that exercises the whole
# matrix route, and the `sum`/`prod` AST nodes that ast_sum()/ast_prod()
# produce (GAMS sums arrive as `func` nodes, so they were not covered before).

test_that("the toy model is a valid mm_model with data on every parameter", {
  toy <- example_models$toy
  expect_s3_class(toy, "mm_model")
  expect_true(all(vapply(toy$parameters, function(p) NROW(p$data) > 0, TRUE)))
  expect_setequal(names(toy$equations), c("eqCapLim", "eqDemand", "eqCost"))
  expect_identical(toy$objectives[[1]]$variable, "vCost")
  v <- validate(toy, stop_on_error = FALSE)
  expect_true(isTRUE(v$valid) || length(v$errors) == 0)
})

test_that("model_to_lp() builds the toy LP including ast_sum() rows", {
  toy <- example_models$toy
  lp <- model_to_lp(toy)
  # 6 eqCapLim + 2 eqDemand + 1 eqCost rows; 6 vCap + 6 vGen + vCost columns
  expect_identical(nrow(lp$A), 9L)
  expect_identical(ncol(lp$A), 13L)
})

test_that("solve_highs() reproduces the toy optimum", {
  skip_if_not_installed("highs")
  r <- solve_highs(example_models$toy, attach = FALSE)
  expect_equal(r$objective, 198000, tolerance = 1e-6)
})

test_that("ast_sum(domain = ) gates the summation like a GAMS $-condition", {
  toy <- example_models$toy
  # restrict the cost sum to the mTR tuples explicitly: same LP as without
  eq <- toy$equations$eqCost
  eq$rhs$domain <- ast_mapping("mTR", dims = ast_dims("tech", "region"))
  toy$equations$eqCost <- eq
  lp <- model_to_lp(toy)
  expect_identical(nrow(lp$A), 9L)
})

test_that("the toy renders to valid MathProg: iterators for ast_sum(), 1-D domains", {
  toy <- example_models$toy
  td <- withr::local_tempdir()
  out <- suppressWarnings(write_gmpl(toy, model_dir = td))
  x <- readLines(out$model)
  expect_true(any(grepl("sum{t in tech, r in region}", x, fixed = TRUE)))
  expect_true(any(grepl("eqDemand{r in mR}", x, fixed = TRUE)))   # not "{(r) in mR}"
  expect_false(any(grepl("sum{}", x, fixed = TRUE)))
  skip_if_not_installed("glpsol")
  r <- glpsol::glpsol_run(c("-m", out$model, "-d", out$data), echo = FALSE,
                          wd = dirname(out$model))
  expect_identical(r$status, 0L)
  expect_true(any(grepl("1.980000000e+05", r$log, fixed = TRUE)))
})

test_that("GAMS brackets a multi-set sum index and JuMP iterates over sets", {
  toy <- example_models$toy
  expect_match(as_gams(toy$equations$eqCost), "sum([tech,region],", fixed = TRUE)
  expect_match(as_gams(toy$equations$eqDemand), "sum(tech,", fixed = TRUE)
  code <- paste(suppressWarnings(write_jump(toy)), collapse = "\n")
  expect_match(code, "for h in tech, r in region", fixed = TRUE)
})
