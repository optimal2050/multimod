# `<set>.val` in the matrix evaluator, and index-alias resolution for the
# intrinsic functions. Found by the mm-route battery: the 41v vintage-group
# cap.up user constraints carry `val(y)` (short alias of `year`), which
# as_jump translated but as_matrix refused.

test_that(".mx_env_name resolves short aliases and alias groups", {
  ctx <- list(model = list(index_aliases = c(year = "y", region = "r"),
                           aliases = list(c("year", "yearp"))))
  env <- data.frame(year = "2030", region = "AL")

  expect_identical(multimod:::.mx_env_name(ctx, env, "year"), "year")
  expect_identical(multimod:::.mx_env_name(ctx, env, "y"), "year")
  expect_identical(multimod:::.mx_env_name(ctx, env, "yearp"), "year")
  # unknown names pass through untouched (the caller errors with context)
  expect_identical(multimod:::.mx_env_name(ctx, env, "zzz"), "zzz")
})

test_that("val() evaluates numeric member labels and refuses others", {
  ctx <- list(model = list(index_aliases = c(year = "y")))
  env <- data.table::data.table(.k = 1:3,
                                year = c("2030", "2040", "2050"),
                                tech = c("A", "B", "C"))
  # parser shape: value is the argument list, each argument a named node
  node_of <- function(nm) list(name = "val", value = list(list(name = nm)))

  out <- multimod:::.mx_eval_val(node_of("y"), env, ctx)
  expect_equal(out$coef, c(2030, 2040, 2050))
  expect_equal(out$.k, 1:3)

  expect_error(multimod:::.mx_eval_val(node_of("tech"), env, ctx),
               "not numeric")
  expect_error(multimod:::.mx_eval_val(node_of("zzz"), env, ctx),
               "unbound")
})

test_that("decimal literals parse as constants, not dot-access", {
  symbols <- list(sets = character(), parameters = character(),
                  variables = character(), mappings = character(),
                  aliases = character())
  n <- multimod:::parse_gams_expr("3201.97612928635", symbols)
  expect_true(inherits(n, "constant") || identical(n$type, "constant") ||
                isTRUE(all.equal(n$value, 3201.97612928635)))
  # genuine dot-access still becomes val(<object>), with the REAL object
  d <- multimod:::parse_gams_expr("year.val", symbols)
  expect_identical(tolower(d$name), "val")
  ob <- d$value
  if (is.list(ob) && is.null(ob$name) && length(ob)) ob <- ob[[1]]
  expect_true(identical(ob$name, "year") ||
                identical(as.character(ob), "year"))
})
