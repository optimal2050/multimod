# Every writer should render every kind of AST node. Three gaps were found one
# at a time by tripping over them - as_gams() lacked `call`, then `setmin`,
# `setmax` and `shift` - each time producing raw R structures in the middle of
# an equation rather than an error. This test holds the whole matrix, so the
# next omission fails here instead.

writer_test_nodes <- function(m) {
  d1 <- ast_dims("tech")
  d2 <- ast_dims("tech", "region")
  v <- ast_variable("vGen", dims = d2)
  p <- ast_parameter("pAvail", dims = d2)
  list(
    constant    = ast_constant(1),
    symbol      = ast_symbol("tech"),
    set_ref     = ast_set("tech"),
    param_ref   = p,
    var_ref     = v,
    mapping_ref = ast_mapping("mTR", dims = d2),
    dims        = d2,
    shift       = ast_shift("region", -1),
    expression  = ast_expression("*", lhs = p, rhs = v),
    unary       = ast_unary("-", v),
    call        = ast_call("abs", args = list(v)),
    func        = ast_func("sum", value = v, index = d1),
    sum         = ast_sum(index = d1, value = v),
    prod        = ast_prod(index = d1, value = v),
    setmin      = ast_setmin(index = d1, value = v),
    setmax      = ast_setmax(index = d1, value = v),
    when        = ast_when(condition = ast_mapping("mTR", dims = d2), then = v),
    where       = ast_where(name = "w1", content = v),
    equation    = m$equations$eqCapLim
  )
}

# as_pyomo() renders a constraint as a `rule=` function plus a Constraint()
# call, which is several statements rather than one expression, and an index
# list differs by position (`model.tech, model.region` in the signature,
# `[tech, r]` in a subscript). Both are reached through write_pyomo() instead.
pyomo_exceptions <- c("dims", "equation")

test_that("every writer renders every kind of AST node", {
  m <- example_models$toy
  nodes <- writer_test_nodes(m)

  for (w in c("as_gams", "as_gmpl", "as_jump", "as_latex")) {
    f <- get(w, envir = asNamespace("multimod"))
    for (nm in names(nodes)) {
      out <- suppressWarnings(f(nodes[[nm]], model = m))
      expect_type(out, "character")
      expect_true(nzchar(paste(out, collapse = "")),
                  info = paste(w, "produced nothing for", nm))
      # as_gams() used to emit `list(name = "min", args = ...)` here
      expect_false(any(grepl("list(", out, fixed = TRUE)),
                   info = paste(w, "leaked an R structure for", nm))
    }
  }
})

test_that("as_pyomo renders every node except its documented exceptions", {
  m <- example_models$toy
  nodes <- writer_test_nodes(m)
  for (nm in setdiff(names(nodes), pyomo_exceptions)) {
    out <- suppressWarnings(as_pyomo(nodes[[nm]], model = m))
    expect_type(out, "character")
    expect_true(nzchar(paste(out, collapse = "")),
                info = paste("as_pyomo produced nothing for", nm))
  }
})

test_that("a function Pyomo cannot express is refused with a clear message", {
  m <- example_models$toy
  v <- ast_variable("vGen", dims = ast_dims("tech", "region"))
  expect_error(as_pyomo(ast_func("log", value = v), model = m),
               "Unsupported function")
})
