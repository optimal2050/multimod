# A GAMS subset is declared `sub(parent)`, which is syntactically identical to a
# one-dimensional mapping declaration. It is both: an index set where it appears
# bare, a mapping where it gates an expression. Only the USE tells them apart.
#
# Getting this wrong is silent in the declaration and fatal in the body:
# classifying a subset as a mapping makes a bare `sum(dem$...)` build a mapping
# with no dimensions, and classifying a single-dimension gate as a set makes
# `$mFlag(comm)` call `ast_set()` with an index list it does not accept.

gms_subsets <- c(
  "sets",
  "comm        commodity",
  "process     any process - see the subsets below",
  "dem(process)  demand",
  ";",
  "sets",
  "mDemComm(dem, comm)   demand to commodity",
  "mFlag(comm)           single-dimension gate",
  ";",
  "parameters",
  "pDemand(dem, comm)    demand",
  ";",
  "variables",
  "vX(comm)              activity",
  ";",
  "equations",
  "eqA(comm)             the equation",
  ";",
  "eqA(comm)$mFlag(comm)..",
  "    vX(comm) =e= sum(dem$mDemComm(dem, comm), pDemand(dem, comm));"
)

test_that("a subset is registered as both a set and a mapping", {
  ms <- read_gams(gms_subsets, include = FALSE, verbose = FALSE)

  # the parent and the plain set are sets; the subset is in BOTH lists
  expect_true(all(c("comm", "process", "dem") %in% names(ms$sets)))
  expect_true("dem" %in% names(ms$mappings))

  # a single-dimension gate keeps its mapping registration
  expect_true("mFlag" %in% names(ms$mappings))
})

test_that("as_multimod resolves a subset by how it is used", {
  ms <- read_gams(gms_subsets, include = FALSE, verbose = FALSE)
  m <- as_multimod(ms)

  expect_length(m$equations, 1L)
  eq <- m$equations[["eqA"]]

  # the equation's domain is the one-dimensional gate, as a mapping
  expect_equal(node_type(eq$domain), "mapping")
  expect_equal(eq$domain$name, "mFlag")

  # `sum(dem$mDemComm(dem, comm), ...)` parses as a sum whose index is a
  # `when`: the bare subset is the index, the gate is the condition.
  expect_equal(node_type(eq$rhs), "func")
  expect_equal(eq$rhs$name, "sum")

  idx <- eq$rhs$index
  expect_equal(node_type(idx), "when")

  # the bare occurrence resolves to a SET ...
  expect_equal(node_type(idx$then), "set")
  expect_equal(idx$then$name, "dem")

  # ... and the parenthesised one to a MAPPING
  expect_equal(node_type(idx$condition), "mapping")
  expect_equal(idx$condition$name, "mDemComm")
})

test_that("detect_symbol_type picks set or mapping from the use", {
  symbols <- list(sets = c("comm", "dem"), mappings = c("dem", "mFlag"),
                  parameters = character(), variables = character())

  # bare -> set, indexed -> mapping, for a name in both lists
  expect_equal(detect_symbol_type("dem", symbols, indexed = FALSE), "set")
  expect_equal(detect_symbol_type("dem", symbols, indexed = TRUE), "mapping")

  # a name in one list only is unaffected by the context
  expect_equal(detect_symbol_type("mFlag", symbols, indexed = FALSE), "mapping")
  expect_equal(detect_symbol_type("comm", symbols, indexed = TRUE), "set")

  # callers that do not track the context keep the long-standing order
  expect_equal(detect_symbol_type("dem", symbols), "mapping")
})
