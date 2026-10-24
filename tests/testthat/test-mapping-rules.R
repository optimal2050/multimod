# =============================================================================
# Rule mappings: tuples defined by joins, enumerated on demand
# =============================================================================

pf <- data.frame(tech = c("coal", "wind", "pv"), frame = c("ANNUAL", "HOUR", "HOUR"),
                 stringsAsFactors = FALSE)
fs <- data.frame(frame = c("ANNUAL", "HOUR", "HOUR"), slice = c("y", "h1", "h2"),
                 stringsAsFactors = FALSE)
want_ts <- data.frame(tech = c("coal", "wind", "wind", "pv", "pv"),
                      slice = c("y", "h1", "h2", "h1", "h2"), stringsAsFactors = FALSE)

as_set <- function(d) {
  d <- as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
  d <- d[do.call(order, d), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# one LP as label-keyed tables, so two builds compare up to a permutation
lp_signature <- function(lp) {
  lab <- function(ix) {
    idx <- grep("^i[0-9]+$", names(ix), value = TRUE)
    parts <- cbind(ix$symbol, as.matrix(as.data.frame(ix)[, idx, drop = FALSE]))
    apply(parts, 1, function(p) paste(p[!is.na(p)], collapse = "|"))
  }
  rl <- lab(lp$row_index)
  cl <- lab(lp$col_index)
  tr <- Matrix::summary(lp$A)
  trip <- data.frame(row = rl[tr$i], col = cl[tr$j], coef = as.numeric(tr$x))
  trip <- trip[trip$coef != 0, ]
  trip <- trip[order(trip$row, trip$col), ]
  rownames(trip) <- NULL
  cols <- data.frame(col = cl, lo = lp$col_lo, up = lp$col_up, obj = lp$obj)
  cols <- cols[order(cols$col), ]; rownames(cols) <- NULL
  rows <- data.frame(row = rl, lo = lp$row_lo, up = lp$row_up)
  rows <- rows[order(rows$row), ]; rownames(rows) <- NULL
  list(trip = trip, cols = cols, rows = rows)
}

test_that("a rule joins, gates, subtracts and projects", {
  r <- mapping_rule(of = c("pf", "fs"), tables = list(pf = pf, fs = fs), cols = c("tech", "slice"))
  expect_s3_class(r, "mm_rule")
  expect_equal(as_set(materialise_rule(list(), r)), as_set(want_ts))

  # a gate keeps the matching tuples, a subtraction drops them
  hour_only <- data.frame(slice = c("h1", "h2"), stringsAsFactors = FALSE)
  g <- mapping_rule(of = c("pf", "fs"), gate = "ho", tables = list(pf = pf, fs = fs, ho = hour_only),
                    cols = c("tech", "slice"))
  expect_equal(as_set(materialise_rule(list(), g)), as_set(want_ts[want_ts$slice != "y", ]))
  d <- mapping_rule(of = c("pf", "fs"), minus = "ho", tables = list(pf = pf, fs = fs, ho = hour_only),
                    cols = c("tech", "slice"))
  expect_equal(as_set(materialise_rule(list(), d)), as_set(want_ts[want_ts$slice == "y", ]))

  # the hash follows the content
  pf2 <- pf; pf2$frame[1] <- "HOUR"
  r2 <- mapping_rule(of = c("pf", "fs"), tables = list(pf = pf2, fs = fs), cols = c("tech", "slice"))
  expect_false(identical(r$hash, r2$hash))
  expect_identical(r$hash, mapping_rule(of = c("pf", "fs"), tables = list(pf = pf, fs = fs),
                                        cols = c("tech", "slice"))$hash)

  # errors: an unknown leaf, a column in no table
  bad <- mapping_rule(of = c("pf", "nowhere"), tables = list(pf = pf), cols = "tech")
  expect_error(materialise_rule(list(), bad), "neither an embedded table")
  bad2 <- mapping_rule(of = "pf", tables = list(pf = pf), cols = c("tech", "region"))
  expect_error(materialise_rule(list(), bad2), "in none of the joined tables")
})

test_that("leaves resolve to the model's mappings and sets; no shared column is a product", {
  m <- example_models$toy
  # tech x region as sets: the toy's mTR is that product
  r <- mapping_rule(of = c("tech", "region"), cols = c("tech", "region"))
  expect_equal(as_set(materialise_rule(m, r)), as_set(m$mappings$mTR$data))
  # a model mapping as a leaf, joined with an embedded table
  grp <- data.frame(region = unique(m$mappings$mTR$data$region), stringsAsFactors = FALSE)
  grp <- grp[1, , drop = FALSE]
  r2 <- mapping_rule(of = "mTR", gate = "grp", tables = list(grp = grp), cols = c("tech", "region"))
  want <- m$mappings$mTR$data[m$mappings$mTR$data$region == grp$region[1], ]
  expect_equal(as_set(materialise_rule(m, r2)), as_set(want))
  # a rule of a rule
  m$mappings$mTRr <- new_mapping("mTRr", dims = c("tech", "region"), rule = r)
  r3 <- mapping_rule(of = "mTRr", gate = "grp", tables = list(grp = grp), cols = c("tech", "region"))
  expect_equal(as_set(materialise_rule(m, r3)), as_set(want))
})

test_that("new_mapping() carries a rule; validate() checks it against the dims", {
  r <- mapping_rule(of = c("pf", "fs"), tables = list(pf = pf, fs = fs))
  mp <- new_mapping("mTS", desc = "tech x slice", dims = c("tech", "slice"), rule = r)
  expect_null(mp$data)
  expect_identical(mp$rule$cols, c("tech", "slice"))
  expect_true(validate(mp, stop_on_error = FALSE)$valid)
  # a plain mapping has no rule field at all
  expect_false("rule" %in% names(new_mapping("mA", dims = "tech", data = data.frame(tech = "coal"))))
  # cols at odds with the dims
  wrong <- mapping_rule(of = c("pf", "fs"), tables = list(pf = pf, fs = fs), cols = c("slice", "tech"))
  expect_error(new_mapping("mTS", dims = c("tech", "slice"), rule = wrong), "must equal the dimension names")
  expect_error(new_mapping("mTS", dims = c("tech", "slice"), rule = list(of = "pf")), "mapping_rule")
})

test_that("get_data() and the writers' loader enumerate a rule; trim never drops one", {
  m <- example_models$toy
  m$mappings$mTR <- new_mapping("mTR", desc = m$mappings$mTR$desc, dims = c("tech", "region"),
                                rule = mapping_rule(of = c("tech", "region")))
  want <- as_set(example_models$toy$mappings$mTR$data)
  expect_equal(as_set(get_data(m, "mTR", type = "mapping")), want)
  expect_equal(as_set(.mapping_data(m, "mTR")), want)
  # the loader fills rules for the text writers, unless told not to
  expect_equal(as_set(load_model_data(m)$mappings$mTR$data), want)
  expect_null(load_model_data(m, rules = FALSE)$mappings$mTR$data)
  t <- trim_model(m, verbose = FALSE)
  expect_false(isTRUE(t$mappings$mTR$trimmed))
  expect_false(isTRUE(t$variables$vCap$trimmed))
})

test_that("a rule is written beside the mapping and read back", {
  m <- example_models$toy
  m$mappings$mTR <- new_mapping("mTR", dims = c("tech", "region"),
                                rule = mapping_rule(of = c("tech", "region")))
  dir <- tempfile("rule-ws-")
  saved <- save_model(m, dir, verbose = FALSE)
  f <- file.path(dir, "mappings", "mTR", "rule.json")
  expect_true(file.exists(f))
  expect_false(file.exists(file.path(dir, "mappings", "mTR", "data.parquet")))
  back <- .rule_read(f)
  expect_identical(back$hash, m$mappings$mTR$rule$hash)
  loaded <- load_model(dir, verbose = FALSE)
  expect_identical(loaded$mappings$mTR$rule$hash, m$mappings$mTR$rule$hash)
  expect_equal(as_set(get_data(loaded, "mTR", type = "mapping")),
               as_set(example_models$toy$mappings$mTR$data))
  # with embedded tables
  r <- mapping_rule(of = c("pf", "fs"), tables = list(pf = pf, fs = fs), cols = c("tech", "slice"))
  f2 <- tempfile(fileext = ".json")
  .rule_write(r, f2)
  expect_identical(.rule_read(f2)$hash, r$hash)
  expect_equal(.rule_read(f2)$tables$fs, unique(fs))
})

test_that("coded enumeration keeps hidden labels out of the build dictionary", {
  r <- mapping_rule(of = c("pf", "fs"), tables = list(pf = pf, fs = fs), cols = c("tech", "slice"))
  codes <- .mx_new_codes()
  out <- rule_tuples_coded(list(), r, codes)
  expect_true(is.integer(out$tech) && is.integer(out$slice))
  expect_true(isTRUE(attr(out$tech, "mx_coded")))
  expect_false(any(c("ANNUAL", "HOUR") %in% codes$levels))
  dec <- data.frame(tech = codes$levels[out$tech], slice = codes$levels[out$slice])
  expect_equal(as_set(dec), as_set(want_ts))
  # cached on the dictionary by content
  expect_identical(rule_tuples_coded(list(), r, codes), out)
})

test_that("a rule-defined domain gives the same LP up to a permutation, on both engines", {
  skip_if_not_installed("highs")
  base <- example_models$toy
  m <- base
  m$mappings$mTR <- new_mapping("mTR", desc = base$mappings$mTR$desc, dims = c("tech", "region"),
                                rule = mapping_rule(of = c("tech", "region")))
  want <- lp_signature(model_to_lp(base, verbose = FALSE))
  got <- lp_signature(model_to_lp(m, verbose = FALSE))
  expect_equal(got, want)
  nat <- withr::with_options(list(multimod.engine = "native"),
                             lp_signature(model_to_lp(m, verbose = FALSE)))
  expect_equal(nat, want)
  expect_equal(solve_highs(m, attach = FALSE)$objective, 198000, tolerance = 1e-9)

  # the streamed route: coded index tables from the rule, same optimum, a
  # decodable bundle
  dir <- tempfile("rule-mps-"); dir.create(dir)
  f <- file.path(dir, "m.mps")
  withr::with_options(list(multimod.engine = "native"), write_mps(m, f, stream = TRUE))
  r <- solve_mps(f)
  expect_equal(r$objective, 198000, tolerance = 1e-9)
  sol <- read_mps_solution(r$sol_file, index_dir = dir)
  expect_equal(nrow(sol$primal), nrow(model_to_lp(base, verbose = FALSE)$col_index))
  expect_false(anyNA(sol$primal$value))
})

test_that("a rule with hidden join columns reproduces the energyRt model's LP and optimum", {
  skip_on_cran()
  skip_if_not_installed("highs")
  base <- example_models$energyRt$multimod
  # meqTradeCapFlow(trade, comm, year, timeslice) is the join of the balance
  # rows and the inter-regional trade columns on (comm, year, timeslice); the
  # regions and the route ends are hidden, and the projection is not
  # one-to-one, so the enumeration must be made unique
  m <- base
  m$mappings$meqTradeCapFlow <- new_mapping(
    "meqTradeCapFlow", desc = base$mappings$meqTradeCapFlow$desc,
    dims = c("trade", "comm", "year", "timeslice"),
    rule = mapping_rule(of = c("meqBalLo", "mvTradeIr")))
  expect_equal(as_set(get_data(m, "meqTradeCapFlow", type = "mapping")),
               as_set(base$mappings$meqTradeCapFlow$data))
  want <- lp_signature(model_to_lp(base, verbose = FALSE))
  got <- lp_signature(model_to_lp(m, verbose = FALSE))
  expect_equal(got, want)
  nat <- withr::with_options(list(multimod.engine = "native"),
                             lp_signature(model_to_lp(m, verbose = FALSE)))
  expect_equal(nat, want)
  dir <- tempfile("rule-utopia-"); dir.create(dir)
  f <- file.path(dir, "u.mps")
  withr::with_options(list(multimod.engine = "native"),
                      write_mps(m, f, stream = TRUE, presolve = TRUE))
  r <- solve_mps(f)
  expect_equal(r$objective, 46398.4234209742, tolerance = 1e-9)
  sol <- read_mps_solution(r$sol_file, index_dir = dir)
  expect_equal(nrow(sol$primal), nrow(model_to_lp(base, verbose = FALSE)$col_index))
  expect_false(anyNA(sol$primal$value))
})
