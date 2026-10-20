# =============================================================================
# JuMP writer: symbols registered as both a set and a mapping
# =============================================================================
#
# A subset declaration (`sup(process)`) registers its name in BOTH model$sets
# and model$mappings, so bare and parenthesised usage each resolve - see
# test-read-gams-subsets.R. The store writes the members on one side only and
# leaves the other directory empty, so emitting both loaders in data.jl lets
# the second assignment win and `_load_mapping_file()` returns an empty Set()
# for a name whose members live under sets/.
#
# Symptom when this regresses: every `sum(... for h in tech if ...)` yields no
# terms, eqCost carries one nonzero per row, and the objective solves to 0
# while the matrix route on the same model returns 46398.423421.
# =============================================================================

test_that("write_jump emits one loader per dual-registered symbol", {
  skip_on_cran()
  skip_if_not_installed("arrow")

  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod

  dual <- intersect(names(model$sets), names(model$mappings))
  expect_gt(length(dual), 0)

  model_dir <- tempfile("jump_subsets_")
  dir.create(model_dir, recursive = TRUE)
  on.exit(unlink(model_dir, recursive = TRUE), add = TRUE)

  save_model(model, model_dir, format = "ipc", verbose = FALSE)
  write_jump(model, model_dir = model_dir)

  data_jl <- readLines(file.path(model_dir, "solvers", "jump", "data.jl"),
                       warn = FALSE)

  for (nm in dual) {
    assigns <- grep(paste0("^", nm, " = _load_"), data_jl, value = TRUE)
    expect_gte(length(assigns), 1L)

    # Assignments run in emission order, so the LAST one decides the value.
    # It has to name the side that holds the members; a loader pointed at the
    # empty directory returns an empty collection and blanks the symbol.
    has_set <- length(Sys.glob(file.path(model_dir, "sets", nm, "data.*"))) > 0
    has_map <- length(Sys.glob(file.path(model_dir, "mappings", nm, "data.*"))) > 0
    if (has_set && !has_map) {
      expect_match(utils::tail(assigns, 1), "_load_set_file", info = nm)
    } else if (has_map && !has_set) {
      expect_match(utils::tail(assigns, 1), "_load_mapping_file", info = nm)
    }
  }
})

test_that("process subsets keep their members in generated data.jl", {
  skip_on_cran()
  skip_if_not_installed("arrow")

  data(example_models, package = "multimod")
  model <- example_models$energyRt$multimod

  model_dir <- tempfile("jump_members_")
  dir.create(model_dir, recursive = TRUE)
  on.exit(unlink(model_dir, recursive = TRUE), add = TRUE)

  save_model(model, model_dir, format = "ipc", verbose = FALSE)
  write_jump(model, model_dir = model_dir)

  data_jl <- readLines(file.path(model_dir, "solvers", "jump", "data.jl"),
                       warn = FALSE)

  for (nm in c("sup", "dem", "tech", "stg", "trade", "expp", "imp")) {
    skip_if_not(length(Sys.glob(file.path(model_dir, "sets", nm, "data.*"))) > 0)
    assigns <- grep(paste0("^", nm, " = _load_"), data_jl, value = TRUE)
    expect_length(assigns, 1L)
    expect_match(assigns, "_load_set_file", info = nm)
  }
})
