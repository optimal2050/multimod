## data-raw/build_energyRt_fixture.R
##
## Builds the multimod fixture from the CURRENT energyRt sources.
##
##   structure : energyRt/gams/energyRt.gms  (read with include = FALSE)
##   data      : energyRt::interpolate_model() on a UTOPIA kit
##
## GAMS is the source of truth for structure: the GMPL/JuMP/Pyomo templates are
## still being edited. energyRt itself is never modified by this script.
##
## Usage:
##   pkgload::load_all(".")
##   source("data-raw/build_energyRt_fixture.R")
##   m_R1 <- build_energyRt_fixture("R1")   # fast build loop
##   m_R7 <- build_energyRt_fixture("R7")   # shipped fixture

ENERGYRT_GMS <- "C:/Users/admin/Documents/R/energyRt/gams/energyRt.gms"

## ---------------------------------------------------------------------------
## `energyrt_gates_var()`, `fill_variable_domains()` and
## `multimod_from_energyRt()` now live in the PACKAGE (R/energyrt_pipeline.R),
## exported. They were here while this file was the only caller, but
## data-raw/ is .Rbuildignore'd, so an installed multimod could not reproduce
## the pipeline - and fill_variable_domains() is not optional (without it 24
## of 93 variables get no domain and contribute no columns).
##
## Nothing to define here any more; pkgload::load_all(".") supplies them.
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## Cross-check a multimod model against energyRt's own GLPK run.
##
## Independent oracle: energyRt generates and solves its own GMPL, never
## touching multimod. Compares LP dimensions and (optionally) the objective.
## ---------------------------------------------------------------------------

check_against_energyRt <- function(m, scen, solve = TRUE, verbose = TRUE) {
  dom <- function(d) if (is.null(d)) NA_character_ else if (is.character(d)) d[1] else
         if (!is.null(d$name)) d$name else NA_character_
  nrw <- function(nm) {
    if (is.na(nm) || is.null(m$mappings[[nm]])) return(NA_integer_)
    d <- m$mappings[[nm]]$data
    if (is.null(d)) NA_integer_ else nrow(as.data.frame(d))
  }
  nc <- vapply(vapply(m$variables, function(v) dom(v$domain), ""), nrw, 1L)
  nr <- vapply(vapply(m$equations, function(e) dom(e$domain), ""), nrw, 1L)
  cols <- sum(nc, na.rm = TRUE) + sum(is.na(nc))   # scalars contribute 1
  rows <- sum(nr, na.rm = TRUE) + sum(is.na(nr))

  # Solve where the scenario already lives - resetting the scenarios path here
  # would point at an empty directory and the generated .mod would not be found.
  s <- energyRt::solve_scenario(scen, solver = energyRt::solver_options$glpk,
                                echo = FALSE)
  roots <- unique(c(s@path, scen@path, energyRt::get_scenarios_path()))
  roots <- roots[nzchar(roots) & dir.exists(roots)]
  md <- unlist(lapply(roots, list.dirs, recursive = TRUE))
  md <- md[vapply(md, function(d) length(list.files(d, pattern = "[.]mod$")) > 0, TRUE)]
  if (!length(md)) {
    stop("check_against_energyRt: no generated .mod found under ",
         paste(roots, collapse = ", "))
  }
  st <- get_glpk_model_stats(md[1], verbose = FALSE)
  obj <- energyRt::getData(s, "vObjective", merge = TRUE)$value[1]

  out <- list(
    multimod = list(cols = cols, rows = rows),
    energyRt = list(cols = st$n_cols, rows = st$n_rows, nnz = st$n_nonzeros, objective = obj),
    # GLPK adds one objective row (vObjective2) beyond the equation rows
    cols_match = identical(as.integer(cols), as.integer(st$n_cols)),
    rows_match = identical(as.integer(rows) + 1L, as.integer(st$n_rows))
  )
  if (verbose) {
    message("multimod index : cols=", cols, " rows=", rows)
    message("energyRt GMPL  : cols=", st$n_cols, " rows=", st$n_rows,
            " nnz=", st$n_nonzeros)
    message(sprintf("objective      : %.10f", obj))
    message("cols match: ", out$cols_match, "   rows(+1 obj) match: ", out$rows_match)
  }
  invisible(out)
}

## ---------------------------------------------------------------------------

build_energyRt_fixture <- function(kit = c("R7", "R1", "R3", "R11"),
                                   ## "spec" verified correct: it reproduces the
                                   ## GLPK column count (2,318 for R1) where "hint"
                                   ## gives 2,295. The three *@ hints are stale.
                                   prefer = "spec",
                                   gms = ENERGYRT_GMS,
                                   verbose = TRUE) {
  kit <- match.arg(kit)
  stopifnot(file.exists(gms))

  ## data, from an interpolated energyRt scenario
  energyRt::set_scenarios_path(file.path(tempdir(), "mm_fixture"))
  energyRt::set_registry_file(file.path(tempdir(), "mm_fixture", "registry.csv"))
  utopia <- energyRt::utopia
  um  <- utopia$modules$electricity[[kit]]
  mod <- energyRt::newModel(
    kit,
    data     = um$repo,
    calendar = utopia$modules$calendars$utopia_seasons,
    region   = um$regions,
    horizon  = utopia$modules$horizons$base,
    discount = 0.05
  )
  scen <- energyRt::interpolate_model(mod, name = "BASE")

  m <- multimod_from_energyRt(scen, gms = gms, prefer = prefer, verbose = verbose)
  attr(m, "kit") <- kit
  m
}
