# =============================================================================
# Building a multimod model from an energyRt scenario
# =============================================================================
#
# The two halves come from different places, deliberately:
#
#   structure  energyRt's GAMS template (gams/energyRt.gms) - the source of
#              truth. The GMPL / JuMP / Pyomo templates are generated and are
#              still being edited, so GAMS is the one to read.
#   data       the interpolated scenario you pass in.
#
# These functions used to live in `data-raw/build_energyRt_fixture.R`, which is
# `.Rbuildignore`d - so an *installed* multimod could not reproduce the
# pipeline at all, and `fill_variable_domains()` is not optional: without it 24
# of 93 variables get no domain and therefore contribute no columns.
#
# energyRt is a Suggests, not an Imports: multimod renders models for several
# backends and only this file needs it. Every entry point checks for it.
# =============================================================================

#' Fail with an actionable message when energyRt is absent
#' @keywords internal
#' @noRd
.need_energyRt <- function(what) {
  if (!requireNamespace("energyRt", quietly = TRUE)) {
    stop(what, " needs the 'energyRt' package.\n",
         '  pak::pak("optimal2050/energyRt")', call. = FALSE)
  }
  invisible(TRUE)
}

#' Variable -> gating-map table from energyRt's mapping spec
#'
#' `.mapping_spec` is internal energyRt data. It is read through
#' `getFromNamespace()` rather than `:::` so the dependency is explicit and
#' fails with a clear message if energyRt's internals move.
#'
#' @return A named list: variable name -> character vector of mapping names.
#' @keywords internal
#' @noRd
.energyrt_gates_var <- function() {
  .need_energyRt("Resolving variable domains")
  spec <- tryCatch(
    utils::getFromNamespace(".mapping_spec", "energyRt"),
    error = function(e) NULL
  )
  if (is.null(spec)) {
    stop("energyRt no longer exposes the internal `.mapping_spec` object, ",
         "which carries the authoritative variable -> gating-map table.\n",
         "  Without it 24 of 93 variables have no domain and would silently ",
         "contribute no columns.", call. = FALSE)
  }
  inv <- list()
  for (nm in names(spec)) {
    g <- spec[[nm]]$gates_var
    if (length(g)) for (v in g[nzchar(g)]) inv[[v]] <- c(inv[[v]], nm)
  }
  inv
}

#' Name of a symbol's domain, whichever form it takes
#'
#' `variable$domain` is a character (mapping name); `equation$domain` is an
#' `ast_mapping` node. Both occur.
#' @keywords internal
#' @noRd
.dom_name <- function(d) {
  if (is.null(d)) return(NA_character_)
  if (is.character(d)) return(if (length(d)) d[1] else NA_character_)
  if (!is.null(d$name)) return(d$name)
  NA_character_
}

#' Fill in variable domains that the GAMS comments do not carry
#'
#' `en_extract_domains_from_comments()` reads `*@ mXxx(...)` hints out of the
#' GAMS template. As of energyRt 0.89.5 there are hints for 70 of 93 variables,
#' so the rest (phase-out / retirement / storage auxiliary capacity) would get
#' no domain. energyRt's own `.mapping_spec` carries the authoritative table and
#' covers 92 of 93 (`vObjective` is a genuine scalar).
#'
#' @param model A multimod model.
#' @param prefer Where the hint and the spec disagree: `"spec"` trusts
#'   `.mapping_spec`, `"hint"` keeps the GAMS comment. Three variables differ
#'   (`vTechStockCap`, `vStorageInp`, `vStorageOut`). `"spec"` is the default
#'   because it is the choice that reproduces GLPK's column count (2,318 on the
#'   R1 kit, against 2,295 for `"hint"`) - the three hints are stale.
#' @param scalars Variables that legitimately have no domain.
#' @param verbose Logical; report what was filled and what conflicted.
#'
#' @return The model, with every variable carrying a domain.
#' @export
fill_variable_domains <- function(model, prefer = c("spec", "hint"),
                                  scalars = "vObjective", verbose = TRUE) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  prefer <- match.arg(prefer)
  inv <- .energyrt_gates_var()

  filled <- conflicts <- unresolved <- character()
  for (v in names(model$variables)) {
    cur <- .dom_name(model$variables[[v]]$domain)
    spec <- inv[[v]]

    if (is.na(cur)) {
      if (v %in% scalars) next                       # genuinely scalar
      if (is.null(spec)) { unresolved <- c(unresolved, v); next }
      model$variables[[v]]$domain <- spec[1]
      filled <- c(filled, v)
    } else if (!is.null(spec) && !(cur %in% spec)) {
      conflicts <- c(conflicts, sprintf("%s (hint=%s, spec=%s)", v, cur, spec[1]))
      if (prefer == "spec") model$variables[[v]]$domain <- spec[1]
    }
  }

  if (verbose) {
    message("fill_variable_domains(prefer = '", prefer, "'):")
    message("  filled from .mapping_spec: ", length(filled))
    if (length(conflicts)) {
      message("  hint/spec conflicts (", prefer, " wins): ", length(conflicts))
      for (x in conflicts) message("    ", x)
    }
    if (length(unresolved)) {
      message("  UNRESOLVED: ", paste(unresolved, collapse = ", "))
    }
  }

  # A variable with no domain contributes no columns, so this must stop rather
  # than warn: the model would build, solve, and be missing a whole block.
  if (length(unresolved)) {
    stop("No domain map for variable(s): ", paste(unresolved, collapse = ", "),
         ".\n  Add a '*@ mXxx(...)' hint in energyRt/gams/energyRt.gms, or a ",
         "gates_var entry in energyRt's mapping spec.", call. = FALSE)
  }
  model
}

#' Path to energyRt's GAMS template
#'
#' @return The installed template path, or `NULL` when it cannot be found.
#' @keywords internal
#' @noRd
.energyrt_gms <- function() {
  .need_energyRt("Reading the energyRt model")
  for (p in c(system.file("gams", "energyRt.gms", package = "energyRt"),
              system.file("energyRt.gms", package = "energyRt"))) {
    if (nzchar(p) && file.exists(p)) return(p)
  }
  NULL
}

#' energyRt's GAMS model code, as shipped
#'
#' energyRt `.Rbuildignore`s its `gams/` directory, so `system.file()` finds
#' nothing in an *installed* energyRt and [.energyrt_gms()] returns NULL - which
#' meant `gms = NULL` could never resolve and every caller had to point at a
#' source checkout.
#'
#' The code is shipped, though: `.modelCode$GAMS` in energyRt's `R/sysdata.rda`
#' carries it as a character vector of ~3,000 lines (alongside the JuMP, Pyomo
#' and GLPK templates). Reading that is strictly better than a file lookup - it
#' needs no checkout, and it ties the AST to the *installed* energyRt version
#' rather than to whichever of the nine `.gms` files in `gams/` happened to be
#' on disk. (The two differ: 0.90.0.9000 ships 2,954 lines where the working
#' tree's `gams/energyRt.gms` had 2,979.)
#'
#' Read through `getFromNamespace()` rather than `:::`, as `.mapping_spec` is,
#' so the dependency is explicit and fails with a clear message if it moves.
#'
#' @return Character vector of GAMS source lines, or `NULL`.
#' @keywords internal
#' @noRd
.energyrt_model_code <- function() {
  .need_energyRt("Reading the energyRt model")
  mc <- tryCatch(utils::getFromNamespace(".modelCode", "energyRt"),
                 error = function(e) NULL)
  if (is.null(mc) || is.null(mc$GAMS) || !length(mc$GAMS)) return(NULL)
  mc$GAMS
}

#' Build a multimod model from an interpolated energyRt scenario
#'
#' Structure is read from energyRt's GAMS template; data comes from the
#' scenario. This is the entry point for the direct-matrix / MPS route -
#' everything downstream ([model_to_lp()], [write_mps()], [solve_highs()])
#' takes the model this returns.
#'
#' @param scen An interpolated energyRt scenario.
#' @param gms The GAMS model source. `NULL` (the default) reads the code
#'   energyRt ships in `.modelCode$GAMS`, which needs no source checkout and
#'   matches the installed energyRt version. Pass a path to read a working
#'   tree instead, or a character vector of GAMS source directly.
#' @param prefer Passed to [fill_variable_domains()].
#' @param inMemory Load the scenario's data into memory. Must be `TRUE` for a
#'   folded scenario (the lazy path cannot expand wildcards) - see
#'   [import_energyRt_data()].
#' @param verbose Logical; report progress.
#'
#' @return A multimod model with data attached, ready for [model_to_lp()].
#'
#' @examples
#' \dontrun{
#' scen <- energyRt::interpolate_model(mod, name = "BASE")
#' m <- multimod_from_energyRt(scen)
#' lp <- model_to_lp(m)
#' write_mps(m, "model.mps", lp = lp)
#' }
#' @export
multimod_from_energyRt <- function(scen, gms = NULL, prefer = c("spec", "hint"),
                                   inMemory = TRUE, verbose = TRUE) {
  .need_energyRt("multimod_from_energyRt()")
  if (!inherits(scen, "scenario")) {
    stop("`scen` must be an energyRt scenario object.", call. = FALSE)
  }
  prefer <- match.arg(prefer)

  # Where the equations come from. The default is the code energyRt SHIPS
  # (.modelCode$GAMS); a path or a character vector is honoured if given,
  # and a source checkout is the last resort.
  if (is.null(gms)) {
    code <- .energyrt_model_code()
    src <- sprintf("energyRt::.modelCode$GAMS (energyRt %s)",
                   utils::packageVersion("energyRt"))
    if (is.null(code)) {
      code <- .energyrt_gms()
      src <- code
    }
    if (is.null(code)) {
      stop("energyRt ships its model code as `.modelCode$GAMS`, which this ",
           "energyRt does not expose, and no `gams/energyRt.gms` was found ",
           "either.\n  Pass `gms = ` explicitly (a path, or a character ",
           "vector of GAMS source).", call. = FALSE)
    }
  } else if (length(gms) == 1L && file.exists(gms)) {
    code <- gms                       # read_gams() takes a path
    src <- gms
  } else if (is.character(gms) && length(gms) > 1L) {
    code <- gms                       # ... or the source itself
    src <- "<character vector>"
  } else {
    stop("`gms` must be a path to a .gms file or a character vector of GAMS ",
         "source; got ", class(gms)[1], " of length ", length(gms), ".",
         call. = FALSE)
  }

  ms <- read_gams(code, include = FALSE)
  ms <- en_extract_domains_from_comments(ms)
  ms <- populate_defvals_from_energyrt(ms)
  m <- as_multimod(ms)
  m <- fill_variable_domains(m, prefer = prefer, verbose = verbose)

  m <- import_energyRt_data(m, scen, inMemory = inMemory)
  m <- suppressWarnings(
    add_index_aliases(m, index_aliases_energyRt, overwrite = TRUE))

  # Constraints and cost terms added with energyRt's newConstraint() /
  # newCosts() are NOT in the GAMS template: interpolation compiles them to a
  # GAMS string IR on `modInp@user_constraints`, per scenario. Omitting this
  # step produced a model of exactly the right shape with those rows missing -
  # the silent-wrong-answer failure this package keeps guarding against.
  m <- add_user_constraints(m, scen, verbose = verbose)

  attr(m, "energyRt_version") <- as.character(utils::packageVersion("energyRt"))
  attr(m, "energyRt_gms") <- src
  m
}
