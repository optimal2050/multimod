# GAMS data export via GDX ----------------------------------------------------
#
# Status: experimental. Exercised against an older GDX library and gdxtools
# 0.7, with the solve run on NEOS; not tested with the current gdxtools.
#
# write_gams() emits declarations and equations; the values travel in a GDX.
# The GDX library ships with GAMS and needs no licence, so a GDX can be written
# against an unlicensed installation and solved elsewhere.

#' Symbol tables for a GDX
#'
#' Sets and mappings become one data frame of key columns; parameters gain a
#' trailing `value` column. Empty symbols are included as zero-row frames,
#' because a symbol that is declared and never assigned fails a GAMS solve.
#'
#' @param model A `multimod` model with data attached.
#' @param use_folded Logical; prefer folded data where a parameter carries it.
#'
#' @return A list with `sets` and `params`, each a named list of data frames.
#' @keywords internal
gams_gdx_tables <- function(model, use_folded = FALSE) {
  model <- load_model_data(model)
  empty_frame <- function(cols, value = FALSE) {
    if (length(cols) == 0 && !value) return(NULL)
    df <- as.data.frame(
      stats::setNames(rep(list(character(0)), length(cols)), cols),
      stringsAsFactors = FALSE)
    if (value) df$value <- numeric(0)
    df
  }

  # A subset declaration is one symbol on both sides and only one side holds
  # the members, so each is taken from the side that has them. Writing the
  # empty side instead leaves a gate that never fires.
  map_side_has_data <- function(nm) {
    d <- .mapping_data(model, nm)
    !is.null(d) && nrow(d) > 0
  }

  sets <- list()
  for (nm in names(model$sets %||% list())) {
    members <- model$sets[[nm]]$data
    if ((is.null(members) || length(members) == 0) && map_side_has_data(nm)) next
    df <- if (is.null(members) || length(members) == 0) {
      empty_frame(nm)
    } else {
      stats::setNames(data.frame(as.character(members), stringsAsFactors = FALSE), nm)
    }
    if (!is.null(df)) sets[[nm]] <- df
  }

  # A subset declaration is one symbol registered on both sides; its members
  # are carried above.
  for (nm in names(model$mappings %||% list())) {
    if (nm %in% names(sets)) next
    d <- .mapping_data(model, nm)
    if (is.null(d) || nrow(d) == 0) {
      df <- empty_frame(get_dim_names(model$mappings[[nm]]$dims))
      if (!is.null(df)) sets[[nm]] <- df
      next
    }
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    d <- d[, setdiff(names(d), "value"), drop = FALSE]
    for (j in seq_along(d)) d[[j]] <- as.character(d[[j]])
    sets[[nm]] <- d
  }

  params <- list()
  for (nm in names(model$parameters %||% list())) {
    p <- model$parameters[[nm]]
    d <- if (isTRUE(use_folded) && !is.null(p$folded_data)) p$folded_data else p$data
    if (is.null(d) || nrow(d) == 0) {
      df <- empty_frame(get_dim_names(p$dims), value = TRUE)
      if (!is.null(df)) params[[nm]] <- df
      next
    }
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    if (!"value" %in% names(d)) next
    keys <- setdiff(names(d), c("value", "n_original"))
    out <- d[, c(keys, "value"), drop = FALSE]
    for (j in keys) out[[j]] <- as.character(out[[j]])
    out$value <- as.numeric(out$value)
    params[[nm]] <- out
  }

  list(sets = sets, params = params)
}

#' GDX symbol records for gdxtools
#'
#' Builds the list `wgdx()` takes. Written here rather than via
#' `gdxtools::write.gdx()` because that drops zero-valued parameter records,
#' and those are needed as EPS.
#'
#' @param tbl Output of [gams_gdx_tables()].
#' @return A list of GDX symbol records.
#' @keywords internal
gams_gdx_records <- function(tbl) {
  record <- function(nm, df, type) {
    n_key <- if (type == "parameter") ncol(df) - 1L else ncol(df)
    if (n_key == 0L) {
      return(list(name = nm, type = type, dim = 0L,
                  val = matrix(as.numeric(df$value), nrow = 1L),
                  uels = list(), domains = character(0), ts = ""))
    }
    uels <- vector("list", n_key)
    val <- matrix(0, nrow = nrow(df), ncol = if (type == "parameter") n_key + 1L else n_key)
    for (j in seq_len(n_key)) {
      f <- factor(df[[j]])
      uels[[j]] <- levels(f)
      val[, j] <- as.numeric(f)
    }
    if (type == "parameter") val[, n_key + 1L] <- as.numeric(df$value)
    list(name = nm, type = type, dim = n_key, val = val, uels = uels,
         domains = rep("*", n_key), ts = "")
  }

  c(lapply(names(tbl$sets), function(nm) record(nm, tbl$sets[[nm]], "set")),
    lapply(names(tbl$params), function(nm) record(nm, tbl$params[[nm]], "parameter")))
}

#' Write a model's data to a GDX for GAMS
#'
#' Companion to [write_gams()]: the generated model declares the symbols and
#' `$load`s them from this file. Pass the same `use_folded` to both.
#'
#' Needs the `gdxtools` package and a GAMS installation for the GDX library.
#' Writing a GDX does not require a GAMS licence.
#'
#' Zero-valued parameter records are written as GAMS `EPS`. A GDX stores no
#' zeros for parameters, so a stored zero would otherwise be indistinguishable
#' from an absent one and would be overwritten by the default that
#' [write_gams()] assigns after loading.
#'
#' @section Experimental:
#' This function is experimental and is not covered by the package tests. It
#' was exercised against an older GDX library and gdxtools 0.7, with the solve
#' run on NEOS; it has not been tested with the current gdxtools on CRAN.
#' Its interface may change.
#'
#' @param model A `multimod` model with data attached.
#' @param file Path of the GDX to write. NEOS decodes its input GDX to
#'   `in.gdx`.
#' @param use_folded Logical; prefer folded data where a parameter carries it.
#' @param gams_dir GAMS system directory for the GDX library. `NULL` lets
#'   `gdxtools::igdx()` find one.
#' @param verbose Logical; report what was written.
#'
#' @return Invisibly, the character vector of symbol names written.
#' @seealso [write_gams()]
#' @export
write_gams_data <- function(model, file, use_folded = FALSE, gams_dir = NULL,
                            verbose = FALSE) {
  if (!requireNamespace("gdxtools", quietly = TRUE)) {
    stop("Writing a GDX needs the 'gdxtools' package.\n",
         '  install.packages("gdxtools")', call. = FALSE)
  }
  loaded <- tryCatch({
    if (is.null(gams_dir)) gdxtools::igdx(silent = TRUE) else
      gdxtools::igdx(gams_dir, silent = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(loaded)) {
    stop("Could not load the GAMS GDX library. Point 'gams_dir' at a GAMS ",
         "installation; writing a GDX needs no licence.", call. = FALSE)
  }

  tbl <- gams_gdx_tables(model, use_folded = use_folded)
  if (length(tbl$sets) == 0 && length(tbl$params) == 0) {
    stop("Model carries no data to write.", call. = FALSE)
  }

  wgdx <- get("wgdx", envir = asNamespace("gdxtools"))
  wgdx(file, gams_gdx_records(tbl), squeeze = "e")

  written <- c(names(tbl$sets), names(tbl$params))
  if (verbose) {
    message("Wrote ", basename(file), ": ", length(tbl$sets), " sets/mappings, ",
            length(tbl$params), " parameters")
  }
  invisible(written)
}
