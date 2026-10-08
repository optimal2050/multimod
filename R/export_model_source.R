#' Export the bundled energyRt model as a solver workspace
#'
#' @description
#' Writes the energyRt model shipped in `example_models` to a directory, as a
#' model workspace (see [save_model()]) with one sub-directory per requested
#' backend under `solvers/`. The GAMS source is the file the model was parsed
#' from; the MathProg, JuMP and Pyomo code is rendered from the parsed model
#' with [write_gmpl()], [write_jump()] and [write_pyomo()].
#'
#' @param dataset Name of the dataset to export. Currently only `"energyrt"`.
#' @param dir Directory path for the workspace. Created if it does not exist.
#' @param format Which backend to export: `"gams"`, `"gmpl"`, `"jump"`,
#'   `"pyomo"`, or `"all"` (default).
#' @param overwrite Logical. If `TRUE`, existing solver directories are
#'   rewritten. Default is `FALSE`: a backend whose directory exists is skipped.
#' @param verbose Logical. If `TRUE`, print progress messages. Default is `TRUE`.
#'
#' @return Invisibly, a character vector of the files written.
#'
#' @details
#' The workspace layout is the one every `solve_*()` function expects:
#'
#' - `model.rds`, `sets/`, `mappings/`, `parameters/`: the saved model and its
#'   data (CSV), written once by [save_model()].
#' - `solvers/gams/model.gms`: the GAMS source of the energyRt model.
#' - `solvers/gmpl/model.mod` and `data.dat`: MathProg, run with
#'   `glpsol -m model.mod -d data.dat` from that directory.
#' - `solvers/jump/model.jl` and `data.jl`: JuMP, run with `julia model.jl`.
#' - `solvers/pyomo/model.py` and `data.py`: Pyomo, run with `python model.py`.
#' - `README.txt`: provenance and licence of the model.
#'
#' @examples
#' \donttest{
#' ws <- file.path(tempdir(), "energyrt_ws")
#' files <- export_model_source("energyrt", ws, format = "gmpl")
#' basename(files)
#'
#' model <- read_gmpl(file.path(ws, "solvers", "gmpl", "model.mod"),
#'                    file.path(ws, "solvers", "gmpl", "data.dat"))
#' unlink(ws, recursive = TRUE)
#' }
#'
#' @export
export_model_source <- function(dataset = c("energyrt"),
                                dir,
                                format = "all",
                                overwrite = FALSE,
                                verbose = TRUE) {
  dataset <- match.arg(dataset)
  format <- match.arg(format, c("gams", "gmpl", "jump", "pyomo", "all"))
  formats <- if (format == "all") c("gams", "gmpl", "jump", "pyomo") else format

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    if (verbose) message("Created directory: ", dir)
  }

  files <- character()
  if (dataset == "energyrt") {
    files <- export_energyrt(dir, formats, overwrite, verbose)
  }

  if (verbose) {
    message("Export complete: ", length(files), " file(s) written to ",
            normalizePath(dir, mustWork = FALSE))
  }

  invisible(files)
}

#' @keywords internal
#' @noRd
export_energyrt <- function(dir, formats, overwrite, verbose) {
  env <- environment()
  utils::data("example_models", envir = env)
  source <- get("example_models", envir = env)$energyRt
  model <- source$multimod
  files <- character()

  say <- function(...) if (verbose) message(...)

  # The workspace: data the rendered code reads at run time. Written once;
  # save_model(overwrite = TRUE) would wipe the solver directories with it.
  if (!file.exists(file.path(dir, "model.rds"))) {
    if (length(list.files(dir, all.files = TRUE, no.. = TRUE)) > 0) {
      stop(dir, " is not empty and holds no model workspace (model.rds); ",
           "choose an empty directory.")
    }
    save_model(model, dir, format = "csv", verbose = FALSE)
    files <- c(files, file.path(dir, "model.rds"))
    say("Wrote: model workspace (model.rds, sets/, mappings/, parameters/)")
  }

  readme_file <- file.path(dir, "README.txt")
  if (!file.exists(readme_file) || overwrite) {
    meta <- source$metadata
    writeLines(c(
      "energyRt model workspace exported by multimod::export_model_source()",
      "",
      "Source:",
      paste0("  Package:    ", meta$source),
      paste0("  Scenario:   ", meta$scenario),
      paste0("  Repository: ", meta$repository),
      paste0("  Imported:   ", meta$date_imported),
      paste0("  Exported:   ", Sys.Date()),
      "",
      "License:",
      paste0("  ", meta$license),
      "  https://www.apache.org/licenses/LICENSE-2.0",
      "",
      "Layout:",
      "  model.rds, sets/, mappings/, parameters/  saved model and data (CSV)",
      "  solvers/gams/model.gms                    GAMS source the model was parsed from",
      "  solvers/gmpl/model.mod, data.dat          glpsol -m model.mod -d data.dat",
      "  solvers/jump/model.jl, data.jl            julia model.jl",
      "  solvers/pyomo/model.py, data.py           python model.py",
      "",
      "Note:",
      paste0("  ", meta$note)
    ), readme_file)
    files <- c(files, readme_file)
    say("Wrote: README.txt")
  }

  for (fmt in formats) {
    solver_dir <- file.path(dir, "solvers", fmt)
    if (dir.exists(solver_dir) && !overwrite) {
      say("Skipped ", fmt, ": ", solver_dir, " exists (overwrite = FALSE)")
      next
    }
    dir.create(solver_dir, recursive = TRUE, showWarnings = FALSE)

    written <- switch(fmt,
      gams = {
        f <- file.path(solver_dir, "model.gms")
        writeLines(source$gams$model, f)
        f
      },
      gmpl = {
        out <- suppressMessages(suppressWarnings(write_gmpl(model, model_dir = dir, verbose = FALSE)))
        c(out$model, out$data)
      },
      jump = {
        suppressMessages(suppressWarnings(write_jump(model, model_dir = dir)))
        file.path(solver_dir, c("model.jl", "data.jl"))
      },
      pyomo = {
        suppressMessages(write_pyomo(model, model_dir = dir))
        file.path(solver_dir, c("model.py", "data.py"))
      }
    )
    written <- written[file.exists(written)]
    files <- c(files, written)
    say("Wrote: solvers/", fmt, "/ (", paste(basename(written), collapse = ", "), ")")
  }

  files
}
