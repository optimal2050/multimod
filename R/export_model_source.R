#' Export Model Source Files
#'
#' @description
#' Export model source code from the bundled `example_models` dataset to files
#' in a specified directory. This is useful for testing parsers, running
#' models, or inspecting model code.
#'
#' @param dataset Name of the dataset to export. Currently only "energyrt".
#' @param dir Directory path where files will be written. Will be created if it
#'   doesn't exist.
#' @param format Which format to export: "gams", "gmpl", "jump", "pyomo", or
#'   "all" (default).
#' @param overwrite Logical. If TRUE, overwrite existing files. Default is FALSE.
#' @param verbose Logical. If TRUE, print progress messages. Default is TRUE.
#'
#' @return Invisible list of file paths that were created.
#'
#' @details
#' ## energyRt Export
#'
#' For the energyRt dataset, exports files based on the `format` argument:
#'
#' - **GAMS** (`format = "gams"`):
#'   - `model.gms`: Model code with all includes resolved
#'   - `data.gms`: Data assignments with all includes resolved
#'   - `README.txt`: Model metadata and information
#'
#' - **GMPL** (`format = "gmpl"`):
#'   - `model.mod`: Model declarations
#'   - `data.dat`: Data statements
#'   - `README.txt`: Model metadata and information
#'
#' - **JuMP** (`format = "jump"`):
#'   - `model.jl`: JuMP model code with includes resolved
#'   - `data.jl`: Pure Julia data code (no RData dependency)
#'   - `README.txt`: Model metadata and information
#'
#' - **Pyomo** (`format = "pyomo"`):
#'   - `model.py`: Pyomo model code
#'   - `data.py`: Python data code with exec() resolved
#'   - `README.txt`: Model metadata and information
#'
#' - **All** (`format = "all"`):
#'   - Exports all formats to subdirectories: gams/, gmpl/, jump/, pyomo/
#'
#' @examples
#' \dontrun{
#' tmp_dir <- tempdir()
#'
#' # Export energyRt GMPL format
#' export_model_source("energyrt", file.path(tmp_dir, "energyrt"),
#'                     format = "gmpl")
#'
#' # Export all energyRt formats
#' export_model_source("energyrt", file.path(tmp_dir, "energyrt_all"),
#'                     format = "all")
#'
#' # Use exported files
#' gmpl_dir <- file.path(tmp_dir, "energyrt")
#' model <- read_gmpl(file.path(gmpl_dir, "model.mod"),
#'                    file.path(gmpl_dir, "data.dat"))
#' }
#'
#' @export
export_model_source <- function(dataset = c("energyrt"),
                                 dir,
                                 format = "all",
                                 overwrite = FALSE,
                                 verbose = TRUE) {

  dataset <- match.arg(dataset)

  # Create directory if needed
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    if (verbose) message("Created directory: ", dir)
  }

  # Track created files
  files_created <- list()

  # Export based on dataset
  if (dataset == "energyrt") {
    format <- match.arg(format, c("gams", "gmpl", "jump", "pyomo", "all"))
    files_created <- export_energyrt(dir, format, overwrite, verbose)
  }

  if (verbose) {
    message("\nExport complete! ", length(files_created), " file(s) written to:")
    message("  ", normalizePath(dir, mustWork = FALSE))
  }

  invisible(files_created)
}

#' @keywords internal
export_energyrt <- function(dir, format, overwrite, verbose) {
  # Load dataset into local environment
  env <- environment()
  utils::data("example_models", envir = env)
  energyRt_source <- get("example_models", envir = env)$energyRt
  dataset <- energyRt_source

  if (format == "all") {
    # Export all formats to subdirectories
    files <- character()
    for (fmt in c("gams", "gmpl", "jump", "pyomo")) {
      subdir <- file.path(dir, fmt)
      if (!dir.exists(subdir)) {
        dir.create(subdir, recursive = TRUE)
      }
      files <- c(files, export_energyrt_format(dataset, subdir,
                                                fmt, overwrite, verbose))
    }
    return(files)
  } else {
    # Export single format
    return(export_energyrt_format(dataset, dir, format,
                                  overwrite, verbose))
  }
}

#' @keywords internal
export_energyrt_format <- function(energyRt_source, dir, format, 
                                   overwrite, verbose) {
  files <- character()
  
  # Create output subdirectory (needed by all models)
  output_dir <- file.path(dir, "output")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Write README with metadata
  readme_file <- file.path(dir, "README.txt")
  if (!file.exists(readme_file) || overwrite) {
    # Format-specific usage notes
    usage_note <- switch(format,
      "gmpl" = paste(
        "Usage:",
        "  1. Create output directory: mkdir output",
        "  2. Run model: glpsol -m model.mod -d data.dat",
        "",
        sep = "\n  "
      ),
      "jump" = paste(
        "Usage:",
        "  julia model.jl",
        "  (Output directory is created automatically)",
        "",
        sep = "\n  "
      ),
      "pyomo" = paste(
        "Usage:",
        "  python model.py",
        "",
        "Note: This export contains fully resolved inline data (no exec() calls",
        "or SQLite database dependencies). However, some constraint rules may",
        "fail with 'Invalid constraint expression' errors when parameters are",
        "missing. This is a known issue in the energyRt Pyomo writer that",
        "needs to be fixed upstream.",
        "",
        sep = "\n  "
      ),
      ""  # Default: no usage note
    )
    
    readme_content <- c(
      "energyRt Model Source Files",
      paste0("Format: ", toupper(format)),
      "",
      "Source:",
      paste0("  Package: ", energyRt_source$metadata$source),
      paste0("  Scenario: ", energyRt_source$metadata$scenario),
      paste0("  Repository: ", energyRt_source$metadata$repository),
      "",
      "License:",
      paste0("  ", energyRt_source$metadata$license),
      "  GNU Affero General Public License v3.0",
      "  https://www.gnu.org/licenses/agpl-3.0.html",
      "",
      "Date:",
      paste0("  Imported: ", energyRt_source$metadata$date_imported),
      paste0("  Exported: ", Sys.Date()),
      "",
      "Files:",
      paste0("  model.", get_extension(format), " - Model declarations and equations"),
      paste0("  data.", get_extension(format), " - Parameter values and data"),
      if (nchar(usage_note) > 0) c("", usage_note) else NULL,
      "",
      "Note:",
      "  ", energyRt_source$metadata$note
    )
    writeLines(readme_content, readme_file)
    files <- c(files, readme_file)
    if (verbose) message("Wrote: README.txt")
  }
  
  if (format == "gams") {
    # GAMS format - uniform names
    model_file <- file.path(dir, "model.gms")
    data_file <- file.path(dir, "data.gms")
    
    if (!file.exists(model_file) || overwrite) {
      writeLines(energyRt_source$gams$model, model_file)
      files <- c(files, model_file)
      if (verbose) message("Wrote: model.gms (", 
                          length(energyRt_source$gams$model), " lines)")
    }
    
    if (!file.exists(data_file) || overwrite) {
      writeLines(energyRt_source$gams$data, data_file)
      files <- c(files, data_file)
      if (verbose) message("Wrote: data.gms (", 
                          length(energyRt_source$gams$data), " lines)")
    }
    
  } else if (format == "gmpl") {
    # GMPL format - uniform names
    model_file <- file.path(dir, "model.mod")
    data_file <- file.path(dir, "data.dat")
    
    if (!file.exists(model_file) || overwrite) {
      writeLines(energyRt_source$gmpl$model, model_file)
      files <- c(files, model_file)
      if (verbose) message("Wrote: model.mod (", 
                          length(energyRt_source$gmpl$model), " lines)")
    }
    
    if (!file.exists(data_file) || overwrite) {
      writeLines(energyRt_source$gmpl$data, data_file)
      files <- c(files, data_file)
      if (verbose) message("Wrote: data.dat (", 
                          length(energyRt_source$gmpl$data), " lines)")
    }
    
  } else if (format == "jump") {
    # JuMP format - uniform names
    model_file <- file.path(dir, "model.jl")
    data_jl_file <- file.path(dir, "data.jl")
    
    if (!file.exists(model_file) || overwrite) {
      writeLines(energyRt_source$jump$model, model_file)
      files <- c(files, model_file)
      if (verbose) message("Wrote: model.jl (", 
                          length(energyRt_source$jump$model), " lines)")
    }
    
    if (!is.null(energyRt_source$jump$data) && 
        (!file.exists(data_jl_file) || overwrite)) {
      writeLines(energyRt_source$jump$data, data_jl_file)
      files <- c(files, data_jl_file)
      if (verbose) message("Wrote: data.jl (", 
                          length(energyRt_source$jump$data), 
                          " lines, pure Julia - no RData needed)")
    }
    
  } else if (format == "pyomo") {
    # Pyomo format - uniform names
    model_file <- file.path(dir, "model.py")
    data_file <- file.path(dir, "data.py")
    
    if (!is.null(energyRt_source$pyomo$model) && 
        (!file.exists(model_file) || overwrite)) {
      writeLines(energyRt_source$pyomo$model, model_file)
      files <- c(files, model_file)
      if (verbose) message("Wrote: model.py (", 
                          length(energyRt_source$pyomo$model), " lines)")
    }
    
    if (!is.null(energyRt_source$pyomo$data) && 
        (!file.exists(data_file) || overwrite)) {
      writeLines(energyRt_source$pyomo$data, data_file)
      files <- c(files, data_file)
      if (verbose) message("Wrote: data.py (", 
                          length(energyRt_source$pyomo$data), " lines)")
    }
  }
  
  return(files)
}

#' @keywords internal
get_extension <- function(format) {
  switch(format,
    gams = "gms",
    gmpl = "mod / dat",
    jump = "jl",
    pyomo = "py",
    "txt"
  )
}
