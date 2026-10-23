#' Model Workspace and Data Management
#'
#' @description
#' Functions for creating, managing, saving, and loading multimod model workspaces on disk.
#'
#' @section Key Functions:
#' \describe{
#'   \item{\code{save_model()}}{Save complete model workspace to disk with format options (csv/ipc/parquet)}
#'   \item{\code{load_model()}}{Load model from workspace with optional lazy loading}
#'   \item{\code{get_data()}}{Retrieve data with automatic lazy loading from disk}
#'   \item{\code{update_parameter()}}{Low-level function to modify parameter data}
#'   \item{\code{update_mapping()}}{Low-level function to modify mapping data}
#' }
#'
#' @section Workflow:
#' 1. Import or interpolate data into model (creates data.frames in parameters/mappings)
#' 2. Use \code{update_parameter()} to set/modify parameter values
#' 3. Use \code{save_model()} to persist model workspace to disk
#' 4. Use \code{load_model()} to reload model from workspace later
#' 5. Use \code{get_data()} to access data (loads from disk if needed)
#'
#' @section Storage Formats:
#' \itemize{
#'   \item \strong{parquet} - default; zstd level 3. The smallest for model
#'     data (set members and mappings are dictionary-encoded before
#'     compression) and read by every Arrow implementation
#'   \item \strong{ipc} - Arrow IPC/Feather (.arrow extension); faster to
#'     read, several times larger on categorical data
#'   \item \strong{csv} - Universal, human-readable
#' }
#'
#' @name model_workspace
#' @keywords internal
NULL


#' Write the sets and parameters metadata CSV files of a model workspace
#'
#' @param model A multimod model object.
#' @param model_dir Workspace directory; `sets/sets.csv` and
#'   `parameters/parameters.csv` are written under it.
#' @return Invisibly, `NULL`; called for its side effect of writing the files.
#' @keywords internal
.generate_model_metadata_csv <- function(model, model_dir) {
  # Generate sets/sets.csv
  if (!is.null(model$sets) && length(model$sets) > 0) {
    # Filter out trimmed sets
    active_sets <- model$sets[!sapply(model$sets, function(s) isTRUE(s$trimmed))]

    if (length(active_sets) > 0) {
      sets_data <- data.frame(
        name = names(active_sets),
        n_elements = sapply(active_sets, function(s) {
          if (!is.null(s$data)) return(length(s$data))
          if (!is.null(s$members)) return(length(s$members))
          return(0L)
        }),
        desc = sapply(active_sets, function(s) {
          if (!is.null(s$desc) && length(s$desc) > 0) return(s$desc[1])
          return("")
        }),
        stringsAsFactors = FALSE
      )
      sets_dir <- file.path(model_dir, "sets")
      dir.create(sets_dir, showWarnings = FALSE, recursive = TRUE)
      write.csv(sets_data, file.path(sets_dir, "sets.csv"), row.names = FALSE)
    }
  }

  # Generate mappings/mappings.csv
  if (!is.null(model$mappings) && length(model$mappings) > 0) {
    # Filter out trimmed mappings
    active_mappings <- model$mappings[!sapply(model$mappings, function(m) isTRUE(m$trimmed))]

    if (length(active_mappings) > 0) {
      mappings_data <- data.frame(
        name = names(active_mappings),
        n_rows = sapply(active_mappings, function(m) {
          if (!is.null(m$data)) return(nrow(m$data))
          return(0L)
        }),
        n_cols = sapply(active_mappings, function(m) {
          if (!is.null(m$data)) return(ncol(m$data))
          return(0L)
        }),
        dims = sapply(active_mappings, function(m) {
          if (!is.null(m$dims)) return(paste(m$dims, collapse = ","))
          if (!is.null(m$data)) return(paste(names(m$data), collapse = ","))
          return("")
        }),
        desc = sapply(active_mappings, function(m) {
          if (!is.null(m$desc) && length(m$desc) > 0) return(m$desc[1])
          return("")
        }),
        stringsAsFactors = FALSE
      )
      mappings_dir <- file.path(model_dir, "mappings")
      dir.create(mappings_dir, showWarnings = FALSE, recursive = TRUE)
      write.csv(mappings_data, file.path(mappings_dir, "mappings.csv"), row.names = FALSE)
    }
  }

  # Generate parameters/parameters.csv
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    # Filter out trimmed parameters
    active_params <- model$parameters[!sapply(model$parameters, function(p) isTRUE(p$trimmed))]

    if (length(active_params) > 0) {
      params_data <- data.frame(
        name = names(active_params),
        n_rows = sapply(active_params, function(p) {
          if (!is.null(p$data)) return(nrow(p$data))
          return(0L)
        }),
        n_cols = sapply(active_params, function(p) {
          if (!is.null(p$data)) return(ncol(p$data))
          return(0L)
        }),
        dims = sapply(active_params, function(p) {
          # Always use actual column names from data (excluding 'value' column)
          # This avoids duplicate dimension names in metadata
          if (!is.null(p$data) && ncol(p$data) > 1) {
            dim_cols <- names(p$data)[-ncol(p$data)]
            return(paste(dim_cols, collapse = ","))
          }
          # Fallback to p$dims if data doesn't exist
          if (!is.null(p$dims) && length(p$dims) > 0) {
            return(paste(p$dims, collapse = ","))
          }
          return("")
        }),
        default_value = sapply(active_params, function(p) {
          if (is.null(p$defVal)) return(NA_character_)
          
          # Handle ast_formula - convert to R code string
          if (inherits(p$defVal, "ast_formula")) {
            return(paste0("<formula: ", as_r(p$defVal$expr), ">"))
          }
          
          # Handle other AST - convert to R code string
          if (inherits(p$defVal, "ast")) {
            return(paste0("<expr: ", as_r(p$defVal), ">"))
          }
          
          # Handle scalar values
          if (is.numeric(p$defVal) || is.character(p$defVal) || is.logical(p$defVal)) {
            return(as.character(p$defVal))
          }
          
          # Fallback for other types
          return(NA_character_)
        }),
        desc = mapply(function(p, nm) {
          sanitize_description(p$desc, nm)
        }, active_params, names(active_params), SIMPLIFY = TRUE, USE.NAMES = FALSE),
        stringsAsFactors = FALSE
      )
      params_dir <- file.path(model_dir, "parameters")
      dir.create(params_dir, showWarnings = FALSE, recursive = TRUE)
      write.csv(params_data, file.path(params_dir, "parameters.csv"), row.names = FALSE)
    }
  }

  invisible(NULL)
}


#' Save model data on disk
#'
#' @param model A multimod model object
#' @param path Path to save the model files
#' @param format Character. Data format: "parquet" (default), "ipc" (Arrow
#'   IPC/Feather), or "csv"
#' @param compression Compression method to use (`"zstd"`)
#' @param compression_level Compression level; zstd 1-22, default 3. Levels
#'   up to 12 give the same size on model data; 15 is a third smaller on
#'   sorted tables at several times the time
#' @param overwrite Logical; if TRUE, overwrite existing files
#' @param keep_in_memory Logical; if TRUE, keep data in memory after saving
#' @param save_structure Logical; if TRUE, save model structure separately
#' @param verbose Logical; if TRUE, print progress messages
#'
#' @return Invisibly, the model with storage metadata attached.
#' @export
#'
#' @details
#' Directory structure created (the extension follows `format`):
#' \preformatted{
#' path/
#'   model.rds                       model structure (without data when
#'                                   keep_in_memory = FALSE)
#'   metadata.json                   counts and format, for readers outside R
#'   format.txt                      "csv", "ipc" or "parquet"
#'   sets/sets.csv                   index of the sets
#'   sets/<name>/data.*              one file per set, column `member`
#'   mappings/mappings.csv           index of the mappings
#'   mappings/<name>/data.*          one column per dimension
#'   parameters/parameters.csv       index of the parameters
#'   parameters/<name>/data.*        dimension columns plus `value`
#'   parameters/<name>/folded_data/data.*   if the parameter is folded
#' }
#' The solvers write their generated code and output under
#' `path/solvers/<backend>/`; see the "Model workspace and data" article.
#'
#' @examples
#' m <- example_models$energyRt$multimod
#' ws <- file.path(tempdir(), "energyRt_ws")
#'
#' # Save with the default parquet format
#' m <- save_model(m, ws, verbose = FALSE)
#'
#' # Save and drop the data from memory; get_data() reloads it from disk
#' m_light <- save_model(m, ws, keep_in_memory = FALSE, verbose = FALSE)
#' head(get_data(m_light, "pTechOlife"))
save_model <- function(
    model,
    path,
    format = c("parquet", "ipc", "csv"),
    compression = "zstd",
    compression_level = 3,
    overwrite = TRUE,
    keep_in_memory = TRUE,
    save_structure = TRUE,
    verbose = TRUE
) {
  format <- match.arg(format)

  # Validate model
  if (!inherits(model, "mm_model") && !inherits(model, "multimod")) {
    stop("Object must be a multimod model")
  }

  # Check/create directory
  if (dir.exists(path)) {
    if (!overwrite) {
      stop("Model directory already exists: ", path, "\nUse overwrite = TRUE to replace")
    }
    if (verbose) cat("Overwriting existing model directory:", path, "\n")
    unlink(path, recursive = TRUE)
  }

  if (verbose) cat("Creating model directory:", path, "\n")
  dir.create(path, recursive = TRUE)

  # Save format information
  writeLines(format, file.path(path, "format.txt"))

  # Save metadata as JSON
  metadata <- list(
    name = model$name,
    desc = model$desc,
    saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    format = format,
    compression = if (format != "csv") compression else NA,
    compression_level = if (format != "csv") compression_level else NA,
    n_sets = length(model$sets),
    n_parameters = length(model$parameters),
    n_mappings = length(model$mappings),
    n_equations = if (save_structure) length(model$equations) else NA,
    n_variables = if (save_structure) length(model$variables) else NA
  )

  jsonlite::write_json(
    metadata,
    file.path(path, "metadata.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  # Save sets
  if (!is.null(model$sets) && length(model$sets) > 0) {
    if (verbose) cat("Saving", length(model$sets), "sets...\n")
    sets_dir <- file.path(path, "sets")
    dir.create(sets_dir, showWarnings = FALSE)

    for (set_name in names(model$sets)) {
      set_obj <- model$sets[[set_name]]
      # Sets use $data field in AST structure
      if (!is.null(set_obj$data) && length(set_obj$data) > 0) {
        # Convert set members to data frame to use consistent format
        set_df <- data.frame(
          member = as.character(set_obj$data),
          stringsAsFactors = FALSE
        )

        set_subdir <- file.path(sets_dir, set_name)
        dir.create(set_subdir, showWarnings = FALSE)

        # .save_dataframe adds extension, so pass path without extension
        .save_dataframe(
          data = set_df,
          path = file.path(set_subdir, "data"),
          format = format,
          compression = compression,
          compression_level = compression_level,
          verbose = FALSE
        )
      }
    }
  }

  # Save parameters
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    if (verbose) cat("Saving", length(model$parameters), "parameters...\n")
    params_dir <- file.path(path, "parameters")
    dir.create(params_dir, showWarnings = FALSE)

    for (param_name in names(model$parameters)) {
      param <- model$parameters[[param_name]]
      param_dir <- file.path(params_dir, param_name)
      dir.create(param_dir, showWarnings = FALSE)

      # Save data if exists
      if (!is.null(param$data) && nrow(param$data) > 0) {
        .save_dataframe(
          data = param$data,
          path = file.path(param_dir, "data"),
          format = format,
          compression = compression,
          compression_level = compression_level,
          verbose = verbose,
          label = param_name
        )

        # Save folded_data if exists
        if (!is.null(param$folded_data) && nrow(param$folded_data) > 0) {
          folded_dir <- file.path(param_dir, "folded_data")
          dir.create(folded_dir, showWarnings = FALSE, recursive = TRUE)
          .save_dataframe(
            data = param$folded_data,
            path = file.path(folded_dir, "data"),
            format = format,
            compression = compression,
            compression_level = compression_level,
            verbose = FALSE
          )
        }

        # Free memory if requested (metadata stays in main model object)
        if (!keep_in_memory) {
          model$parameters[[param_name]]$data <- NULL
          model$parameters[[param_name]]$folded_data <- NULL
        }
      }
    }
  }

  # Save mappings
  if (!is.null(model$mappings) && length(model$mappings) > 0) {
    if (verbose) cat("Saving", length(model$mappings), "mappings...\n")
    mappings_dir <- file.path(path, "mappings")
    dir.create(mappings_dir, showWarnings = FALSE)

    for (mapping_name in names(model$mappings)) {
      mapping <- model$mappings[[mapping_name]]
      mapping_dir <- file.path(mappings_dir, mapping_name)
      dir.create(mapping_dir, showWarnings = FALSE)

      # Save data if exists
      if (!is.null(mapping$data) && nrow(mapping$data) > 0) {
        .save_dataframe(
          data = mapping$data,
          path = file.path(mapping_dir, "data"),
          format = format,
          compression = compression,
          compression_level = compression_level,
          verbose = FALSE
        )

        # Free memory if requested (metadata stays in main model object)
        if (!keep_in_memory) {
          model$mappings[[mapping_name]]$data <- NULL
        }
      }
    }
  }

  # Equations and variables are saved in model.rds (no separate files needed)

  # Generate metadata CSV files (sets.csv, mappings.csv, parameters.csv)
  .generate_model_metadata_csv(model, path)

  # Update model storage info
  if (is.null(model$storage)) {
    model$storage <- list()
  }
  model$storage$path <- path
  model$storage$format <- format
  model$storage$inMemory <- keep_in_memory
  model$base_path <- path

  # Save complete model object (with structure, metadata, ASTs)
  # Data is stored separately in cross-language formats
  saveRDS(model, file.path(path, "model.rds"))

  if (verbose) {
    size_mb <- sum(file.size(list.files(path, recursive = TRUE, full.names = TRUE)), na.rm = TRUE) / 1024^2
    cat("Model saved:", path, "\n")
    cat("Total size:", round(size_mb, 2), "MB\n")
  }

  invisible(model)
}


#' Load multimod model from disk
#'
#' @param path Character. Path to model directory
#' @param load_data Logical. Load all data into memory (default: TRUE)
#' @param load_structure Logical. Load equations and variables (default: TRUE)
#' @param verbose Logical. Print progress messages
#'
#' @return A multimod model object
#' @export
#'
#' @examples
#' ws <- file.path(tempdir(), "energyRt_ws")
#' save_model(example_models$energyRt$multimod, ws, verbose = FALSE)
#'
#' # Load model with all data
#' m <- load_model(ws, verbose = FALSE)
#'
#' # Load structure only (for inspection)
#' m_light <- load_model(ws, load_data = FALSE, verbose = FALSE)
load_model <- function(
    path,
    load_data = TRUE,
    load_structure = TRUE,
    verbose = TRUE
) {
  if (!dir.exists(path)) {
    stop("Model directory not found: ", path)
  }

  model_file <- file.path(path, "model.rds")
  if (!file.exists(model_file)) {
    stop("Model file not found: ", model_file)
  }

  if (verbose) cat("Loading model from:", path, "\n")

  # Load model structure
  model <- readRDS(model_file)

  # Read format
  format_file <- file.path(path, "format.txt")
  if (file.exists(format_file)) {
    format <- readLines(format_file, n = 1, warn = FALSE)
  } else {
    format <- "csv"  # Default fallback
  }

  if (load_data) {
    # Load parameters
    params_dir <- file.path(path, "parameters")
    if (dir.exists(params_dir)) {
      param_names <- list.files(params_dir)
      if (length(param_names) > 0) {
        if (verbose) cat("Loading", length(param_names), "parameters...\n")

        for (param_name in param_names) {
          param_dir <- file.path(params_dir, param_name)

          # Load data
          data_file <- file.path(param_dir, "data")
          data <- .load_dataframe(data_file, format)

          if (!is.null(data)) {
            if (!is.null(model$parameters[[param_name]])) {
              model$parameters[[param_name]]$data <- data
            }

            # Load folded_data if exists
            folded_file <- file.path(param_dir, "folded_data", "data")
            folded_data <- .load_dataframe(folded_file, format)
            if (!is.null(folded_data)) {
              model$parameters[[param_name]]$folded_data <- folded_data
            }
          }
        }
      }
    }

    # Load mappings
    mappings_dir <- file.path(path, "mappings")
    if (dir.exists(mappings_dir)) {
      mapping_names <- list.files(mappings_dir)
      if (length(mapping_names) > 0) {
        if (verbose) cat("Loading", length(mapping_names), "mappings...\n")

        for (mapping_name in mapping_names) {
          mapping_dir <- file.path(mappings_dir, mapping_name)
          data_file <- file.path(mapping_dir, "data")
          data <- .load_dataframe(data_file, format)

          if (!is.null(data) && !is.null(model$mappings[[mapping_name]])) {
            model$mappings[[mapping_name]]$data <- data
          }
        }
      }
    }

    model$storage$inMemory <- TRUE
  } else {
    model$storage$inMemory <- FALSE
  }

  # Equations and variables already loaded from model.rds

  model$storage$path <- path
  model$storage$format <- format
  model$base_path <- path

  if (verbose) cat("Model loaded:", model$name %||% "(unnamed)", "\n")

  model
}


#' Update parameter data
#'
#' @description
#' Low-level function to update parameter data in a model. Use this after
#' data import or interpolation to set parameter values.
#'
#' @param model A multimod model object
#' @param name Character. Parameter name
#' @param data Data.frame with parameter values
#' @param folded_data Data.frame with folded parameter values (optional)
#' @param auto_save Logical. If TRUE and model is on-disk, immediately save to disk
#'
#' @return Modified model object
#' @export
#'
#' @examples
#' m <- example_models$energyRt$multimod
#' olife <- get_data(m, "pTechOlife")
#' olife$value <- olife$value + 5
#' m <- update_parameter(m, "pTechOlife", olife)
#' head(get_data(m, "pTechOlife"))
update_parameter <- function(model, name, data, folded_data = NULL, auto_save = FALSE) {
  if (!name %in% names(model$parameters)) {
    stop("Parameter '", name, "' not found in model")
  }

  if (!is.null(data) && !is.data.frame(data)) {
    stop("Parameter data must be a data.frame")
  }

  # Update data; the table given replaces any reference the symbol carried
  .data_memo_drop(.data_memo_key(model$parameters[[name]],
                                 get_data_path(model$parameters[[name]], model$base_path)))
  .data_memo_drop(paste0("path:", file.path(model$storage$path %||% "", "parameters", name, "data")))
  model$parameters[[name]]$data <- data
  model$parameters[[name]]$misc$ref <- NULL
  model$parameters[[name]]$misc$path <- NULL
  model$parameters[[name]]$misc$onDisk <- NULL
  model$parameters[[name]]$misc$inMemory <- TRUE

  # Update folded_data if provided
  if (!is.null(folded_data)) {
    model$parameters[[name]]$folded_data <- folded_data
  }

  # Mark as modified
  if (is.null(model$storage$modified)) {
    model$storage$modified <- list(parameters = character(0), mappings = character(0))
  }
  if (!name %in% model$storage$modified$parameters) {
    model$storage$modified$parameters <- c(model$storage$modified$parameters, name)
  }

  # Auto-save if requested and model is on-disk
  if (auto_save && !is.null(model$storage$path)) {
    if (!is.null(model$storage$format)) {
      format <- model$storage$format
    } else {
      format <- "ipc"  # Default
    }

    # Save this specific parameter to disk
    param_dir <- file.path(model$storage$path, "parameters", name)
    if (!dir.exists(param_dir)) {
      dir.create(param_dir, recursive = TRUE, showWarnings = FALSE)
    }

    .save_dataframe(
      data = data,
      path = file.path(param_dir, "data"),
      format = format,
      compression = "zstd",
      compression_level = 3,
      verbose = FALSE
    )

    # Save folded_data if exists
    if (!is.null(folded_data)) {
      folded_dir <- file.path(param_dir, "folded_data")
      if (!dir.exists(folded_dir)) {
        dir.create(folded_dir, recursive = TRUE, showWarnings = FALSE)
      }
      .save_dataframe(
        data = folded_data,
        path = file.path(folded_dir, "data"),
        format = format,
        compression = "zstd",
        compression_level = 3,
        verbose = FALSE
      )
    }

    # Remove from modified list since we just saved
    model$storage$modified$parameters <- setdiff(model$storage$modified$parameters, name)
  }

  model
}


#' Update mapping data
#'
#' @description
#' Low-level function to update mapping data in a model.
#'
#' @param model A multimod model object
#' @param name Character. Mapping name
#' @param data Data.frame with mapping tuples
#' @param auto_save Logical. If TRUE and model is on-disk, immediately save to disk
#'
#' @return Modified model object
#' @export
#'
#' @examples
#' m <- example_models$energyRt$multimod
#' map <- get_data(m, "mCommTimeslice", type = "mapping")
#' m <- update_mapping(m, "mCommTimeslice", map[1:10, ])
#' nrow(get_data(m, "mCommTimeslice", type = "mapping"))
update_mapping <- function(model, name, data, auto_save = FALSE) {
  if (!name %in% names(model$mappings)) {
    stop("Mapping '", name, "' not found in model")
  }

  if (!is.null(data) && !is.data.frame(data)) {
    stop("Mapping data must be a data.frame")
  }

  # Update data; the table given replaces any reference the symbol carried
  .data_memo_drop(.data_memo_key(model$mappings[[name]],
                                 get_data_path(model$mappings[[name]], model$base_path)))
  .data_memo_drop(paste0("path:", file.path(model$storage$path %||% "", "mappings", name, "data")))
  model$mappings[[name]]$data <- data
  model$mappings[[name]]$misc$ref <- NULL
  model$mappings[[name]]$misc$path <- NULL
  model$mappings[[name]]$misc$onDisk <- NULL
  model$mappings[[name]]$misc$inMemory <- TRUE

  # Mark as modified
  if (is.null(model$storage$modified)) {
    model$storage$modified <- list(parameters = character(0), mappings = character(0))
  }
  if (!name %in% model$storage$modified$mappings) {
    model$storage$modified$mappings <- c(model$storage$modified$mappings, name)
  }

  # Auto-save if requested
  if (auto_save && !is.null(model$storage$path)) {
    if (!is.null(model$storage$format)) {
      format <- model$storage$format
    } else {
      format <- "ipc"
    }

    mapping_dir <- file.path(model$storage$path, "mappings", name)
    if (!dir.exists(mapping_dir)) {
      dir.create(mapping_dir, recursive = TRUE, showWarnings = FALSE)
    }

    .save_dataframe(
      data = data,
      path = file.path(mapping_dir, "data"),
      format = format,
      compression = "zstd",
      compression_level = 3,
      verbose = FALSE
    )

    # Remove from modified list
    model$storage$modified$mappings <- setdiff(model$storage$modified$mappings, name)
  }

  model
}
# Does this object claim data that lives somewhere other than $data?
#
# energyRt's convert_parameter() records an on-disk source as misc$path /
# misc$onDisk and leaves $data empty, but get_data() only ever looked in $data
# and in model$storage -- two conventions that never met. The consequence was
# silent: every parameter fell through to its scalar default and the model
# built as a 1x1, while the import log still reported every symbol linked.
.claims_external_data <- function(obj) {
  if (is.null(obj)) return(FALSE)
  m <- obj$misc
  if (is.null(m)) return(FALSE)
  isTRUE(!is.null(m$path)) || isTRUE(!is.null(m$onDisk)) || identical(m$inMemory, FALSE)
}

# A symbol that claims data elsewhere and yields none must not quietly become
# its default. Wrong numbers with no error are worse than a stop, especially on
# a large model where nobody can eyeball the result.
#' Does the object's own on-disk record say the table is empty?
#'
#' energyRt stores a summary of the detached table in `misc$onDisk`, including
#' its dimensions. A map that legitimately holds no tuples (`mvTechPhaseOut` in
#' a scenario with no phase-outs) therefore *claims* external data while having
#' nothing to load - which is not the same failure as a path that cannot be
#' read, and must not be reported as one.
#' @keywords internal
#' @noRd
.recorded_empty <- function(obj) {
  d <- .ondisk_dim(obj$misc$onDisk)
  !is.null(d) && isTRUE(as.integer(d[1]) == 0L)
}

#' Dimensions recorded in an on-disk summary, or NULL
#'
#' energyRt keys the record by slot name (`onDisk$data$dim`); a flat record
#' (`onDisk$dim`) also occurs. Accept either.
#' @keywords internal
#' @noRd
.ondisk_dim <- function(rec) {
  if (is.null(rec)) return(NULL)
  if (!is.null(rec$dim)) return(rec$dim)
  rec[["data"]]$dim
}

#' A zero-row table with the object's declared columns
#' @keywords internal
#' @noRd
.empty_like <- function(obj) {
  nms <- tryCatch(
    vapply(obj$dims, dim_binding_name, character(1), USE.NAMES = FALSE),
    error = function(e) character()
  )
  nms <- nms[nzchar(nms)]
  if (inherits(obj, "mm_parameter")) nms <- c(nms, "value")
  if (!length(nms)) return(data.frame())
  cols <- rep(list(character()), length(nms))
  if (inherits(obj, "mm_parameter")) cols[[length(cols)]] <- numeric()
  names(cols) <- nms
  as.data.frame(cols, stringsAsFactors = FALSE)
}

.no_data_for <- function(obj, name, kind) {
  stop(sprintf(paste0(
    "%s '%s' declares data outside $data but none could be loaded.\n",
    "  misc$path   : %s\n",
    "  misc$inMemory: %s\n",
    "Load it in memory (for an energyRt scenario, ",
    "energyRt::multimod_import_data(..., inMemory = TRUE)) or make ",
    "the path readable. Continuing would silently use the default value."),
    kind, name,
    if (is.null(obj$misc$path)) "<none>" else as.character(obj$misc$path)[1],
    if (is.null(obj$misc$inMemory)) "<unset>" else as.character(obj$misc$inMemory)),
    call. = FALSE)
}



#' Get data from model (with optional lazy loading)
#'
#' @description
#' Retrieve parameter, mapping, or set data from a model. Automatically loads
#' from disk if the model is stored and data is not in memory.
#'
#' @param model A multimod model object
#' @param name Character. Name of parameter, mapping, or set
#' @param type Character. Type of object: "parameter", "mapping", or "set"
#' @param ... Additional filtering arguments (for future use)
#'
#' @return Data.frame, character vector (for sets), or NULL
#' @export
#'
#' @examples
#' m <- example_models$energyRt$multimod
#'
#' # Parameter data
#' head(get_data(m, "pTechOlife"))
#'
#' # Mapping data
#' head(get_data(m, "mCommTimeslice", type = "mapping"))
get_data <- function(model, name, type = c("parameter", "mapping", "set", "variable"), ...) {
  type <- match.arg(type)

  if (type == "set") {
    set_obj <- model$sets[[name]]
    if (is.null(set_obj)) return(NULL)

    if (!is.null(set_obj$data) && length(set_obj$data) > 0) {
      return(as.character(set_obj$data))
    }

    # save_model() writes sets/<name>/data.<ext> with one column, `member`
    if (!is.null(model$storage$path)) {
      set_file <- file.path(model$storage$path, "sets", name, "data")
      data <- .load_dataframe(set_file, model$storage$format)
      if (!is.null(data) && "member" %in% names(data)) {
        return(as.character(data$member))
      }
    }

    return(NULL)
  }

  if (type == "parameter") {
    param <- model$parameters[[name]]
    if (is.null(param)) return(NULL)

    # Check if data is in memory
    if (!is.null(param$data) && nrow(param$data) > 0) {
      return(param$data)
    }

    # From the workspace, through the session memo (`model` is a local copy:
    # a table assigned into it would not outlive this call)
    if (!is.null(model$storage$path)) {
      param_file <- file.path(model$storage$path, "parameters", name, "data")
      data <- .data_memo_read(paste0("path:", param_file),
                              function() .load_dataframe(param_file, model$storage$format))
      if (!is.null(data)) return(data)
    }

    # The per-object reference, which the model$storage path above does not
    # know about. get_lazy_data() resolves misc$path/onDisk properly.
    if (.claims_external_data(param)) {
      key <- .data_memo_key(param, get_data_path(param, model$base_path))
      d <- .data_memo_read(key, function() tryCatch(get_lazy_data(param, base_path = model$base_path),
                                                    error = function(e) NULL))
      if (!is.null(d) && NROW(d) > 0) return(d)
      if (.recorded_empty(param)) return(.empty_like(param))
      .no_data_for(param, name, "Parameter")
    }

    # Fallback to scalar default when no data or formula is available
    if (is.null(param$formula) && !is.null(param$defVal) && !inherits(param$defVal, "ast")) {
      return(param$defVal)
    }

    return(NULL)
  }

  if (type == "mapping") {
    mapping <- model$mappings[[name]]
    if (is.null(mapping)) return(NULL)

    # Check if data is in memory
    if (!is.null(mapping$data) && nrow(mapping$data) > 0) {
      return(mapping$data)
    }

    if (!is.null(model$storage$path)) {
      mapping_file <- file.path(model$storage$path, "mappings", name, "data")
      data <- .data_memo_read(paste0("path:", mapping_file),
                              function() .load_dataframe(mapping_file, model$storage$format))
      if (!is.null(data)) return(data)
    }

    # Same per-object reference the parameter branch honours above.
    if (.claims_external_data(mapping)) {
      key <- .data_memo_key(mapping, get_data_path(mapping, model$base_path))
      d <- .data_memo_read(key, function() tryCatch(get_lazy_data(mapping, base_path = model$base_path),
                                                    error = function(e) NULL))
      if (!is.null(d) && NROW(d) > 0) return(d)
      if (.recorded_empty(mapping)) return(.empty_like(mapping))
      .no_data_for(mapping, name, "Mapping")
    }

    return(NULL)
  }

  if (type == "variable") {
    variable <- model$variables[[name]]
    if (is.null(variable)) return(NULL)

    # Check if solution data is in memory
    if (!is.null(variable$solution) && nrow(variable$solution) > 0) {
      return(variable$solution)
    }

    # Try to load from disk
    if (!is.null(model$storage$path) || !is.null(model$base_path)) {
      base <- if (!is.null(model$storage$path)) model$storage$path else model$base_path
      var_file <- file.path(base, "variables", name, "data")
      format <- if (!is.null(model$storage$format)) model$storage$format else "ipc"
      data <- .load_dataframe(var_file, format)
      if (!is.null(data)) {
        # Cache in model
        model$variables[[name]]$solution <- data
        return(data)
      }
    }

    return(NULL)
  }

  NULL
}


# The compression codec to write with. Arrow builds differ in the codecs they
# include (a minimal Linux build may lack zstd), so an unavailable codec falls
# back to `fallback`, and then to no compression.
.arrow_codec <- function(compression, fallback) {
  for (codec in c(compression, fallback)) {
    if (identical(codec, "uncompressed") || isTRUE(arrow::codec_is_available(codec))) {
      return(codec)
    }
  }
  "uncompressed"
}

#' Internal: Save dataframe to disk
#'
#' @return Invisibly, the path of the written file, or `NULL` when `data` is
#'   empty.
#' @keywords internal
.save_dataframe <- function(
    data,
    path,
    format,
    compression = "zstd",
    compression_level = 3,
    verbose = FALSE,
    label = NULL
) {
  if (is.null(data) || nrow(data) == 0) {
    return(invisible(NULL))
  }

  if (format == "csv") {
    file_path <- paste0(path, ".csv")
    # Convert all columns to character to ensure consistent string types
    # This is critical for Julia/JuMP where set members are strings
    data_str <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
    write.csv(data_str, file_path, row.names = FALSE)
    if (verbose && !is.null(label)) {
      cat("  ", label, ": ", nrow(data), " rows\n", sep = "")
    }
    return(invisible(file_path))
  }

  if (format == "ipc") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' required for IPC format. Install with: install.packages('arrow')")
    }
    file_path <- paste0(path, ".arrow")
    codec <- .arrow_codec(compression, fallback = "lz4")
    arrow::write_feather(
      as.data.frame(data),
      file_path,
      compression = codec,
      compression_level = if (identical(codec, compression)) compression_level else NULL
    )
    if (verbose && !is.null(label)) {
      cat("  ", label, ": ", nrow(data), " rows\n", sep = "")
    }
    return(invisible(file_path))
  }

  if (format == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' required for Parquet format. Install with: install.packages('arrow')")
    }
    file_path <- paste0(path, ".parquet")
    codec <- .arrow_codec(compression, fallback = "snappy")
    arrow::write_parquet(
      as.data.frame(data),
      file_path,
      compression = codec,
      compression_level = if (identical(codec, compression)) compression_level else NULL
    )
    if (verbose && !is.null(label)) {
      cat("  ", label, ": ", nrow(data), " rows\n", sep = "")
    }
    return(invisible(file_path))
  }

  stop("Unsupported format: ", format)
}


#' Internal: Load dataframe from disk
#'
#' @return A data frame, or `NULL` when no file of `format` exists at `path`.
#' @keywords internal
.load_dataframe <- function(path, format) {
  # Try each format
  if (format == "csv") {
    file_path <- paste0(path, ".csv")
    if (file.exists(file_path)) {
      return(read.csv(file_path, stringsAsFactors = FALSE))
    }
  }

  if (format == "ipc") {
    file_path <- paste0(path, ".arrow")
    if (file.exists(file_path)) {
      if (requireNamespace("arrow", quietly = TRUE)) {
        # mmap = FALSE: a memory-mapped file stays locked on Windows while the
        # returned data frame lives, so it could not be replaced or removed
        return(as.data.frame(arrow::read_feather(file_path, mmap = FALSE)))
      }
    }
  }

  if (format == "parquet") {
    file_path <- paste0(path, ".parquet")
    if (file.exists(file_path)) {
      if (requireNamespace("arrow", quietly = TRUE)) {
        return(as.data.frame(arrow::read_parquet(file_path, mmap = FALSE)))
      }
    }
  }

  NULL
}


#' Null coalescing operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
