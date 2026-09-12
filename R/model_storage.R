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
#'   \item \strong{csv} - Universal, human-readable (default)
#'   \item \strong{ipc} - Arrow IPC/Feather (.arrow extension), fastest, compression: zstd level 15 (recommended)
#'   \item \strong{parquet} - Popular Arrow format (but may have compatibility issues)
#' }
#'
#' @name model_workspace
#' @keywords internal
NULL


#' Save multimod model workspace to disk
#'
#' @description
#' Save a complete multimod model to a workspace directory with efficient Arrow storage for
#' data frames. The directory structure mirrors the model object structure
#' for consistency and easy navigation.
#'
#' @param model A multimod model object
#' @param path Character. Directory path for model workspace
#' @param format Character. Data format: "csv" (default), "ipc" (Arrow IPC/Feather), or "parquet"
#' @param compression Character. Compression codec for Arrow formats: "zstd" (default), "lz4", "snappy", or "uncompressed"
#' @param compression_level Integer. Compression level (default: 15 for zstd)
#' @param overwrite Logical. Overwrite existing model workspace directory
#' @param keep_in_memory Logical. Keep data in memory after saving (default: TRUE)
#' @param save_structure Logical. Save equations, variables, and other structure (default: TRUE)
#' @param verbose Logical. Print progress messages
#'
#' @return Modified model object with storage metadata
#' @export
#'
#' @details
#' Directory structure created:
#' \preformatted{
#' model_path/
#'   model.rds              # Model structure (thinned if keep_in_memory=FALSE)
#'   metadata.json          # Human-readable metadata
#'   format.txt             # Storage format
#'   sets/
#'     set_name.txt         # Set members (one per line)
#'   parameters/
#'     param_name/
#'       data.{csv|arrow|parquet}   # Parameter data
#'       metadata.rds               # Parameter metadata
#'   mappings/
#'     mapping_name/
#'       data.{csv|arrow|parquet}
#'   folded_data/           # If folding applied
#'     param_name/
#'       folded_data.{csv|arrow|parquet}
#'   equations/
#'     equations.rds        # All equations
#'   variables/
#'     variables.rds        # All variables
#' }
#'
#' @examples
#' \dontrun{
#' # Save with default CSV format
#' model <- save_model(model, "models/utopia")
#'
#' # Save with Arrow IPC (fastest)
#' model <- save_model(model, "models/utopia", format = "ipc")
#'
#' # Save and free memory
#' model <- save_model(model, "models/utopia",
#'                     format = "ipc",
#'                     keep_in_memory = FALSE)
#' }
#'
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
#' @param format File format for saving ("csv", "rds", "parquet", etc.)
#' @param compression Compression method to use
#' @param compression_level Compression level (1-9)
#' @param overwrite Logical; if TRUE, overwrite existing files
#' @param keep_in_memory Logical; if TRUE, keep data in memory after saving
#' @param save_structure Logical; if TRUE, save model structure separately
#' @param verbose Logical; if TRUE, print progress messages
#'
#' @return Invisibly returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' save_model(model, path = "model_data", format = "csv")
#' }
save_model <- function(
    model,
    path,
    format = c("csv", "ipc", "parquet"),
    compression = "zstd",
    compression_level = 15,
    overwrite = TRUE,
    keep_in_memory = TRUE,
    save_structure = TRUE,
    verbose = TRUE
) {
  format <- match.arg(format)

  # Validate model
  if (!inherits(model, "model") && !inherits(model, "multimod")) {
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

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(
      metadata,
      file.path(path, "metadata.json"),
      pretty = TRUE,
      auto_unbox = TRUE
    )
  }

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
#' \dontrun{
#' # Load model with all data
#' model <- load_model("models/utopia")
#'
#' # Load structure only (for inspection)
#' model <- load_model("models/utopia", load_data = FALSE)
#' }
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
#' \dontrun{
#' # Update parameter data
#' model <- update_parameter(model, "pTechCost", new_tech_cost_data)
#'
#' # Update with folded data
#' model <- update_parameter(model, "pTechCost",
#'                          data = tech_cost_data,
#'                          folded_data = tech_cost_folded)
#'
#' # Update and immediately save to disk
#' model <- update_parameter(model, "pTechCost", new_data, auto_save = TRUE)
#' }
update_parameter <- function(model, name, data, folded_data = NULL, auto_save = FALSE) {
  if (!name %in% names(model$parameters)) {
    stop("Parameter '", name, "' not found in model")
  }

  if (!is.null(data) && !is.data.frame(data)) {
    stop("Parameter data must be a data.frame")
  }

  # Update data
  model$parameters[[name]]$data <- data

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
      compression_level = 15,
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
        compression_level = 15,
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
#' \dontrun{
#' # Update mapping data
#' model <- update_mapping(model, "mTechRegion", new_mapping_data)
#' }
update_mapping <- function(model, name, data, auto_save = FALSE) {
  if (!name %in% names(model$mappings)) {
    stop("Mapping '", name, "' not found in model")
  }

  if (!is.null(data) && !is.data.frame(data)) {
    stop("Mapping data must be a data.frame")
  }

  # Update data
  model$mappings[[name]]$data <- data

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
      compression_level = 15,
      verbose = FALSE
    )

    # Remove from modified list
    model$storage$modified$mappings <- setdiff(model$storage$modified$mappings, name)
  }

  model
}
# Does this object claim data that lives somewhere other than $data?
#
# convert_energyrt_parameter() records an on-disk source as misc$path /
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
  if (inherits(obj, "parameter")) nms <- c(nms, "value")
  if (!length(nms)) return(data.frame())
  cols <- rep(list(character()), length(nms))
  if (inherits(obj, "parameter")) cols[[length(cols)]] <- numeric()
  names(cols) <- nms
  as.data.frame(cols, stringsAsFactors = FALSE)
}

.no_data_for <- function(obj, name, kind) {
  stop(sprintf(paste0(
    "%s '%s' declares data outside $data but none could be loaded.\n",
    "  misc$path   : %s\n",
    "  misc$inMemory: %s\n",
    "Load it in memory (import_energyRt_data(..., inMemory = TRUE)) or make ",
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
#' \dontrun{
#' # Get parameter data
#' data <- get_data(model, "pTechCost")
#'
#' # Get mapping data
#' data <- get_data(model, "mTechRegion", type = "mapping")
#'
#' # Get set members
#' members <- get_data(model, "tech", type = "set")
#' }
get_data <- function(model, name, type = c("parameter", "mapping", "set", "variable"), ...) {
  type <- match.arg(type)

  if (type == "set") {
    if (!is.null(model$sets[[name]])) {
      return(model$sets[[name]]$members)
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

    # Try to load from disk
    if (!is.null(model$storage$path)) {
      param_file <- file.path(model$storage$path, "parameters", name, "data")
      data <- .load_dataframe(param_file, model$storage$format)
      if (!is.null(data)) {
        # Cache in model
        model$parameters[[name]]$data <- data
        return(data)
      }
    }

    # The per-object reference, which the model$storage path above does not
    # know about. get_lazy_data() resolves misc$path/onDisk properly.
    if (.claims_external_data(param)) {
      d <- tryCatch(get_lazy_data(param, base_path = model$base_path),
                    error = function(e) NULL)
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

    # Try to load from disk
    if (!is.null(model$storage$path)) {
      mapping_file <- file.path(model$storage$path, "mappings", name, "data")
      data <- .load_dataframe(mapping_file, model$storage$format)
      if (!is.null(data)) {
        # Cache in model
        model$mappings[[name]]$data <- data
        return(data)
      }
    }

    # Same per-object reference the parameter branch honours above.
    if (.claims_external_data(mapping)) {
      d <- tryCatch(get_lazy_data(mapping, base_path = model$base_path),
                    error = function(e) NULL)
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


#' Internal: Save dataframe to disk
#'
#' @keywords internal
.save_dataframe <- function(
    data,
    path,
    format,
    compression = "zstd",
    compression_level = 15,
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
    arrow::write_feather(
      as.data.frame(data),
      file_path,
      compression = compression,
      compression_level = compression_level
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
    arrow::write_parquet(
      as.data.frame(data),
      file_path,
      compression = compression,
      compression_level = compression_level
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
        return(as.data.frame(arrow::read_feather(file_path)))
      }
    }
  }

  if (format == "parquet") {
    file_path <- paste0(path, ".parquet")
    if (file.exists(file_path)) {
      if (requireNamespace("arrow", quietly = TRUE)) {
        return(as.data.frame(arrow::read_parquet(file_path)))
      }
    }
  }

  NULL
}


#' Null coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
