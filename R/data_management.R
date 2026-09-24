#' Data management utilities for multimod models
#'
#' Functions to manage in-memory vs on-disk data storage for model parameters
#' and mappings, following the energyRt pattern.

#' Check if data is in memory
#'
#' @param obj A multimod model or parameter/mapping object
#' @return Logical. TRUE if data is in memory, FALSE if on disk
#' 
#' @details
#' Checks hierarchy:
#' 1. Object's misc$inMemory if set
#' 2. Model's inMemory default if object is a parameter/mapping
#' 3. TRUE by default (conservative)
#' 
#' @export
is_in_memory <- function(obj) {
  # Check object's own inMemory flag
  if (!is.null(obj$misc$inMemory)) {
    return(obj$misc$inMemory)
  }
  
  # For parameters/mappings, check if they have a model context
  # (would need to be passed separately or stored as attribute)
  
  # Default: assume in memory (conservative)
  TRUE
}

#' Get data path for a parameter or mapping
#'
#' @param obj Parameter or mapping object
#' @param base_path Optional base path to prepend for relative paths
#' @return Character path to data directory
#' 
#' @export
get_data_path <- function(obj, base_path = NULL) {
  if (is.null(obj$misc$path)) {
    return(NULL)
  }
  
  path <- obj$misc$path
  
  # If absolute path, return as-is
  if (grepl("^[A-Za-z]:|^/|^\\\\", path)) {
    return(path)
  }
  
  # Relative path - prepend base_path if provided
  if (!is.null(base_path)) {
    path <- file.path(base_path, path)
  }
  
  path
}

#' Set data path for a parameter or mapping
#'
#' @param obj Parameter or mapping object
#' @param path Character path (relative or absolute)
#' @return Modified object
#' 
#' @export
set_data_path <- function(obj, path) {
  if (is.null(obj$misc)) {
    obj$misc <- list()
  }
  obj$misc$path <- path
  obj
}

#' Detect Arrow data format
#'
#' @param path Path to data directory or file
#' @return Character format name: "parquet", "feather", "ipc", "csv", or NULL
#' 
#' @keywords internal
detect_arrow_format <- function(path) {
  if (!dir.exists(path) && !file.exists(path)) {
    return(NULL)
  }
  
  # Check for common Arrow dataset structure (data/*.parquet)
  data_dir <- file.path(path, "data")
  if (dir.exists(data_dir)) {
    files <- list.files(data_dir)
    if (any(grepl("\\.parquet$", files))) return("parquet")
    if (any(grepl("\\.feather$", files))) return("feather")
    if (any(grepl("\\.arrow$", files))) return("ipc")
    if (any(grepl("\\.csv$", files))) return("csv")
  }
  
  # Check path itself
  if (grepl("\\.parquet$", path)) return("parquet")
  if (grepl("\\.feather$", path)) return("feather")
  if (grepl("\\.arrow$", path)) return("ipc")
  if (grepl("\\.csv$", path)) return("csv")
  
  NULL
}

#' Load data from disk using Arrow
#'
#' @param path Path to data directory or file
#' @param format Optional format specification
#' @param collect Logical. If TRUE, collect into memory (data.table)
#' @return Arrow dataset or data.table
#' 
#' @keywords internal
#' @export
load_arrow_data <- function(path, format = NULL, collect = TRUE) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' required for on-disk data. Install with: install.packages('arrow')")
  }
  
  if (is.null(format)) {
    format <- detect_arrow_format(path)
  }
  
  if (is.null(format)) {
    stop("Cannot detect data format at: ", path)
  }
  
  # Open dataset based on format
  ds <- tryCatch({
    if (format %in% c("parquet", "feather", "ipc", "csv")) {
      arrow::open_dataset(path, format = format)
    } else {
      stop("Unsupported format: ", format)
    }
  }, error = function(e) {
    stop("Failed to open dataset at ", path, ": ", conditionMessage(e))
  })
  
  if (collect) {
    df <- dplyr::collect(ds)
    # Convert to data.table if available
    if (requireNamespace("data.table", quietly = TRUE)) {
      return(data.table::as.data.table(df))
    }
    return(as.data.frame(df))
  }
  
  ds
}



#' Save data to disk using Arrow
#'
#' @param data Data frame or data.table to save
#' @param path Path to save to
#' @param format Format: "parquet", "feather", "ipc", "csv"
#' @param overwrite Logical. Overwrite existing data?
#' 
#' @keywords internal
save_arrow_data <- function(data, path, format = "parquet", overwrite = FALSE) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' required. Install with: install.packages('arrow')")
  }
  
  # Create directory structure
  data_dir <- file.path(path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  
  # Write based on format
  if (format == "parquet") {
    arrow::write_dataset(data, data_dir, format = "parquet")
  } else if (format == "feather") {
    arrow::write_dataset(data, data_dir, format = "feather")
  } else if (format == "ipc") {
    arrow::write_dataset(data, data_dir, format = "ipc")
  } else if (format == "csv") {
    arrow::write_dataset(data, data_dir, format = "csv")
  } else {
    stop("Unsupported format: ", format)
  }
  
  invisible(path)
}

#' Get lazy data with automatic loading
#'
#' @param obj Parameter or mapping object
#' @param base_path Base path for relative paths
#' @param collect Logical. Collect into memory?
#' @return Data (data.table if collect=TRUE, Arrow dataset if FALSE)
#' 
#' @export
get_lazy_data <- function(obj, base_path = NULL, collect = TRUE) {
  # If in memory, return data directly
  if (is_in_memory(obj)) {
    return(obj$data)
  }
  
  # Load from disk
  path <- get_data_path(obj, base_path)
  if (is.null(path)) {
    # No path set - data should be in memory or doesn't exist
    return(NULL)
  }
  
  if (!dir.exists(path) && !file.exists(path)) {
    # Only warn if object explicitly has a path set (not just NULL)
    if (!is.null(obj$data_path)) {
      warning("Data path does not exist: ", path)
    }
    return(NULL)
  }
  
  load_arrow_data(path, collect = collect)
}

#' Load parameter data into memory
#'
#' @param model Multimod model
#' @param param_name Name of parameter to load
#' @return Modified model with parameter data loaded
#' 
#' @export
load_parameter <- function(model, param_name) {
  stopifnot(inherits(model, "multimod"))
  
  if (!param_name %in% names(model$parameters)) {
    stop("Parameter not found: ", param_name)
  }
  
  param <- model$parameters[[param_name]]
  
  # Already in memory?
  if (is_in_memory(param)) {
    message("Parameter '", param_name, "' already in memory")
    return(model)
  }
  
  # Load from disk
  data <- get_lazy_data(param, base_path = model$base_path, collect = TRUE)
  
  # Update parameter
  param$data <- data
  if (is.null(param$misc)) param$misc <- list()
  param$misc$inMemory <- TRUE
  
  model$parameters[[param_name]] <- param
  model
}

#' Load mapping data into memory
#'
#' @param model Multimod model
#' @param mapping_name Name of mapping to load
#' @return Modified model with mapping data loaded
#' 
#' @export
load_mapping <- function(model, mapping_name) {
  stopifnot(inherits(model, "multimod"))
  
  if (!mapping_name %in% names(model$mappings)) {
    stop("Mapping not found: ", mapping_name)
  }
  
  mapping <- model$mappings[[mapping_name]]
  
  # Already in memory?
  if (is_in_memory(mapping)) {
    message("Mapping '", mapping_name, "' already in memory")
    return(model)
  }
  
  # Load from disk
  data <- get_lazy_data(mapping, base_path = model$base_path, collect = TRUE)
  
  # Update mapping
  mapping$data <- data
  if (is.null(mapping$misc)) mapping$misc <- list()
  mapping$misc$inMemory <- TRUE
  
  model$mappings[[mapping_name]] <- mapping
  model
}

#' Save parameter data to disk and free memory
#'
#' @param model Multimod model
#' @param param_name Name of parameter to save
#' @param format Data format: "parquet", "feather", "ipc", "csv"
#' @param keep_in_memory Keep data in memory after saving?
#' @return Modified model
#' 
#' @export
save_parameter <- function(model, param_name, format = "parquet", keep_in_memory = FALSE) {
  stopifnot(inherits(model, "multimod"))
  
  if (!param_name %in% names(model$parameters)) {
    stop("Parameter not found: ", param_name)
  }
  
  param <- model$parameters[[param_name]]
  
  # Get data
  if (is.null(param$data) || nrow(param$data) == 0) {
    warning("No data to save for parameter: ", param_name)
    return(model)
  }
  
  # Determine path
  path <- get_data_path(param, base_path = model$base_path)
  if (is.null(path)) {
    # Generate default path
    if (is.null(model$base_path)) {
      stop("No base_path set in model and no path set for parameter")
    }
    path <- file.path(model$base_path, "parameters", param_name)
    param <- set_data_path(param, file.path("parameters", param_name))
  } else if (!grepl("^[A-Za-z]:|^/|^\\\\", path)) {
    # Relative path - prepend base_path
    path <- file.path(model$base_path, path)
  }
  
  # Save to disk
  save_arrow_data(param$data, path, format = format)
  
  # Update metadata
  if (is.null(param$misc)) param$misc <- list()
  param$misc$onDisk <- list(
    data = list(
      dim = dim(param$data),
      nrow = nrow(param$data),
      size = format(object.size(param$data), units = "auto")
    )
  )
  
  # Free memory if requested
  if (!keep_in_memory) {
    param$data <- data.frame()
    param$misc$inMemory <- FALSE
  } else {
    param$misc$inMemory <- TRUE
  }
  
  model$parameters[[param_name]] <- param
  model
}

#' Save mapping data to disk and free memory
#'
#' @param model Multimod model
#' @param mapping_name Name of mapping to save
#' @param format Data format: "parquet", "feather", "ipc", "csv"
#' @param keep_in_memory Keep data in memory after saving?
#' @return Modified model
#' 
#' @export
save_mapping <- function(model, mapping_name, format = "parquet", keep_in_memory = FALSE) {
  stopifnot(inherits(model, "multimod"))
  
  if (!mapping_name %in% names(model$mappings)) {
    stop("Mapping not found: ", mapping_name)
  }
  
  mapping <- model$mappings[[mapping_name]]
  
  # Get data
  if (is.null(mapping$data) || nrow(mapping$data) == 0) {
    warning("No data to save for mapping: ", mapping_name)
    return(model)
  }
  
  # Determine path
  path <- get_data_path(mapping, base_path = model$base_path)
  if (is.null(path)) {
    # Generate default path
    if (is.null(model$base_path)) {
      stop("No base_path set in model and no path set for mapping")
    }
    path <- file.path(model$base_path, "mappings", mapping_name)
    mapping <- set_data_path(mapping, file.path("mappings", mapping_name))
  } else if (!grepl("^[A-Za-z]:|^/|^\\\\", path)) {
    # Relative path - prepend base_path
    path <- file.path(model$base_path, path)
  }
  
  # Save to disk
  save_arrow_data(mapping$data, path, format = format)
  
  # Update metadata
  if (is.null(mapping$misc)) mapping$misc <- list()
  mapping$misc$onDisk <- list(
    data = list(
      dim = dim(mapping$data),
      nrow = nrow(mapping$data),
      size = format(object.size(mapping$data), units = "auto")
    )
  )
  
  # Free memory if requested
  if (!keep_in_memory) {
    mapping$data <- data.frame()
    mapping$misc$inMemory <- FALSE
  } else {
    mapping$misc$inMemory <- TRUE
  }
  
  model$mappings[[mapping_name]] <- mapping
  model
}

#' Load all parameters into memory
#'
#' @param model Multimod model
#' @return Modified model
#' 
#' @export
load_all_parameters <- function(model) {
  for (param_name in names(model$parameters)) {
    model <- load_parameter(model, param_name)
  }
  model
}

#' Load all mappings into memory
#'
#' @param model Multimod model
#' @return Modified model
#' 
#' @export
load_all_mappings <- function(model) {
  for (mapping_name in names(model$mappings)) {
    model <- load_mapping(model, mapping_name)
  }
  model
}

#' Save all parameters to disk
#'
#' @param model Multimod model
#' @param format Data format
#' @param keep_in_memory Keep data in memory after saving?
#' @return Modified model
#' 
#' @export
save_all_parameters <- function(model, format = "parquet", keep_in_memory = FALSE) {
  for (param_name in names(model$parameters)) {
    model <- save_parameter(model, param_name, format = format, keep_in_memory = keep_in_memory)
  }
  model
}

#' Save all mappings to disk
#'
#' @param model Multimod model
#' @param format Data format
#' @param keep_in_memory Keep data in memory after saving?
#' @return Modified model
#' 
#' @export
save_all_mappings <- function(model, format = "parquet", keep_in_memory = FALSE) {
  for (mapping_name in names(model$mappings)) {
    model <- save_mapping(model, mapping_name, format = format, keep_in_memory = keep_in_memory)
  }
  model
}

#' Add data to a set
#'
#' @param model Multimod model
#' @param set_name Name of set
#' @param members Character vector of set members
#' @return Modified model
#' 
#' @export
add_set_data <- function(model, set_name, members) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  
  if (!set_name %in% names(model$sets)) {
    stop("Set not found: ", set_name)
  }
  
  model$sets[[set_name]]$data <- unique(sort(as.character(members)))
  model
}


#' Populate sets from model's own mappings and parameters
#'
#' @param model Multimod model
#' @param load_data Logical. Load data from disk if needed?
#' @return Modified model with set data populated
#' 
#' @details
#' Extracts unique members for each set from the model's own mappings
#' and parameters. Useful when model already has data loaded.
#' 
#' @export
populate_sets_from_data <- function(model, load_data = FALSE) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  
  cat("Populating sets from model data...\n")
  
  # Initialize set data storage
  set_members <- list()
  for (set_name in names(model$sets)) {
    set_members[[set_name]] <- character()
  }
  
  # Extract from mappings
  if (!is.null(model$mappings) && length(model$mappings) > 0) {
    for (m in model$mappings) {
      m_data <- if (load_data) {
        get_lazy_data(m, base_path = model$base_path, collect = TRUE)
      } else {
        m$data
      }
      
      if (!is.null(m_data) && nrow(m_data) > 0) {
        # Mappings use 'dims' not 'sets'
        m_dims <- if (!is.null(m$dims)) {
          if (is.character(m$dims)) m$dims else as.character(m$dims)
        } else {
          NULL
        }
        if (!is.null(m_dims) && length(m_dims) > 0) {
          for (i in seq_along(m_dims)) {
            set_name <- m_dims[i]
            if (set_name %in% names(set_members) && i <= ncol(m_data)) {
              members <- as.character(m_data[[i]])
              set_members[[set_name]] <- c(set_members[[set_name]], members)
            }
          }
        }
      }
    }
  }
  
  # Extract from parameters
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    for (p in model$parameters) {
      p_data <- if (load_data) {
        get_lazy_data(p, base_path = model$base_path, collect = TRUE)
      } else {
        p$data
      }
      
      if (!is.null(p_data) && nrow(p_data) > 0) {
        p_dims <- if (is.character(p$dims)) p$dims else as.character(p$dims)
        if (!is.null(p_dims) && length(p_dims) > 0) {
          for (i in seq_along(p_dims)) {
            dim_name <- p_dims[i]
            if (dim_name %in% names(set_members) && i <= ncol(p_data)) {
              members <- as.character(p_data[[i]])
              set_members[[dim_name]] <- c(set_members[[dim_name]], members)
            }
          }
        }
      }
    }
  }
  
  # Update model sets with unique members
  n_populated <- 0
  for (set_name in names(set_members)) {
    members <- unique(set_members[[set_name]])
    if (length(members) > 0) {
      model$sets[[set_name]]$data <- sort(members)
      n_populated <- n_populated + 1
      cat("  ", set_name, ":", length(members), "members\n")
    }
  }
  
  cat("Populated", n_populated, "of", length(model$sets), "sets\n")
  model
}




















#' Export import log to CSV file
#'
#' @keywords internal
#' @export
export_import_log <- function(import_log, log_file) {
  # Flatten the log structure into a data frame
  rows <- list()
  
  # Sets
  if (!is.null(import_log$sets)) {
    for (name in names(import_log$sets)) {
      entry <- import_log$sets[[name]]
      rows[[length(rows) + 1]] <- data.frame(
        category = "set",
        multimod_name = entry$multimod_name,
        scenario_name = NA,
        status = entry$status,
        n_members = entry$n_members,
        dims = NA,
        defVal = NA,
        n_rows = NA,
        path = NA,
        sources = if (!is.na(entry$source_parameters[1])) 
          paste(entry$source_parameters, collapse = "; ") else NA,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Parameters
  if (!is.null(import_log$parameters)) {
    for (name in names(import_log$parameters)) {
      entry <- import_log$parameters[[name]]
      rows[[length(rows) + 1]] <- data.frame(
        category = "parameter",
        multimod_name = if (!is.na(entry$multimod_name)) entry$multimod_name else NA,
        scenario_name = entry$scenario_name,
        status = entry$status,
        n_members = NA,
        dims = entry$dims,
        defVal = entry$defVal,
        n_rows = entry$n_rows,
        path = entry$path,
        sources = NA,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Mappings
  if (!is.null(import_log$mappings)) {
    for (name in names(import_log$mappings)) {
      entry <- import_log$mappings[[name]]
      rows[[length(rows) + 1]] <- data.frame(
        category = "mapping",
        multimod_name = if (!is.na(entry$multimod_name)) entry$multimod_name else NA,
        scenario_name = entry$scenario_name,
        status = entry$status,
        n_members = NA,
        dims = entry$dims,
        defVal = entry$defVal,
        n_rows = entry$n_rows,
        path = entry$path,
        sources = NA,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Bounds
  if (!is.null(import_log$bounds)) {
    for (name in names(import_log$bounds)) {
      entry <- import_log$bounds[[name]]
      # Create two rows for Lo and Up
      rows[[length(rows) + 1]] <- data.frame(
        category = "bounds_lo",
        multimod_name = entry$multimod_lo,
        scenario_name = entry$scenario_name,
        status = entry$status_lo,
        n_members = NA,
        dims = entry$dims,
        defVal = as.character(entry$defVal_lo),
        n_rows = NA,
        path = entry$path,
        sources = NA,
        stringsAsFactors = FALSE
      )
      rows[[length(rows) + 1]] <- data.frame(
        category = "bounds_up",
        multimod_name = entry$multimod_up,
        scenario_name = entry$scenario_name,
        status = entry$status_up,
        n_members = NA,
        dims = entry$dims,
        defVal = as.character(entry$defVal_up),
        n_rows = NA,
        path = entry$path,
        sources = NA,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine and write
  if (length(rows) > 0) {
    log_df <- do.call(rbind, rows)
    write.csv(log_df, log_file, row.names = FALSE)
  }
}











