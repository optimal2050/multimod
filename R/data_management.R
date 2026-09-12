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


#' Collect scenario parameter data into memory
#'
#' @param ert_param energyRt parameter object
#' @return data.frame with parameter values (may be empty)
#'
#' @keywords internal
collect_scenario_parameter_data <- function(ert_param, scenario = NULL) {
  if (!is.null(ert_param@data) && nrow(ert_param@data) > 0) {
    return(.unfold_param_data(as.data.frame(ert_param@data), ert_param, scenario))
  }

  param_path <- ert_param@misc$path
  if (!is.null(param_path) && (dir.exists(param_path) || file.exists(param_path))) {
    data <- tryCatch(
      load_arrow_data(param_path, collect = TRUE),
      error = function(e) {
        warning("Failed to load data for ", ert_param@name, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(data) && nrow(data) > 0) {
      return(.unfold_param_data(as.data.frame(data), ert_param, scenario))
    }
  }

  data.frame()
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

#' Populate sets from energyRt scenario
#'
#' @param model Multimod model
#' @param scenario energyRt scenario object
#' @return Modified model with set data populated
#' 
#' @details
#' Extracts unique members for each set by scanning through all parameters
#' and mappings in the energyRt scenario. For each dimension that matches
#' a set name, collects unique values.
#' 
#' @export
populate_sets_from_scenario <- function(model, scenario) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  
  if (!inherits(scenario, "scenario")) {
    stop("scenario must be an energyRt scenario object")
  }
  
  cat("Populating sets from energyRt scenario...\n")
  
  # Initialize set data storage
  set_members <- list()
  for (set_name in names(model$sets)) {
    set_members[[set_name]] <- character()
  }
  
  # Scan through scenario parameters
  param_names <- names(scenario@modInp@parameters)
  cat("  Scanning", length(param_names), "parameters...\n")
  
  for (pname in param_names) {
    ert_param <- scenario@modInp@parameters[[pname]]
    
    # Get dimension names
    dims <- ert_param@dimSets
    if (length(dims) == 0) next
    
    # Load data if available
    path <- ert_param@misc$path
    if (!is.null(path) && (dir.exists(path) || file.exists(path))) {
      # Path exists - load it
      data <- tryCatch(
        load_arrow_data(path, collect = TRUE),
        error = function(e) {
          cat("  Warning: could not load", pname, "-", conditionMessage(e), "\n")
          NULL
        }
      )
      if (!is.null(data) && nrow(data) > 0) {
        # Extract members for each dimension
        for (i in seq_along(dims)) {
          dim_name <- dims[i]
          if (dim_name %in% names(set_members) && i <= ncol(data)) {
            members <- as.character(data[[i]])
            set_members[[dim_name]] <- c(set_members[[dim_name]], members)
          }
        }
      }
    } else if (!is.null(ert_param@data) && nrow(ert_param@data) > 0) {
      # Data in memory
      data <- ert_param@data
      for (i in seq_along(dims)) {
        dim_name <- dims[i]
        if (dim_name %in% names(set_members) && i <= ncol(data)) {
          members <- as.character(data[[i]])
          set_members[[dim_name]] <- c(set_members[[dim_name]], members)
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


#' Split bounds-style parameter into low/high tables
#'
#' @param ert_param energyRt parameter object of type "bounds"
#' @return List with lo/up data.frames (each containing dims + value)
#'
#' @keywords internal
split_bounds_parameter_data <- function(ert_param, scenario = NULL) {
  # Bounds take their own import path, so the unfold has to be threaded here
  # too - a folded pTechAf reaches the matrix as pTechAfLo / pTechAfUp.
  data <- collect_scenario_parameter_data(ert_param, scenario = scenario)
  if (nrow(data) == 0) {
    return(NULL)
  }

  if (!"type" %in% names(data)) {
    warning("Bounds parameter ", ert_param@name, " lacks a 'type' column")
    return(NULL)
  }

  dims <- as.character(ert_param@dimSets)
  available_dims <- intersect(dims, names(data))
  if (length(available_dims) == 0) {
    warning("Bounds parameter ", ert_param@name, " has no matching dimension columns")
    return(NULL)
  }

  value_col <- if ("value" %in% names(data)) "value" else tail(names(data), 1)
  dtype <- tolower(as.character(data$type))

  select_subset <- function(indices) {
    if (!any(indices)) {
      return(data.frame())
    }
    subset <- data[indices, c(available_dims, value_col), drop = FALSE]
    colnames(subset) <- c(available_dims, "value")
    subset$value <- suppressWarnings(as.numeric(subset$value))
    subset
  }

  list(
    lo = select_subset(dtype %in% c("lo", "lower", "min")),
    up = select_subset(dtype %in% c("up", "upper", "max")),
    dims = available_dims
  )
}


#' Attach bounds data to model parameters
#'
#' @param model Multimod model
#' @param scenario_param energyRt bounds parameter
#' @param base_name Base parameter name (without Lo/Up suffix)
#' @return List with model and diagnostic info
#'
#' @keywords internal
apply_bounds_to_model <- function(model, scenario_param, base_name, scenario = NULL) {
  lo_name <- paste0(base_name, "Lo")
  up_name <- paste0(base_name, "Up")
  if (is.null(model$parameters)) model$parameters <- list()

  # Validate bounds parameter has two default values
  defaults <- scenario_param@defVal
  if (length(defaults) < 2) {
    stop("Bounds parameter '", base_name, "' must have exactly 2 default values (lo, up), but has ", 
         length(defaults), ": [", paste(defaults, collapse = ", "), "]")
  }
  
  lo_default <- defaults[1]
  up_default <- defaults[2]

  attached <- character()
  missing <- character()
  issues <- character()
  
  # Try to split data if available
  splits <- split_bounds_parameter_data(scenario_param, scenario = scenario)
  has_data <- !is.null(splits)
  
  # If no data available, create empty data frames
  if (!has_data) {
    splits <- list(
      lo = data.frame(),
      up = data.frame(),
      dims = as.character(scenario_param@dimSets)
    )
    issues <- c(issues, paste0(base_name, " (no data to split)"))
  }

  assign_side <- function(target_name, data_block, default_value, side_label) {
    if (!target_name %in% names(model$parameters)) {
      missing <<- c(missing, target_name)
      return()
    }
    param_obj <- model$parameters[[target_name]]
    
    # Set data
    param_obj$data <- data_block
    
    # Get dimension names from both model and scenario
    scenario_dims <- as.character(scenario_param@dimSets)
    
    if (is.null(param_obj$dims) || length(param_obj$dims) == 0) {
      # No existing dims - use scenario dims
      param_obj$dims <- ast_dims(scenario_dims)
    } else {
      # Ensure dims are a dims object
      if (!inherits(param_obj$dims, "dims")) {
        param_obj$dims <- ast_dims(param_obj$dims)
      }
      
      # Check for dimension name differences (but allow aliases)
      model_dims <- sapply(param_obj$dims, function(d) {
        if (inherits(d, "symbol")) d$name else as.character(d)
      })
      
      if (!identical(model_dims, scenario_dims)) {
        # Check if this is just an alias situation (e.g., src/dst for region)
        # If model has duplicate dimension names, it's likely an alias case in scenario
        has_duplicates_in_model <- length(model_dims) != length(unique(model_dims))
        
        if (has_duplicates_in_model && length(model_dims) == length(scenario_dims)) {
          # Model has duplicates (e.g., region, region), scenario likely has aliases (e.g., src, dst)
          # This is the expected alias pattern - no warning needed
        } else if (length(model_dims) != length(scenario_dims)) {
          warning("Dimension count mismatch for ", target_name,
                  ": model has ", length(model_dims), " dims, scenario has ", length(scenario_dims))
        } else {
          # Genuine mismatch - warn
          warning("Dimension mismatch for ", target_name, 
                  ": model has [", paste(model_dims, collapse=","), 
                  "], scenario has [", paste(scenario_dims, collapse=","), "]")
        }
      }
    }
    
    # Set default value
    param_obj$defVal <- default_value
    
    # Set metadata
    if (is.null(param_obj$misc)) param_obj$misc <- list()
    param_obj$misc$inMemory <- TRUE
    param_obj$misc$source <- scenario_param@name
    param_obj$misc$boundType <- side_label
    
    # Store path reference for lazy loading if data is on disk
    if (!is.null(scenario_param@misc$path)) {
      param_obj$misc$source_path <- scenario_param@misc$path
    }
    
    # Ensure value column is named correctly
    if (!"value" %in% names(param_obj$data) && nrow(param_obj$data) > 0) {
      colnames(param_obj$data)[ncol(param_obj$data)] <- "value"
    }
    
    # Use super-assignment to modify parent scope
    model$parameters[[target_name]] <<- param_obj
    attached <<- c(attached, target_name)
  }

  assign_side(lo_name, splits$lo, lo_default, "lo")
  assign_side(up_name, splits$up, up_default, "up")

  if (nrow(splits$lo) == 0 && is.na(lo_default)) {
    issues <- c(issues, paste0(base_name, " (missing lo data)"))
  }
  if (nrow(splits$up) == 0 && is.na(up_default)) {
    issues <- c(issues, paste0(base_name, " (missing up data)"))
  }

  list(model = model, attached = attached, missing = missing, issues = issues)
}


#' Import energyRt scenario data into a multimod model
#'
#' Combines set population, parameter linking, and special handling for
#' bounds-style parameters (stored as single objects in energyRt but split
#' into *Lo/*Up entries inside multimod models). Creates a detailed import
#' log stored in model$misc$data_import_log.
#'
#' @param model Multimod model
#' @param scenario energyRt scenario object
#' @param inMemory Logical. Load parameter data into memory?
#' @param log_file Optional path to export import log as CSV
#' @return Modified model with scenario data linked and import log
#' @export
import_energyRt_data <- function(model, scenario, inMemory = scenario@inMemory, log_file = NULL) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  if (!inherits(scenario, "scenario")) {
    stop("scenario must be an energyRt scenario object")
  }

  if (is.null(inMemory)) inMemory <- FALSE

  # A folded scenario must be unfolded as it is read, and unfolding needs the
  # data in hand. The lazy path loads straight from the parameter store later,
  # with no scenario to resolve membership against, so it would read the
  # wildcard rows back and silently drop them on the join.
  if (!isTRUE(inMemory) && .scenario_is_folded(scenario)) {
    stop("This scenario was interpolated with fold = TRUE, so its parameters ",
         "carry wildcard (NA) index values that must be expanded on read.\n",
         "  Pass inMemory = TRUE to import_energyRt_data(); the lazy path ",
         "cannot expand them and would build a model that solves to a wrong ",
         "answer without erroring.", call. = FALSE)
  }

  cat("Importing energyRt scenario data...\n")
  
  # Initialize import log
  import_log <- list()
  
  # Step 1: Populate sets and track sources
  set_log <- populate_sets_with_log(model, scenario)
  import_log$sets <- set_log$log
  model <- set_log$model
  
  # Step 1b: Declare this scenario's user-constraint support symbols
  # (mCns*/pCns*/mCosts*/pCosts*). They are per-scenario, so energyRt.gms does
  # not declare them, and step 2 only links symbols the model already has.
  model <- declare_user_constraint_symbols(model, scenario)

  # Step 2: Link regular parameters and mappings
  link_result <- link_scenario_data_with_log(model, scenario, inMemory = inMemory)
  import_log$parameters <- link_result$param_log
  import_log$mappings <- link_result$mapping_log
  model <- link_result$model

  # Step 2b: Parse the compiled user constraints and user costs into equations.
  model <- add_user_constraints(model, scenario)

  # Step 3: Handle bounds parameters
  scenario_params <- scenario@modInp@parameters
  bounds_names <- names(Filter(function(p) {
    identical(as.character(p@type), "bounds")
  }, scenario_params))

  if (length(bounds_names) > 0) {
    cat("  Processing", length(bounds_names), "bounds parameters...\n")
    bounds_result <- process_bounds_with_log(model, scenario_params, bounds_names,
                                            scenario = scenario)
    import_log$bounds <- bounds_result$log
    model <- bounds_result$model
    
    # Report bounds processing
    cat("    Bound datasets attached:", bounds_result$attached_count, "\n")
    if (length(bounds_result$missing_targets) > 0) {
      cat("    Missing target parameters for bounds:\n")
      for (nm in names(bounds_result$missing_targets)) {
        cat("      -", nm, "->", paste(bounds_result$missing_targets[[nm]], collapse = ", "), "\n")
      }
    }
  }
  
  # Step 4: Identify unmatched elements
  import_log$unmatched <- find_unmatched_elements(model, scenario, import_log)
  
  # Store log in model
  if (is.null(model$misc)) model$misc <- list()
  model$misc$data_import_log <- import_log
  model$misc$data_import_timestamp <- Sys.time()
  
  # Export to CSV if requested
  if (!is.null(log_file)) {
    export_import_log(import_log, log_file)
    cat("  Import log exported to:", log_file, "\n")
  }
  
  # Print summary
  print_import_summary(import_log)

  model
}


#' Populate sets and track data sources
#'
#' @param model Multimod model
#' @param scenario energyRt scenario object
#' @return List with model and log
#'
#' @keywords internal
populate_sets_with_log <- function(model, scenario) {
  cat("Populating sets from energyRt scenario...\n")
  
  # Initialize set data storage and log
  set_members <- list()
  set_sources <- list()  # Track which parameters contributed to each set
  
  for (set_name in names(model$sets)) {
    set_members[[set_name]] <- character()
    set_sources[[set_name]] <- character()
  }
  
  # Scan through scenario parameters
  param_names <- names(scenario@modInp@parameters)
  cat("  Scanning", length(param_names), "parameters...\n")
  
  for (pname in param_names) {
    ert_param <- scenario@modInp@parameters[[pname]]
    
    # Get dimension names
    dims <- ert_param@dimSets
    if (length(dims) == 0) next
    
    # Load data if available
    path <- ert_param@misc$path
    data <- NULL
    
    if (!is.null(path) && (dir.exists(path) || file.exists(path))) {
      data <- tryCatch(
        load_arrow_data(path, collect = TRUE),
        error = function(e) {
          cat("  Warning: could not load", pname, "-", conditionMessage(e), "\n")
          NULL
        }
      )
    } else if (!is.null(ert_param@data) && nrow(ert_param@data) > 0) {
      data <- ert_param@data
    }
    
    # Extract members for each dimension
    if (!is.null(data) && nrow(data) > 0) {
      for (i in seq_along(dims)) {
        dim_name <- dims[i]
        if (dim_name %in% names(set_members) && i <= ncol(data)) {
          members <- as.character(data[[i]])
          set_members[[dim_name]] <- c(set_members[[dim_name]], members)
          set_sources[[dim_name]] <- c(set_sources[[dim_name]], pname)
        }
      }
    }
  }
  
  # Update model sets with unique members and create log
  n_populated <- 0
  log_entries <- list()
  
  for (set_name in names(set_members)) {
    members <- unique(set_members[[set_name]])
    sources <- unique(set_sources[[set_name]])
    
    if (length(members) > 0) {
      model$sets[[set_name]]$data <- sort(members)
      n_populated <- n_populated + 1
      cat("  ", set_name, ":", length(members), "members\n")
    }
    
    log_entries[[set_name]] <- list(
      multimod_name = set_name,
      type = "set",
      status = if (length(members) > 0) "populated" else "empty",
      n_members = length(members),
      source_parameters = if (length(sources) > 0) sources else NA,
      n_sources = length(sources)
    )
  }
  
  cat("Populated", n_populated, "of", length(model$sets), "sets\n")
  
  list(
    model = model,
    log = log_entries
  )
}


#' Link scenario data with logging
#'
#' @param model Multimod model
#' @param scenario energyRt scenario object
#' @param inMemory Logical
#' @return List with model and logs
#'
#' @keywords internal
link_scenario_data_with_log <- function(model, scenario, inMemory = FALSE) {
  param_names <- names(scenario@modInp@parameters)
  
  param_log <- list()
  mapping_log <- list()
  
  for (pname in param_names) {
    ert_param <- scenario@modInp@parameters[[pname]]
    param_type <- as.character(ert_param@type)
    
    # Skip bounds - they'll be handled separately
    if (param_type == "bounds") {
      next
    }
    
    if (param_type == "map") {
      # Mapping
      if (pname %in% names(model$mappings)) {
        model$mappings[[pname]] <- convert_energyrt_parameter(
          ert_param,
          model$mappings[[pname]],
          inMemory = inMemory,
          scenario = scenario
        )
        
        mapping_log[[pname]] <- create_link_log_entry(
          multimod_name = pname,
          scenario_name = pname,
          type = "mapping",
          status = "linked",
          ert_param = ert_param,
          inMemory = inMemory
        )
      } else {
        mapping_log[[pname]] <- create_link_log_entry(
          multimod_name = NA,
          scenario_name = pname,
          type = "mapping",
          status = "unmatched_in_multimod",
          ert_param = ert_param,
          inMemory = inMemory
        )
      }
    } else if (param_type == "numpar") {
      # Numeric parameter
      if (pname %in% names(model$parameters)) {
        model$parameters[[pname]] <- convert_energyrt_parameter(
          ert_param,
          model$parameters[[pname]],
          inMemory = inMemory,
          scenario = scenario
        )
        
        param_log[[pname]] <- create_link_log_entry(
          multimod_name = pname,
          scenario_name = pname,
          type = "parameter",
          status = "linked",
          ert_param = ert_param,
          inMemory = inMemory
        )
      } else {
        param_log[[pname]] <- create_link_log_entry(
          multimod_name = NA,
          scenario_name = pname,
          type = "parameter",
          status = "unmatched_in_multimod",
          ert_param = ert_param,
          inMemory = inMemory
        )
      }
    }
  }
  
  cat("  Mappings linked: ", sum(sapply(mapping_log, function(x) x$status == "linked")), "\n")
  cat("  Parameters linked:", sum(sapply(param_log, function(x) x$status == "linked")), "\n")
  
  list(
    model = model,
    param_log = param_log,
    mapping_log = mapping_log
  )
}
# "linked" counts symbols attached, not data reachable. A run in which every
# symbol links and every one carries zero rows builds a 1x1 model that solves
# cleanly and returns a plausible wrong answer -- the failure this reporting
# exists to make visible.
.report_rows <- function(entries, what) {
  linked <- Filter(function(x) identical(x$status, "linked"), entries)
  if (!length(linked)) return(invisible(NULL))
  rows <- vapply(linked, function(x) {
    n <- x$n_rows
    if (is.null(n) || is.na(n)) 0 else as.numeric(n)
  }, numeric(1))
  empty <- sum(rows == 0)
  reachable <- vapply(linked, function(x) isTRUE(x$resolvable), logical(1))
  cat(sprintf("            %s rows across %d linked %s, %d empty, %d reachable\n",
              format(sum(rows), big.mark = ","), length(linked), what, empty,
              sum(reachable)))
  # Not a warning. A model in which nothing is reachable still builds, still
  # solves, and still returns a plausible number - built entirely from default
  # values. That failure has to stop the import, not decorate its log.
  if (!any(reachable)) {
    stop(sprintf(paste0(
      "Import failed: not one of the %d linked %s can be reached - no rows in ",
      "memory and no on-disk reference. The model would be built from default ",
      "values alone and would solve to a plausible wrong answer.\n",
      "  If the scenario is stored on disk, load it first, or pass ",
      "inMemory = TRUE to import_energyRt_data()."),
      length(linked), what), call. = FALSE)
  }
  invisible(sum(rows))
}



#' Rows an energyRt parameter holds, attached or detached
#'
#' A detached (on-disk) parameter keeps its row count in the `misc$onDisk`
#' summary; its `@data` slot is empty by design.
#' @keywords internal
.ert_n_rows <- function(ert_param) {
  if (!is.null(ert_param@data) && nrow(ert_param@data) > 0) {
    return(nrow(ert_param@data))
  }
  d <- .ondisk_dim(ert_param@misc$onDisk)
  if (!is.null(d)) return(as.integer(d[1]))
  0L
}

#' Create log entry for parameter/mapping link
#'
#' @keywords internal
create_link_log_entry <- function(multimod_name, scenario_name, type, status,
                                   ert_param, inMemory) {
  list(
    multimod_name = multimod_name,
    scenario_name = scenario_name,
    type = type,
    status = status,
    dims = paste(ert_param@dimSets, collapse = ", "),
    n_dims = length(ert_param@dimSets),
    defVal = if (!is.null(ert_param@defVal)) paste(ert_param@defVal, collapse = ", ") else NA,
    has_data = .ert_n_rows(ert_param) > 0,
    # Row count as stored, not as attached. An on-disk parameter's @data slot is
    # detached and always reports 0, which made the import summary announce
    # "0 rows, all empty" for a scenario whose data was entirely readable - and
    # so made the real version of that failure impossible to notice.
    n_rows = .ert_n_rows(ert_param),
    path = if (!is.null(ert_param@misc$path)) ert_param@misc$path else NA,
    # Whether the data can be reached at all - in memory now, or on disk via
    # the per-object reference that get_lazy_data() resolves. An on-disk
    # scenario legitimately reports n_rows = 0 here while being perfectly
    # readable, so row counts alone cannot tell a lazy link from a lost one.
    resolvable = (!is.null(ert_param@data) && nrow(ert_param@data) > 0) ||
      !is.null(ert_param@misc$path) || isTRUE(ert_param@misc$onDisk),
    inMemory = inMemory
  )
}


#' Process bounds parameters with logging
#'
#' @keywords internal
process_bounds_with_log <- function(model, scenario_params, bounds_names,
                                    scenario = NULL) {
  log_entries <- list()
  attached_total <- 0
  missing_targets <- list()
  
  for (pname in bounds_names) {
    ert_param <- scenario_params[[pname]]
    result <- apply_bounds_to_model(model, ert_param, pname, scenario = scenario)
    model <- result$model
    
    # Create log entry for this bounds parameter
    lo_name <- paste0(pname, "Lo")
    up_name <- paste0(pname, "Up")
    
    log_entries[[pname]] <- list(
      scenario_name = pname,
      type = "bounds",
      multimod_lo = lo_name,
      multimod_up = up_name,
      status_lo = if (lo_name %in% result$attached) "split_linked" else "unmatched_in_multimod",
      status_up = if (up_name %in% result$attached) "split_linked" else "unmatched_in_multimod",
      dims = paste(ert_param@dimSets, collapse = ", "),
      n_dims = length(ert_param@dimSets),
      defVal_lo = if (length(ert_param@defVal) >= 1) ert_param@defVal[1] else NA,
      defVal_up = if (length(ert_param@defVal) >= 2) ert_param@defVal[2] else ert_param@defVal[1],
      path = if (!is.null(ert_param@misc$path)) ert_param@misc$path else NA,
      issues = if (length(result$issues) > 0) paste(result$issues, collapse = "; ") else NA
    )
    
    attached_total <- attached_total + length(result$attached)
    if (length(result$missing) > 0) {
      missing_targets[[pname]] <- result$missing
    }
  }
  
  list(
    model = model,
    log = log_entries,
    attached_count = attached_total,
    missing_targets = missing_targets
  )
}


#' Find unmatched elements between model and scenario
#'
#' @keywords internal
find_unmatched_elements <- function(model, scenario, import_log) {
  unmatched <- list()
  
  # Find multimod parameters not in scenario
  multimod_params <- names(model$parameters)
  linked_params <- names(import_log$parameters)
  
  # Also check bounds splits
  bounds_targets <- unlist(lapply(import_log$bounds, function(b) {
    c(b$multimod_lo, b$multimod_up)
  }))
  
  unmatched$multimod_params_without_data <- setdiff(
    multimod_params,
    c(linked_params, bounds_targets)
  )
  
  # Find multimod mappings not in scenario
  multimod_mappings <- names(model$mappings)
  linked_mappings <- names(import_log$mappings)
  
  unmatched$multimod_mappings_without_data <- setdiff(
    multimod_mappings,
    linked_mappings
  )
  
  # Find scenario parameters not matched
  unmatched$scenario_params_unmatched <- names(Filter(function(x) {
    !is.na(x$status) && x$status == "unmatched_in_multimod"
  }, import_log$parameters))
  
  unmatched$scenario_mappings_unmatched <- names(Filter(function(x) {
    !is.na(x$status) && x$status == "unmatched_in_multimod"
  }, import_log$mappings))
  
  unmatched$scenario_bounds_unmatched <- names(Filter(function(x) {
    (!is.na(x$status_lo) && x$status_lo == "unmatched_in_multimod") ||
    (!is.na(x$status_up) && x$status_up == "unmatched_in_multimod")
  }, import_log$bounds))
  
  unmatched
}


#' Export import log to CSV file
#'
#' @keywords internal
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


#' Print import summary
#'
#' @keywords internal
print_import_summary <- function(import_log) {
  cat("\n=== Data Import Summary ===\n")
  
  # Sets
  if (!is.null(import_log$sets)) {
    n_populated <- sum(sapply(import_log$sets, function(x) x$status == "populated"))
    cat("Sets:      ", n_populated, "/", length(import_log$sets), "populated\n")
  }
  
  # Parameters
  if (!is.null(import_log$parameters)) {
    n_linked <- sum(sapply(import_log$parameters, function(x) x$status == "linked"))
    cat("Parameters:", n_linked, "/", length(import_log$parameters), "linked\n")
    .report_rows(import_log$parameters, "parameters")
  }
  
  # Mappings
  if (!is.null(import_log$mappings)) {
    n_linked <- sum(sapply(import_log$mappings, function(x) x$status == "linked"))
    cat("Mappings:  ", n_linked, "/", length(import_log$mappings), "linked\n")
    .report_rows(import_log$mappings, "mappings")
  }
  
  # Bounds
  if (!is.null(import_log$bounds)) {
    n_lo_linked <- sum(sapply(import_log$bounds, function(x) x$status_lo == "split_linked"))
    n_up_linked <- sum(sapply(import_log$bounds, function(x) x$status_up == "split_linked"))
    cat("Bounds:    ", length(import_log$bounds), "parameters split into",
        n_lo_linked, "Lo +", n_up_linked, "Up\n")
  }
  
  # Unmatched
  if (!is.null(import_log$unmatched)) {
    um <- import_log$unmatched
    if (length(um$multimod_params_without_data) > 0) {
      cat("\nWarning:", length(um$multimod_params_without_data), 
          "multimod parameters have no data\n")
    }
    if (length(um$scenario_params_unmatched) > 0) {
      cat("Warning:", length(um$scenario_params_unmatched), 
          "scenario parameters not matched in multimod\n")
    }
  }
  
  cat("===========================\n\n")
}


#' Get data import log from model
#'
#' @param model Multimod model with import log
#' @return Import log list or NULL if not available
#' @export
get_import_log <- function(model) {
  if (is.null(model$misc$data_import_log)) {
    message("No import log found. Run import_energyRt_data() to create one.")
    return(NULL)
  }
  model$misc$data_import_log
}


#' Export import log to CSV
#'
#' @param model Multimod model with import log
#' @param file Path to CSV file
#' @export
export_import_log_from_model <- function(model, file) {
  log <- get_import_log(model)
  if (is.null(log)) {
    stop("No import log available")
  }
  export_import_log(log, file)
  cat("Import log exported to:", file, "\n")
  invisible(file)
}


#' Show bounds parameter mapping
#'
#' @param model Multimod model with import log
#' @return Data frame showing bounds parameter splits
#' @export
show_bounds_mapping <- function(model) {
  log <- get_import_log(model)
  if (is.null(log) || is.null(log$bounds)) {
    message("No bounds mapping found")
    return(NULL)
  }
  
  rows <- lapply(names(log$bounds), function(name) {
    entry <- log$bounds[[name]]
    data.frame(
      scenario_param = entry$scenario_name,
      multimod_lo = entry$multimod_lo,
      multimod_up = entry$multimod_up,
      status_lo = entry$status_lo,
      status_up = entry$status_up,
      dims = entry$dims,
      defVal_lo = entry$defVal_lo,
      defVal_up = entry$defVal_up,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, rows)
}


#' Show unmatched elements
#'
#' @param model Multimod model with import log
#' @export
show_unmatched <- function(model) {
  log <- get_import_log(model)
  if (is.null(log) || is.null(log$unmatched)) {
    message("No unmatched elements information found")
    return(NULL)
  }
  
  um <- log$unmatched
  
  cat("\n=== Unmatched Elements ===\n\n")
  
  if (length(um$multimod_params_without_data) > 0) {
    cat("Multimod parameters without scenario data:\n")
    cat("  ", paste(um$multimod_params_without_data, collapse = ", "), "\n\n")
  }
  
  if (length(um$multimod_mappings_without_data) > 0) {
    cat("Multimod mappings without scenario data:\n")
    cat("  ", paste(um$multimod_mappings_without_data, collapse = ", "), "\n\n")
  }
  
  if (length(um$scenario_params_unmatched) > 0) {
    cat("Scenario parameters not in multimod:\n")
    cat("  ", paste(um$scenario_params_unmatched, collapse = ", "), "\n\n")
  }
  
  if (length(um$scenario_mappings_unmatched) > 0) {
    cat("Scenario mappings not in multimod:\n")
    cat("  ", paste(um$scenario_mappings_unmatched, collapse = ", "), "\n\n")
  }
  
  if (length(um$scenario_bounds_unmatched) > 0) {
    cat("Scenario bounds with missing targets in multimod:\n")
    cat("  ", paste(um$scenario_bounds_unmatched, collapse = ", "), "\n\n")
  }
  
  cat("==========================\n")
}

