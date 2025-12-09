#' Read OSeMOSYS data files
#'
#' Functions to import OSeMOSYS data in various formats (Pyomo .dat, AMPL .dat, etc.)
#' and convert to multimod format.

#' Parse OSeMOSYS .dat file (Pyomo/AMPL format)
#'
#' @param dat_file Path to .dat file
#' @param verbose Logical. Print progress messages?
#' @return List with sets and parameters as data frames
#' 
#' @details
#' Parses OSeMOSYS data files in Pyomo/AMPL format. Handles:
#' - Set declarations: `set SETNAME := value1 value2 ... ;`
#' - Parameters with dimensions: `param ParamName := key1 key2 value ;`
#' - Multi-line continuations
#' - Comments starting with #
#' 
#' @export
read_osemosys_dat <- function(dat_file, verbose = TRUE) {
  if (!file.exists(dat_file)) {
    stop("File not found: ", dat_file)
  }
  
  if (verbose) {
    cat("Reading OSeMOSYS .dat file:", dat_file, "\n")
  }
  
  # Read entire file
  lines <- readLines(dat_file, warn = FALSE)
  
  # Remove comments and empty lines
  lines <- gsub("#.*$", "", lines)  # Remove comments
  lines <- trimws(lines)  # Trim whitespace
  lines <- lines[nchar(lines) > 0]  # Remove empty lines
  
  # Concatenate into single string for easier parsing
  content <- paste(lines, collapse = " ")
  
  # Split by semicolons to get declarations
  declarations <- strsplit(content, ";")[[1]]
  declarations <- trimws(declarations)
  declarations <- declarations[nchar(declarations) > 0]
  
  if (verbose) {
    cat("  Found", length(declarations), "declarations\n")
  }
  
  # Parse each declaration
  sets <- list()
  parameters <- list()
  
  for (decl in declarations) {
    # Check if it's a set or parameter
    if (grepl("^set\\s+", decl, ignore.case = TRUE)) {
      # Parse set
      result <- parse_osemosys_set(decl)
      if (!is.null(result)) {
        sets[[result$name]] <- result$data
        if (verbose) {
          cat("  Set:", result$name, "(", length(result$data), "members )\n")
        }
      }
    } else if (grepl("^param\\s+", decl, ignore.case = TRUE)) {
      # Parse parameter
      result <- parse_osemosys_parameter(decl)
      if (!is.null(result)) {
        parameters[[result$name]] <- result$data
        if (verbose) {
          cat("  Parameter:", result$name, "(", nrow(result$data), "rows,", 
              ncol(result$data), "cols )\n")
        }
      }
    }
  }
  
  if (verbose) {
    cat("Parsed", length(sets), "sets and", length(parameters), "parameters\n")
  }
  
  list(
    sets = sets,
    parameters = parameters
  )
}


#' Parse OSeMOSYS set declaration
#'
#' @param decl Set declaration string
#' @return List with name and data, or NULL if parsing fails
#' 
#' @keywords internal
parse_osemosys_set <- function(decl) {
  # Extract set name: "set SETNAME := ..."
  match <- regexpr("set\\s+(\\w+)\\s*:=", decl, ignore.case = TRUE, perl = TRUE)
  if (match < 0) return(NULL)
  
  name_start <- attr(match, "capture.start")[1]
  name_length <- attr(match, "capture.length")[1]
  set_name <- substr(decl, name_start, name_start + name_length - 1)
  
  # Extract values after :=
  values_part <- sub("set\\s+\\w+\\s*:=\\s*", "", decl, ignore.case = TRUE)
  values_part <- trimws(values_part)
  
  # Split by whitespace
  members <- unlist(strsplit(values_part, "\\s+"))
  members <- members[nchar(members) > 0]
  
  list(
    name = set_name,
    data = members
  )
}


#' Parse OSeMOSYS parameter declaration
#'
#' @param decl Parameter declaration string
#' @return List with name and data frame, or NULL if parsing fails
#' 
#' @keywords internal
parse_osemosys_parameter <- function(decl) {
  # Extract parameter name: "param ParamName := ..."
  match <- regexpr("param\\s+(\\w+)\\s*:=", decl, ignore.case = TRUE, perl = TRUE)
  if (match < 0) return(NULL)
  
  name_start <- attr(match, "capture.start")[1]
  name_length <- attr(match, "capture.length")[1]
  param_name <- substr(decl, name_start, name_start + name_length - 1)
  
  # Extract data after :=
  data_part <- sub("param\\s+\\w+\\s*:=\\s*", "", decl, ignore.case = TRUE)
  data_part <- trimws(data_part)
  
  # Split into tokens
  tokens <- unlist(strsplit(data_part, "\\s+"))
  tokens <- tokens[nchar(tokens) > 0]
  
  if (length(tokens) == 0) {
    return(list(name = param_name, data = data.frame()))
  }
  
  # Determine dimensionality by looking at patterns
  # OSeMOSYS format: key1 key2 ... keyN value
  # Need to infer how many keys vs value
  
  # Try to parse rows - last token should be numeric value
  rows <- list()
  i <- 1
  
  # Better heuristic: Look at the first occurrence of a repeating pattern
  # OSeMOSYS data typically has consistent structure where each row has the same format
  # Strategy: Find the smallest n_cols where data divides evenly AND value column is numeric
  
  # Try different column counts starting from 2 (1 dim + 1 value)
  n_cols <- NULL
  
  for (test_cols in 2:min(10, length(tokens))) {
    if (length(tokens) %% test_cols != 0) next  # Must divide evenly
    
    # Check if last column of each row is numeric
    all_values_numeric <- TRUE
    for (row_start in seq(1, length(tokens), by = test_cols)) {
      value_idx <- row_start + test_cols - 1
      if (value_idx > length(tokens)) break
      
      value <- suppressWarnings(as.numeric(tokens[value_idx]))
      if (is.na(value)) {
        all_values_numeric <- FALSE
        break
      }
    }
    
    if (all_values_numeric) {
      # Additional check: if there are decimals, they should be in the value column
      has_decimal <- grepl("\\.", tokens)
      if (any(has_decimal)) {
        # Check if decimals are only in last column positions
        decimal_positions <- which(has_decimal)
        expected_value_positions <- seq(test_cols, length(tokens), by = test_cols)
        
        # If all decimals are in value positions, this is likely correct
        if (all(decimal_positions %in% expected_value_positions)) {
          n_cols <- test_cols
          break
        }
      } else {
        # No decimals - use this if it works
        n_cols <- test_cols
        break
      }
    }
  }
  
  if (is.null(n_cols)) {
    # Fallback: couldn't determine structure
    return(list(name = param_name, data = data.frame()))
  }
  
  n_dims <- n_cols - 1
  
  # Parse rows
  idx <- 1
  while (idx <= length(tokens)) {
    if (idx + n_cols - 1 > length(tokens)) break
    
    row_data <- tokens[idx:(idx + n_cols - 1)]
    
    # Last column should be numeric
    value <- suppressWarnings(as.numeric(row_data[n_cols]))
    if (is.na(value)) {
      # Not a valid row, skip
      idx <- idx + 1
      next
    }
    
    rows[[length(rows) + 1]] <- row_data
    idx <- idx + n_cols
  }
  
  if (length(rows) == 0) {
    return(list(name = param_name, data = data.frame()))
  }
  
  # Convert to data frame
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  
  # Set column names
  if (n_dims > 0) {
    dim_names <- paste0("dim", 1:n_dims)
    colnames(df) <- c(dim_names, "value")
  } else {
    colnames(df) <- "value"
  }
  
  # Convert value column to numeric
  df$value <- as.numeric(df$value)
  
  list(
    name = param_name,
    data = df
  )
}


#' Export OSeMOSYS data to CSV files
#'
#' @param osemosys_data List returned by read_osemosys_dat()
#' @param output_dir Directory to save CSV files
#' @param sets_subdir Subdirectory for sets (default: "sets")
#' @param params_subdir Subdirectory for parameters (default: "parameters")
#' @param verbose Logical. Print progress?
#' @return Character vector of created files
#' 
#' @export
export_osemosys_to_csv <- function(osemosys_data, 
                                    output_dir,
                                    sets_subdir = "sets",
                                    params_subdir = "parameters",
                                    verbose = TRUE) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  created_files <- character()
  
  # Export sets
  if (length(osemosys_data$sets) > 0) {
    sets_dir <- file.path(output_dir, sets_subdir)
    if (!dir.exists(sets_dir)) {
      dir.create(sets_dir, recursive = TRUE)
    }
    
    if (verbose) {
      cat("Exporting", length(osemosys_data$sets), "sets to", sets_dir, "\n")
    }
    
    for (set_name in names(osemosys_data$sets)) {
      set_data <- osemosys_data$sets[[set_name]]
      df <- data.frame(value = set_data, stringsAsFactors = FALSE)
      
      file_path <- file.path(sets_dir, paste0(set_name, ".csv"))
      write.csv(df, file_path, row.names = FALSE)
      created_files <- c(created_files, file_path)
      
      if (verbose) {
        cat("  ", set_name, "->", basename(file_path), "\n")
      }
    }
  }
  
  # Export parameters
  if (length(osemosys_data$parameters) > 0) {
    params_dir <- file.path(output_dir, params_subdir)
    if (!dir.exists(params_dir)) {
      dir.create(params_dir, recursive = TRUE)
    }
    
    if (verbose) {
      cat("Exporting", length(osemosys_data$parameters), "parameters to", params_dir, "\n")
    }
    
    for (param_name in names(osemosys_data$parameters)) {
      param_data <- osemosys_data$parameters[[param_name]]
      
      if (nrow(param_data) == 0) {
        if (verbose) {
          cat("  ", param_name, "-> (empty, skipped)\n")
        }
        next
      }
      
      file_path <- file.path(params_dir, paste0(param_name, ".csv"))
      write.csv(param_data, file_path, row.names = FALSE)
      created_files <- c(created_files, file_path)
      
      if (verbose) {
        cat("  ", param_name, "->", basename(file_path), "(", nrow(param_data), "rows )\n")
      }
    }
  }
  
  if (verbose) {
    cat("Created", length(created_files), "CSV files in", output_dir, "\n")
  }
  
  invisible(created_files)
}


#' Import OSeMOSYS data into multimod model
#'
#' @param model Multimod model (from read_gams)
#' @param dat_file Path to OSeMOSYS .dat file
#' @param name_mapping Optional named list mapping OSeMOSYS names to multimod names
#' @param verbose Logical. Print progress?
#' @return Modified model with data attached
#' 
#' @details
#' Reads OSeMOSYS .dat file and attaches data to corresponding model elements.
#' For sets, populates set$data. For parameters, attaches parameter$data.
#' 
#' Name mapping example:
#' ```
#' name_mapping <- list(
#'   TECHNOLOGY = "tech",
#'   REGION = "region",
#'   YEAR = "year"
#' )
#' ```
#' 
#' @export
import_osemosys_data <- function(model, dat_file, name_mapping = NULL, verbose = TRUE) {
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  
  # Read OSeMOSYS data
  osemosys_data <- read_osemosys_dat(dat_file, verbose = verbose)
  
  if (verbose) {
    cat("\nImporting data into model...\n")
  }
  
  # Apply name mapping if provided
  if (!is.null(name_mapping)) {
    # Map set names
    for (old_name in names(name_mapping)) {
      new_name <- name_mapping[[old_name]]
      if (old_name %in% names(osemosys_data$sets)) {
        osemosys_data$sets[[new_name]] <- osemosys_data$sets[[old_name]]
        osemosys_data$sets[[old_name]] <- NULL
      }
    }
  }
  
  # Import sets
  n_sets_imported <- 0
  if (!is.null(model$sets) && length(osemosys_data$sets) > 0) {
    for (set_name in names(osemosys_data$sets)) {
      if (set_name %in% names(model$sets)) {
        model$sets[[set_name]]$data <- osemosys_data$sets[[set_name]]
        n_sets_imported <- n_sets_imported + 1
        if (verbose) {
          cat("  Set:", set_name, "(", length(osemosys_data$sets[[set_name]]), "members )\n")
        }
      } else if (verbose) {
        cat("  Set:", set_name, "(not in model, skipped )\n")
      }
    }
  }
  
  # Import parameters
  n_params_imported <- 0
  n_params_skipped <- 0
  
  if (!is.null(model$parameters) && length(osemosys_data$parameters) > 0) {
    for (param_name in names(osemosys_data$parameters)) {
      param_data <- osemosys_data$parameters[[param_name]]
      
      if (nrow(param_data) == 0) {
        n_params_skipped <- n_params_skipped + 1
        next
      }
      
      if (param_name %in% names(model$parameters)) {
        # Get model parameter
        model_param <- model$parameters[[param_name]]
        
        # Determine dimension names
        if (!is.null(model_param$dims)) {
          # Extract dimension names from model
          dim_names <- sapply(model_param$dims, function(d) {
            if (inherits(d, "symbol")) d$name else as.character(d)
          })
          
          # Rename data columns to match (except last column which is value)
          n_dims <- ncol(param_data) - 1
          if (n_dims > 0 && n_dims <= length(dim_names)) {
            colnames(param_data)[1:n_dims] <- dim_names[1:n_dims]
          }
        }
        
        # Attach data
        model$parameters[[param_name]]$data <- param_data
        
        # Set metadata
        if (is.null(model$parameters[[param_name]]$misc)) {
          model$parameters[[param_name]]$misc <- list()
        }
        model$parameters[[param_name]]$misc$inMemory <- TRUE
        model$parameters[[param_name]]$misc$source <- "OSeMOSYS .dat file"
        
        n_params_imported <- n_params_imported + 1
        
        if (verbose) {
          cat("  Parameter:", param_name, "(", nrow(param_data), "rows )\n")
        }
      } else if (verbose) {
        cat("  Parameter:", param_name, "(not in model, skipped )\n")
        n_params_skipped <- n_params_skipped + 1
      }
    }
  }
  
  if (verbose) {
    cat("\nImport summary:\n")
    cat("  Sets imported:      ", n_sets_imported, "\n")
    cat("  Parameters imported:", n_params_imported, "\n")
    if (n_params_skipped > 0) {
      cat("  Parameters skipped: ", n_params_skipped, "(empty or not in model)\n")
    }
  }
  
  model
}


#' Convert OSeMOSYS .dat to CSV and import into model
#'
#' @param model Multimod model
#' @param dat_file Path to OSeMOSYS .dat file
#' @param csv_dir Directory to save CSV files (optional)
#' @param name_mapping Optional name mapping
#' @param verbose Logical
#' @return Modified model with data attached
#' 
#' @details
#' Convenience function that:
#' 1. Reads OSeMOSYS .dat file
#' 2. Optionally exports to CSV
#' 3. Imports data into model
#' 
#' @export
osemosys_dat_to_model <- function(model, 
                                   dat_file, 
                                   csv_dir = NULL,
                                   name_mapping = NULL,
                                   verbose = TRUE) {
  
  # Read data
  osemosys_data <- read_osemosys_dat(dat_file, verbose = verbose)
  
  # Export to CSV if requested
  if (!is.null(csv_dir)) {
    export_osemosys_to_csv(osemosys_data, csv_dir, verbose = verbose)
  }
  
  # Import into model
  model <- import_osemosys_data(model, dat_file, name_mapping = name_mapping, verbose = verbose)
  
  model
}


#' Load OSeMOSYS data from CSV directory
#'
#' @param model Multimod model
#' @param csv_dir Directory containing CSV files
#' @param sets_subdir Subdirectory with set files
#' @param params_subdir Subdirectory with parameter files
#' @param verbose Logical
#' @return Modified model with data loaded
#' 
#' @export
load_osemosys_from_csv <- function(model,
                                    csv_dir,
                                    sets_subdir = "sets",
                                    params_subdir = "parameters",
                                    verbose = TRUE) {
  
  stopifnot(inherits(model, "multimod") || inherits(model, "model"))
  
  if (!dir.exists(csv_dir)) {
    stop("Directory not found: ", csv_dir)
  }
  
  if (verbose) {
    cat("Loading OSeMOSYS data from:", csv_dir, "\n")
  }
  
  n_sets_loaded <- 0
  n_params_loaded <- 0
  
  # Load sets
  sets_dir <- file.path(csv_dir, sets_subdir)
  if (dir.exists(sets_dir)) {
    set_files <- list.files(sets_dir, pattern = "\\.csv$", full.names = TRUE)
    
    for (set_file in set_files) {
      set_name <- tools::file_path_sans_ext(basename(set_file))
      
      if (set_name %in% names(model$sets)) {
        df <- read.csv(set_file, stringsAsFactors = FALSE)
        model$sets[[set_name]]$data <- df$value
        n_sets_loaded <- n_sets_loaded + 1
        
        if (verbose) {
          cat("  Set:", set_name, "(", nrow(df), "members )\n")
        }
      }
    }
  }
  
  # Load parameters
  params_dir <- file.path(csv_dir, params_subdir)
  if (dir.exists(params_dir)) {
    param_files <- list.files(params_dir, pattern = "\\.csv$", full.names = TRUE)
    
    for (param_file in param_files) {
      param_name <- tools::file_path_sans_ext(basename(param_file))
      
      if (param_name %in% names(model$parameters)) {
        df <- read.csv(param_file, stringsAsFactors = FALSE)
        
        # Attach data
        model$parameters[[param_name]]$data <- df
        
        # Set metadata
        if (is.null(model$parameters[[param_name]]$misc)) {
          model$parameters[[param_name]]$misc <- list()
        }
        model$parameters[[param_name]]$misc$inMemory <- TRUE
        model$parameters[[param_name]]$misc$source <- param_file
        
        n_params_loaded <- n_params_loaded + 1
        
        if (verbose) {
          cat("  Parameter:", param_name, "(", nrow(df), "rows )\n")
        }
      }
    }
  }
  
  if (verbose) {
    cat("\nLoaded", n_sets_loaded, "sets and", n_params_loaded, "parameters\n")
  }
  
  model
}
