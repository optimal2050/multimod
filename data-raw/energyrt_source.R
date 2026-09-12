## Code to prepare `energyRt_source` dataset
##
## This script imports energyRt model source code from generated scenario files
## and combines include files into single model and data files for each format.
##
## Source: energyRt package BASE_UTOPIA scenario
## License: AGPL-3.0 (energyRt); multimod itself is Apache-2.0

library(usethis)

# Helper function to resolve $include directives recursively
resolve_includes <- function(file_path, base_dir = dirname(file_path),
                            processed = character(), level = 0) {

  # Prevent circular includes
  if (file_path %in% processed) {
    warning("Circular include detected: ", file_path)
    return(character())
  }
  processed <- c(processed, file_path)

  # Check if file exists
  if (!file.exists(file_path)) {
    warning("Include file not found: ", file_path)
    return(character())
  }

  # Read the file
  lines <- readLines(file_path, warn = FALSE)

  # Find $include directives (GAMS format)
  include_pattern <- "^\\s*\\$include\\s+(.+?)\\s*$"
  include_lines <- grep(include_pattern, lines, perl = TRUE)

  if (length(include_lines) == 0) {
    # No includes, return lines as-is
    return(lines)
  }

  # Process each line
  result <- character()
  current_line <- 1

  for (inc_line in include_lines) {
    # Add lines before this include
    if (inc_line > current_line) {
      result <- c(result, lines[current_line:(inc_line - 1)])
    }

    # Extract include file name
    inc_file <- sub(include_pattern, "\\1", lines[inc_line], perl = TRUE)
    inc_file <- trimws(inc_file)

    # Resolve relative path
    inc_path <- file.path(base_dir, inc_file)

    # Add comment showing original include
    result <- c(result,
                paste0("* --- Included from: ", inc_file, " ---"))

    # Recursively resolve this include
    inc_content <- resolve_includes(inc_path, base_dir, processed, level + 1)
    result <- c(result, inc_content)

    result <- c(result,
                paste0("* --- End include: ", inc_file, " ---"))

    current_line <- inc_line + 1
  }

  # Add remaining lines after last include
  if (current_line <= length(lines)) {
    result <- c(result, lines[current_line:length(lines)])
  }

  return(result)
}

# Helper function to combine GMPL data files (from .dat with data statements)
combine_gmpl_data <- function(dat_file) {
  if (!file.exists(dat_file)) {
    warning("Data file not found: ", dat_file)
    return(character())
  }

  # For GMPL, the .dat file typically contains all data
  # No include resolution needed usually
  lines <- readLines(dat_file, warn = FALSE)
  return(lines)
}

# Helper function to combine JuMP/Julia files
combine_julia_files <- function(main_file, base_dir = dirname(main_file)) {
  if (!file.exists(main_file)) {
    warning("Julia file not found: ", main_file)
    return(character())
  }

  lines <- readLines(main_file, warn = FALSE)

  # Fix 1: Add mkpath("output") before flog = open
  for (i in seq_along(lines)) {
    if (grepl('flog\\s*=\\s*open\\s*\\(\\s*"output/log\\.csv"', lines[i], perl = TRUE)) {
      # Insert mkpath before flog = open
      lines <- c(lines[1:(i-1)],
                'mkpath("output")',
                lines[i:length(lines)])
      break
    }
  }

  # Fix 2: Comment out RData-related lines and add include("data.jl")
  for (i in seq_along(lines)) {
    # Comment using RData
    if (grepl('^\\s*using\\s+RData\\s*$', lines[i], perl = TRUE)) {
      lines[i] <- paste0("# ", lines[i], "  # Using pure Julia data instead")
    }
    # Comment using DataFrames (if standalone, not part of other code)
    if (grepl('^\\s*using\\s+DataFrames\\s*$', lines[i], perl = TRUE)) {
      lines[i] <- paste0("# ", lines[i], "  # DataFrames loaded in data.jl")
    }
    # Comment dt = load line and add include
    if (grepl('dt\\s*=\\s*load\\s*\\(\\s*"data\\.RData"\\)', lines[i], perl = TRUE)) {
      lines[i] <- paste0("# ", lines[i], "  # Using pure Julia data")
      # Add include after this line
      lines <- c(lines[1:i],
                'include("data.jl")',
                lines[(i+1):length(lines)])
      break
    }
  }

  # Find include() directives (Julia format)
  include_pattern <- '^\\s*include\\s*\\(\\s*["\'](.+?)["\']\\s*\\)\\s*$'
  include_lines <- grep(include_pattern, lines, perl = TRUE)

  if (length(include_lines) == 0) {
    return(lines)
  }

  # Process each line
  result <- character()
  current_line <- 1
  processed <- character()

  for (inc_line in include_lines) {
    # Add lines before this include
    if (inc_line > current_line) {
      result <- c(result, lines[current_line:(inc_line - 1)])
    }

    # Extract include file name
    inc_file <- sub(include_pattern, "\\1", lines[inc_line], perl = TRUE)

    # Skip data.jl - it will be replaced with pure Julia version
    if (grepl('data\\.jl', inc_file, ignore.case = TRUE)) {
      # Keep the include statement but don't expand it
      result <- c(result, lines[inc_line])
      current_line <- inc_line + 1
      next
    }

    inc_path <- file.path(base_dir, inc_file)

    # Prevent circular includes
    if (inc_path %in% processed) {
      warning("Circular include detected: ", inc_path)
      current_line <- inc_line + 1
      next
    }
    processed <- c(processed, inc_path)

    # Add comment
    result <- c(result,
                paste0("# --- Included from: ", inc_file, " ---"))

    # Read include file
    if (file.exists(inc_path)) {
      inc_content <- readLines(inc_path, warn = FALSE)
      result <- c(result, inc_content)
    } else {
      warning("Include file not found: ", inc_path)
    }

    result <- c(result,
                paste0("# --- End include: ", inc_file, " ---"))

    current_line <- inc_line + 1
  }

  # Add remaining lines
  if (current_line <= length(lines)) {
    result <- c(result, lines[current_line:length(lines)])
  }

  return(result)
}

# Helper function to combine Python/Pyomo files
combine_python_files <- function(main_file, base_dir = dirname(main_file)) {
  if (!file.exists(main_file)) {
    warning("Python file not found: ", main_file)
    return(character())
  }

  lines <- readLines(main_file, warn = FALSE)

  # Find exec(open()) or similar patterns
  # For energyRt Pyomo, files are typically self-contained
  return(lines)
}

# Helper function to resolve Python exec(open()) statements
resolve_python_exec <- function(file_path, base_dir = dirname(file_path), skip_files = character()) {
  if (!file.exists(file_path)) {
    warning("Python file not found: ", file_path)
    return(character())
  }

  lines <- readLines(file_path, warn = FALSE)

  # Find exec(open("...").read()) patterns
  exec_pattern <- 'exec\\(open\\("(.+?)"\\)\\.read\\(\\)\\)'
  exec_lines <- grep(exec_pattern, lines, perl = TRUE)

  if (length(exec_lines) == 0) {
    # No exec statements, return as-is
    return(lines)
  }

  # Process each line
  result <- character()
  current_line <- 1

  for (exec_line in exec_lines) {
    # Add lines before this exec, commenting out database-related lines
    if (exec_line > current_line) {
      before_lines <- lines[current_line:(exec_line - 1)]
      before_lines <- comment_database_code(before_lines)
      result <- c(result, before_lines)
    }

    # Extract file name
    exec_file <- sub(exec_pattern, "\\1", lines[exec_line], perl = TRUE)

    # Check if this file should be skipped (kept as exec statement)
    if (exec_file %in% skip_files) {
      result <- c(result, lines[exec_line])  # Keep as-is
      current_line <- exec_line + 1
      next
    }

    exec_path <- file.path(base_dir, exec_file)

    # Add comment showing original exec
    result <- c(result,
                paste0("# --- Executed from: ", exec_file, " ---"))

    # Read the executed file
    if (file.exists(exec_path)) {
      # RECURSIVELY resolve exec statements in the included file
      exec_content <- resolve_python_exec(exec_path, base_dir, skip_files)
      result <- c(result, exec_content)
    } else {
      warning("Exec file not found: ", exec_path)
    }

    result <- c(result,
                paste0("# --- End exec: ", exec_file, " ---"))

    current_line <- exec_line + 1
  }

  # Add remaining lines after last exec, commenting out database-related lines
  if (current_line <= length(lines)) {
    remaining_lines <- lines[current_line:length(lines)]
    remaining_lines <- comment_database_code(remaining_lines)
    result <- c(result, remaining_lines)
  }

  return(result)
}

# Helper function to comment out database-related code
comment_database_code <- function(lines) {
  # Comment out sqlite3 imports
  lines <- gsub('^(\\s*import\\s+sqlite3)', '# \\1  # Commented: database replaced with inline data', lines, perl = TRUE)

  # Comment out sqlite3 connections
  lines <- gsub('^(\\s*con\\s*=\\s*sqlite3\\.connect.*)$', '# \\1  # Commented: database replaced with inline data', lines, perl = TRUE)

  # Comment out read_set and read_dict function definitions (multi-line)
  in_function <- FALSE
  function_indent <- 0
  result <- character()

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Check if starting a read_set or read_dict function
    if (grepl('^(\\s*)def\\s+(read_set|read_dict)\\s*\\(', line, perl = TRUE)) {
      in_function <- TRUE
      function_indent <- nchar(sub('^(\\s*).*', '\\1', line))
      result <- c(result, paste0("# ", line, "  # Commented: database replaced with inline data"))
      next
    }

    # If inside function, comment out lines until we reach same or lower indent
    if (in_function) {
      current_indent <- nchar(sub('^(\\s*).*', '\\1', line))
      # Empty lines or lines with greater indent are part of the function
      if (grepl('^\\s*$', line) || current_indent > function_indent) {
        result <- c(result, paste0("# ", line))
      } else {
        # Function ended, add this line normally
        in_function <- FALSE
        result <- c(result, line)
      }
    } else {
      result <- c(result, line)
    }
  }

  return(result)
}

# Path to scenario directory
scenario_dir <- "c:/Users/admin/Documents/R/multimod/dev/scenarios/BASE_UTOPIA"

# Check if directory exists
if (!dir.exists(scenario_dir)) {
  stop("Scenario directory not found: ", scenario_dir)
}

cat("Importing energyRt source files from BASE_UTOPIA scenario...\n")

# Initialize result structure
energyRt_source <- list(
  gams = list(model = NULL, data = NULL),
  gmpl = list(model = NULL, data = NULL),
  jump = list(model = NULL, data = NULL),
  # pyomo = list(model = NULL, data = NULL), # !!! has bugs - revision
  metadata = list(
    source = "energyRt package",
    scenario = "BASE_UTOPIA (Utopia test case)",
    repository = "https://github.com/energyRt/energyRt",
    license = "AGPL-3.0",
    date_imported = Sys.Date(),
    note = paste(
      "energyRt is an R package for energy systems modeling.",
      "This dataset contains generated model files from the BASE_UTOPIA scenario",
      "for testing multimod's parser and converter functions."
    )
  )
)

# --- GAMS ---
cat("Processing GAMS files...\n")
gams_dir <- file.path(scenario_dir, "script", "gams_cbc")
if (dir.exists(gams_dir)) {
  gams_model_file <- file.path(gams_dir, "energyRt.gms")
  gams_data_file <- file.path(gams_dir, "data.gms")

  if (file.exists(gams_model_file)) {
    energyRt_source$gams$model <- resolve_includes(gams_model_file, gams_dir)
    cat("  Model lines: ", length(energyRt_source$gams$model), "\n")
  }

  if (file.exists(gams_data_file)) {
    energyRt_source$gams$data <- resolve_includes(gams_data_file, gams_dir)
    cat("  Data lines: ", length(energyRt_source$gams$data), "\n")
  }
}

# --- GMPL ---
cat("Processing GMPL files...\n")
gmpl_dir <- file.path(scenario_dir, "script", "glpk")
if (dir.exists(gmpl_dir)) {
  gmpl_model_file <- file.path(gmpl_dir, "energyRt.mod")
  gmpl_data_file <- file.path(gmpl_dir, "energyRt.dat")

  if (file.exists(gmpl_model_file)) {
    # GMPL typically doesn't use includes in .mod files
    energyRt_source$gmpl$model <- readLines(gmpl_model_file, warn = FALSE)
    cat("  Model lines: ", length(energyRt_source$gmpl$model), "\n")
  }

  if (file.exists(gmpl_data_file)) {
    energyRt_source$gmpl$data <- combine_gmpl_data(gmpl_data_file)
    cat("  Data lines: ", length(energyRt_source$gmpl$data), "\n")
  }
}

# --- JuMP/Julia ---
cat("Processing JuMP/Julia files...\n")
julia_dir <- file.path(scenario_dir, "script", "julia_highs")
if (dir.exists(julia_dir)) {
  julia_model_file <- file.path(julia_dir, "energyRt.jl")
  julia_data_rdata <- file.path(julia_dir, "data.RData")

  if (file.exists(julia_model_file)) {
    energyRt_source$jump$model <- combine_julia_files(julia_model_file, julia_dir)
    cat("  Model lines: ", length(energyRt_source$jump$model), "\n")
  }

  # Convert data.RData to pure Julia code (no RData dependency)
  if (file.exists(julia_data_rdata)) {
    # Source the converter function
    source("data-raw/convert_rdata_to_julia.R")

    # Create temporary file for conversion
    tmp_data_jl <- tempfile(fileext = ".jl")
    convert_rdata_to_julia(julia_data_rdata, tmp_data_jl)

    # Read the pure Julia data code (DataFrame definitions)
    data_code <- readLines(tmp_data_jl, warn = FALSE)

    # Read original data.jl to extract set creation code
    original_data_jl <- file.path(julia_dir, "data.jl")
    if (file.exists(original_data_jl)) {
      original_data <- readLines(original_data_jl, warn = FALSE)

      # Find where set creation starts (after RData loading)
      # Skip: using RData, using DataFrames, dt = load(...)
      set_start_idx <- grep("^# region$|^region = Set\\(\\)", original_data)[1]

      if (!is.na(set_start_idx)) {
        # Take all set creation code from that point onward
        set_creation_code <- original_data[set_start_idx:length(original_data)]

        # Combine: DataFrame definitions + set creation
        energyRt_source$jump$data <- c(
          data_code,
          "",
          "# === Set creation from DataFrames ===",
          "# The following code creates Julia sets from the dt dictionary",
          "# This replaces the RData loading that was in the original data.jl",
          "",
          set_creation_code
        )

        cat("  Data (pure Julia) lines: ", length(energyRt_source$jump$data),
            " (", length(data_code), " DataFrame defs + ",
            length(set_creation_code), " set creation)\n")
      } else {
        energyRt_source$jump$data <- data_code
        cat("  Data (pure Julia) lines: ", length(data_code), "\n")
        warning("Could not find set creation code in original data.jl")
      }
    } else {
      energyRt_source$jump$data <- data_code
      cat("  Data (pure Julia) lines: ", length(data_code), "\n")
      warning("Original data.jl not found, using only DataFrame definitions")
    }

    cat("  Note: data.jl is self-contained, no data.RData needed\n")

    # Clean up
    unlink(tmp_data_jl)
  }
}

# --- Pyomo ---
# cat("Processing Pyomo files...\n")
# pyomo_dir <- file.path(scenario_dir, "script", "pyomo_cbc_pyinp")
# if (dir.exists(pyomo_dir)) {
#   # Look for Python files
#   pyomo_model_file <- file.path(pyomo_dir, "energyRt.py")
#   pyomo_data_file <- file.path(pyomo_dir, "data.py")
#
#   if (file.exists(pyomo_model_file)) {
#     # Skip data.py to keep it as an exec() statement (not embedded)
#     energyRt_source$pyomo$model <- resolve_python_exec(pyomo_model_file, pyomo_dir, skip_files = "data.py")
#     cat("  Model lines: ", length(energyRt_source$pyomo$model), "\n")
#   }
#
#   # Pyomo data: resolve exec() statements to combine input files (no skip)
#   if (file.exists(pyomo_data_file)) {
#     energyRt_source$pyomo$data <- resolve_python_exec(pyomo_data_file, pyomo_dir)
#     cat("  Data lines: ", length(energyRt_source$pyomo$data), "\n")
#   }
# }

# Save the dataset
cat("\nSaving energyRt_source dataset...\n")
# usethis::use_data(energyRt_source, overwrite = TRUE)
save(energyRt_source, file = "data-raw/energyRt_source.RData")

cat("\nenegyRt source data created successfully!\n")
cat("Summary:\n")
cat("  GAMS model: ", length(energyRt_source$gams$model), " lines\n")
cat("  GAMS data: ", length(energyRt_source$gams$data), " lines\n")
cat("  GMPL model: ", length(energyRt_source$gmpl$model), " lines\n")
cat("  GMPL data: ", length(energyRt_source$gmpl$data), " lines\n")
cat("  JuMP model: ", length(energyRt_source$jump$model), " lines\n")
cat("  JuMP data (pure Julia): ", length(energyRt_source$jump$data), " lines\n")
cat("  Pyomo model: ", length(energyRt_source$pyomo$model), " lines\n")
cat("  Pyomo data: ", length(energyRt_source$pyomo$data), " lines\n")
