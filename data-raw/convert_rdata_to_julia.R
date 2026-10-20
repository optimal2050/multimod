## Function to convert data.RData to pure Julia code
## This creates a self-contained data.jl that doesn't need data.RData

convert_rdata_to_julia <- function(rdata_file, output_file, 
                                   dat_var_name = "dat") {
  
  # Load the RData file
  env <- new.env()
  load(rdata_file, envir = env)
  
  # Get the data object
  if (!dat_var_name %in% ls(env)) {
    stop("Variable '", dat_var_name, "' not found in RData file")
  }
  
  dat <- get(dat_var_name, envir = env)
  
  if (!is.list(dat)) {
    stop("Expected ", dat_var_name, " to be a list")
  }
  
  cat("Converting", length(dat), "data frames to Julia...\n")
  
  # Start building Julia code
  julia_code <- character()
  
  # Header
  julia_code <- c(julia_code,
    "# Data for energyRt BASE_UTOPIA model",
    "# ",
    "# NOTE: This file has been amended from the original energyRt package output.",
    "# The original data.jl used RData.jl to load data from data.RData file.",
    "# This version embeds all data directly in native Julia code (DataFrames),",
    "# eliminating the need for RData.jl package and data.RData file.",
    "# ",
    "# Original: using RData; dt = load(\"data.RData\")[\"dat\"]",
    "# Modified: dt = Dict{String, DataFrame}() with inline data definitions",
    "",
    "using DataFrames",
    "using Dates",
    "",
    "println(\"Loading data \", Dates.format(now(), \"HH:MM:SS\"))",
    "",
    "# Initialize data dictionary",
    "dt = Dict{String, DataFrame}()",
    ""
  )
  
  # Convert each dataframe
  for (name in names(dat)) {
    df <- dat[[name]]
    
    if (!is.data.frame(df)) {
      warning("Skipping non-dataframe: ", name)
      next
    }
    
    julia_code <- c(julia_code, 
                   convert_df_to_julia(df, name))
  }
  
  # Footer
  julia_code <- c(julia_code,
    "",
    "println(\"Data loaded \", Dates.format(now(), \"HH:MM:SS\"))",
    ""
  )
  
  # Write to file
  writeLines(julia_code, output_file)
  
  cat("Wrote", length(julia_code), "lines to", output_file, "\n")
  
  invisible(julia_code)
}

# Helper function to convert a single dataframe to Julia code
convert_df_to_julia <- function(df, name) {
  code <- character()
  
  nrows <- nrow(df)
  ncols <- ncol(df)
  
  if (nrows == 0) {
    # Empty dataframe - just create structure
    col_types <- sapply(df, class)
    col_names <- names(df)
    
    type_map <- c(
      "character" = "String",
      "numeric" = "Float64",
      "integer" = "Int64",
      "logical" = "Bool"
    )
    
    julia_types <- sapply(col_types, function(t) {
      type_map[t[1]] %||% "Any"
    })
    
    if (ncols == 0) {
      code <- c(code, sprintf('dt["%s"] = DataFrame()', name))
    } else {
      cols_spec <- paste(
        sprintf("%s=Vector{%s}()", col_names, julia_types),
        collapse = ", "
      )
      code <- c(code, sprintf('dt["%s"] = DataFrame(%s)', name, cols_spec))
    }
    
  } else {
    # Non-empty dataframe - write data
    code <- c(code, sprintf('dt["%s"] = DataFrame(', name))
    
    for (i in seq_along(names(df))) {
      col_name <- names(df)[i]
      col_data <- df[[i]]
      
      # Format column data
      julia_vec <- format_julia_vector(col_data)
      
      if (i < ncol(df)) {
        code <- c(code, sprintf('    %s = %s,', col_name, julia_vec))
      } else {
        code <- c(code, sprintf('    %s = %s', col_name, julia_vec))
      }
    }
    
    code <- c(code, ')')
  }
  
  code <- c(code, '')
  return(code)
}

# Helper to format R vector as Julia vector
format_julia_vector <- function(vec) {
  if (length(vec) == 0) {
    # Empty vector
    if (is.numeric(vec)) {
      return("Float64[]")
    } else if (is.integer(vec)) {
      return("Int64[]")
    } else if (is.character(vec)) {
      return("String[]")
    } else {
      return("[]")
    }
  }
  
  if (is.character(vec)) {
    # String vector
    escaped <- gsub('\\', '\\\\', vec, fixed = TRUE)
    escaped <- gsub('"', '\\"', escaped, fixed = TRUE)
    quoted <- sprintf('"%s"', escaped)
    return(sprintf('[%s]', paste(quoted, collapse = ', ')))
    
  } else if (is.numeric(vec)) {
    # Numeric vector
    # Handle special values
    vec_str <- ifelse(is.na(vec), "NaN",
                     ifelse(is.infinite(vec) & vec > 0, "Inf",
                           ifelse(is.infinite(vec) & vec < 0, "-Inf",
                                 as.character(vec))))
    return(sprintf('[%s]', paste(vec_str, collapse = ', ')))
    
  } else if (is.integer(vec)) {
    # Integer vector
    vec_str <- as.character(vec)
    return(sprintf('[%s]', paste(vec_str, collapse = ', ')))
    
  } else if (is.logical(vec)) {
    # Boolean vector
    vec_str <- ifelse(vec, "true", "false")
    return(sprintf('[%s]', paste(vec_str, collapse = ', ')))
    
  } else {
    # Fall back to string representation
    warning("Unsupported type: ", class(vec)[1])
    return(sprintf('[%s]', paste(shQuote(as.character(vec)), collapse = ', ')))
  }
}

# Null coalesce operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Example usage:
if (FALSE) {
  convert_rdata_to_julia(
    rdata_file = "dev/scenarios/BASE_UTOPIA/script/julia_highs/data.RData",
    output_file = "dev/scenarios/BASE_UTOPIA/script/julia_highs/data_pure.jl"
  )
}
