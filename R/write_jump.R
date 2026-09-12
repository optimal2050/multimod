#' Extract set members from model data (parameters/mappings)
#' @param model multimod object
#' @param set_name Name of the set to extract members for
#' @return Character vector of unique set members
#' @keywords internal
.extract_set_members <- function(model, set_name) {
  members <- character(0)

  # Check parameters for columns matching set_name
  if (!is.null(model$parameters)) {
    for (param in model$parameters) {
      if (!is.null(param$data) && set_name %in% names(param$data)) {
        members <- c(members, as.character(param$data[[set_name]]))
      }
    }
  }

  # Check mappings for columns matching set_name
  if (!is.null(model$mappings)) {
    for (mapping in model$mappings) {
      if (!is.null(mapping$data) && set_name %in% names(mapping$data)) {
        members <- c(members, as.character(mapping$data[[set_name]]))
      }
    }
  }

  # Return unique, sorted members
  if (length(members) > 0) {
    members <- unique(members)
    members <- sort(members)
  }

  members
}

#' Write a Julia/JuMP model file from a multimod object
#'
#' @param model A `multimod` model object
#' @param file Output file path (optional, overrides model_dir/solver_dir)
#' @param model_dir Directory where model was saved with save_model() (contains model.rds)
#' @param solver_dir Solver subdirectory name (default: "jump", e.g., "jump-highs", "jump-cplex")
#' @param output_name Output filename (default: "model.jl")
#' @param model_name Name for the JuMP model variable (default: "model")
#' @param optimizer Julia optimizer to use (e.g., "HiGHS.Optimizer", "GLPK.Optimizer", "CPLEX.Optimizer")
#' @param optimizer_attributes Named list of optimizer attributes to set (e.g., list(solver = "ipm", presolve = "on", parallel = "on")).
#'   For HiGHS: solver can be "simplex", "ipm" (interior point/barrier), or "choose" (default). 
#'   Use "ipm" for large models (>1M rows) as it scales better than simplex.
#'   If NULL, uses solver defaults with automatic barrier method selection for large models (>1M constraints).
#' @param use_folded logical; whether to use folded equations if available (default: TRUE)
#' @param cleanup logical; whether to remove existing solver_dir before generating new files (default: TRUE)
#' @param export_data logical; whether to export parameter data to CSV files in data_export/ directory (default: FALSE)
#' @param export_vars logical; whether to export variable values to CSV files in vars_export/ directory after solve (default: FALSE). 
#'   Note: Solution is always saved to solution/ directory regardless of this setting (required for load_results).
#' @param export_lp logical or character; if TRUE, exports model to LP format; if character, specifies filename (default: FALSE)
#' @param export_mps logical or character; if TRUE, exports model to MPS format; if character, specifies filename (default: FALSE)
#' @param ... Additional arguments (not used)
#'
#' @return Character vector of Julia code, or writes file if path is determined
#' @export
#'
#' @examples
#' \dontrun{
#' # Write to model directory with default solver subdirectory
#' write_jump(model, model_dir = "my_model")
#' # -> my_model/solvers/jump/model.jl
#'
#' # Specify custom solver directory
#' write_jump(model, model_dir = "my_model", solver_dir = "jump-highs")
#' # -> my_model/solvers/jump-highs/model.jl
#'
#' # Override with explicit file path
#' write_jump(model, file = "custom/path/model.jl")
#' 
#' # Use barrier method for large models
#' write_jump(model, model_dir = "my_model", 
#'            optimizer_attributes = list(solver = "ipm", parallel = "on"))
#' }
write_jump <- function(model, file = NULL, model_dir = NULL, solver_dir = "jump",
                       output_name = "model.jl", model_name = "model",
                       optimizer = "HiGHS.Optimizer", optimizer_attributes = NULL,
                       use_folded = TRUE, cleanup = TRUE,
                       export_data = FALSE, export_vars = FALSE,
                       export_lp = FALSE, export_mps = FALSE,
                       data_mode = c("external", "embedded"), ...) {
  UseMethod("write_jump")
}

#' @export
write_jump.model <- function(model, file = NULL, model_dir = NULL, solver_dir = "jump",
                              output_name = "model.jl", model_name = "model",
                              optimizer = "HiGHS.Optimizer", optimizer_attributes = NULL,
                              use_folded = TRUE, cleanup = TRUE,
                              export_data = FALSE, export_vars = FALSE,
            export_lp = FALSE, export_mps = FALSE,
            data_mode = c("external", "embedded"),
                              use_haskey = TRUE, ...) {
  write_jump_internal(model, file, model_dir, solver_dir, output_name, model_name, 
                      optimizer, optimizer_attributes, use_folded, cleanup,
          export_data, export_vars, export_lp, export_mps,
          use_haskey = use_haskey, data_mode = data_mode, ...)
}

#' @export
write_jump.multimod <- function(model, file = NULL, model_dir = NULL, solver_dir = "jump",
                                 output_name = "model.jl", model_name = "model",
                                 optimizer = "HiGHS.Optimizer", optimizer_attributes = NULL,
                                 use_folded = TRUE, cleanup = TRUE,
                                 export_data = FALSE, export_vars = FALSE,
                                 export_lp = FALSE, export_mps = FALSE,
                                 data_mode = c("external", "embedded"),
                                 use_haskey = TRUE, ...) {
  write_jump_internal(model, file, model_dir, solver_dir, output_name, model_name, 
                      optimizer, optimizer_attributes, use_folded, cleanup,
                      export_data, export_vars, export_lp, export_mps,
                      use_haskey = use_haskey, data_mode = data_mode, ...)
}

#' @export
write_jump.model_structure <- function(model, file = NULL, model_dir = NULL, solver_dir = "jump",
                                        output_name = "model.jl", model_name = "model",
                                        optimizer = "HiGHS.Optimizer", optimizer_attributes = NULL,
                                        use_folded = TRUE, cleanup = TRUE,
                                        export_data = FALSE, export_vars = FALSE,
                                        export_lp = FALSE, export_mps = FALSE,
                                        data_mode = c("external", "embedded"),
                                        use_haskey = TRUE, ...) {
  stopifnot(inherits(model, "model_structure"))
  write_jump_internal(model, file, model_dir, solver_dir, output_name, model_name, 
                      optimizer, optimizer_attributes, use_folded, cleanup,
                      export_data, export_vars, export_lp, export_mps,
                      use_haskey = use_haskey, data_mode = data_mode, ...)
}

#' Generate data.jl file that loads all data upfront (energyRt.jl compatible)
#' @keywords internal
.generate_data_jl <- function(model_dir, solver_dir, model = NULL, use_folded = FALSE, shifts_needed = NULL, export_data = FALSE, data_mode = c("external", "embedded")) {
  data_mode <- match.arg(data_mode)
  if (identical(data_mode, "embedded")) {
    data_lines <- .build_inline_data_lines(model = model, use_folded = use_folded, shifts_needed = shifts_needed)
    data_lines <- c(data_lines, .build_computed_params_block(model, shifts_needed))
    data_lines <- c(data_lines, "println(\"Data loaded successfully\")", "")
    if (export_data) {
      data_lines <- c(data_lines, .build_data_export_block(model))
    }
    data_file <- file.path(model_dir, "solvers", solver_dir, "data.jl")
    writeLines(data_lines, data_file)
    return(invisible(data_file))
  }

  data_lines <- character()

  data_lines <- c(data_lines, "# Data loading - energyRt.jl compatible format")
  data_lines <- c(data_lines, "using Arrow, DataFrames, CSV")
  data_lines <- c(data_lines, "")
  data_lines <- c(data_lines, "# Detect data format")
  data_lines <- c(data_lines, 'format_file = joinpath(@__DIR__, "..", "..", "format.txt")')
  data_lines <- c(data_lines, 'DATA_FORMAT = isfile(format_file) ? strip(read(format_file, String)) : "csv"')
  data_lines <- c(data_lines, 'DATA_EXT = DATA_FORMAT == "ipc" ? ".arrow" : DATA_FORMAT == "parquet" ? ".parquet" : ".csv"')
  data_lines <- c(data_lines, 'println("Data format: $DATA_FORMAT (extension: $DATA_EXT)")')
  data_lines <- c(data_lines, "")
  data_lines <- c(data_lines, "# Helper functions for loading")
  data_lines <- c(data_lines, "function _load_set_file(set_name)")
  data_lines <- c(data_lines, '    file = joinpath(@__DIR__, "..", "..", "sets", set_name, "data" * DATA_EXT)')
  data_lines <- c(data_lines, "    if !isfile(file); return String[]; end")
  data_lines <- c(data_lines, "    df = DATA_FORMAT == \"csv\" ? CSV.read(file, DataFrame; types=String) : (Arrow.Table(file) |> DataFrame)")
  data_lines <- c(data_lines, "    return string.(df[!, 1])")
  data_lines <- c(data_lines, "end")
  data_lines <- c(data_lines, "")
  data_lines <- c(data_lines, "function _load_mapping_file(mapping_name)")
  data_lines <- c(data_lines, '    file = joinpath(@__DIR__, "..", "..", "mappings", mapping_name, "data" * DATA_EXT)')
  data_lines <- c(data_lines, "    if !isfile(file); return Set(); end")
  data_lines <- c(data_lines, "    df = DATA_FORMAT == \"csv\" ? CSV.read(file, DataFrame; types=String) : (Arrow.Table(file) |> DataFrame)")
  data_lines <- c(data_lines, "    if ncol(df) == 1")
  data_lines <- c(data_lines, "        # Single dimension: use scalar not tuple (for t in mTradeCapacityVariable not (t,) in ...)")
  data_lines <- c(data_lines, "        return Set(string(row[1]) for row in eachrow(df))")
  data_lines <- c(data_lines, "    else")
  data_lines <- c(data_lines, "        # Multiple dimensions: use tuple")
  data_lines <- c(data_lines, "        return Set(Tuple(string(row[i]) for i in 1:ncol(df)) for row in eachrow(df))")
  data_lines <- c(data_lines, "    end")
  data_lines <- c(data_lines, "end")
  data_lines <- c(data_lines, "")
  data_lines <- c(data_lines, "function _load_param_file(param_name, dims, use_folded_path = false)")
  data_lines <- c(data_lines, "    # Try folded_data first if requested, fall back to regular data")
  data_lines <- c(data_lines, "    if use_folded_path")
  data_lines <- c(data_lines, '        folded_file = joinpath(@__DIR__, "..", "..", "parameters", param_name, "folded_data", "data" * DATA_EXT)')
  data_lines <- c(data_lines, "        if isfile(folded_file)")
  data_lines <- c(data_lines, "            file = folded_file")
  data_lines <- c(data_lines, "        else")
  data_lines <- c(data_lines, '            file = joinpath(@__DIR__, "..", "..", "parameters", param_name, "data" * DATA_EXT)')
  data_lines <- c(data_lines, "        end")
  data_lines <- c(data_lines, "    else")
  data_lines <- c(data_lines, '        file = joinpath(@__DIR__, "..", "..", "parameters", param_name, "data" * DATA_EXT)')
  data_lines <- c(data_lines, "    end")
  data_lines <- c(data_lines, "    if !isfile(file); return isempty(dims) ? 0.0 : Dict(); end")
  data_lines <- c(data_lines, "    df = if DATA_FORMAT == \"csv\"")
  data_lines <- c(data_lines, "        d = CSV.read(file, DataFrame; types=String)")
  data_lines <- c(data_lines, "        if \"value\" in names(d); d.value = parse.(Float64, d.value); end")
  data_lines <- c(data_lines, "        d")
  data_lines <- c(data_lines, "    else")
  data_lines <- c(data_lines, "        Arrow.Table(file) |> DataFrame")
  data_lines <- c(data_lines, "    end")
  data_lines <- c(data_lines, "    if isempty(dims)")
  data_lines <- c(data_lines, "        return df.value[1]")
  data_lines <- c(data_lines, "    elseif length(dims) == 1")
  data_lines <- c(data_lines, "        # Single dimension: use scalar key not tuple (for ordYear[y] not ordYear[(y,)])")
  data_lines <- c(data_lines, "        # Always use .value column (folded_data has n_original as last column)")
  data_lines <- c(data_lines, "        val_col = findfirst(==(:value), propertynames(df))")
  data_lines <- c(data_lines, "        return Dict(string(row[1]) => row[val_col] for row in eachrow(df))")
  data_lines <- c(data_lines, "    else")
  data_lines <- c(data_lines, "        # Multiple dimensions: use tuple key")
  data_lines <- c(data_lines, "        # Always use .value column (folded_data has n_original as last column)")
  data_lines <- c(data_lines, "        val_col = findfirst(==(:value), propertynames(df))")
  data_lines <- c(data_lines, "        return Dict(Tuple(string(row[i]) for i in 1:length(dims)) => row[val_col] for row in eachrow(df))")
  data_lines <- c(data_lines, "    end")
  data_lines <- c(data_lines, "end")
  data_lines <- c(data_lines, "")

  # Read metadata CSVs to generate data loading code
  base_dir <- file.path(model_dir)
  sets_csv <- file.path(base_dir, "sets", "sets.csv")
  mappings_csv <- file.path(base_dir, "mappings", "mappings.csv")
  params_csv <- file.path(base_dir, "parameters", "parameters.csv")

  # Load sets
  data_lines <- c(data_lines, "# Load sets (fundamental dimensions)")
  if (file.exists(sets_csv)) {
    sets_df <- read.csv(sets_csv, stringsAsFactors = FALSE)
    for (i in 1:nrow(sets_df)) {
      set_name <- sets_df$name[i]
      desc <- sanitize_description(sets_df$desc[i], set_name)
      data_lines <- c(data_lines, sprintf("%s = _load_set_file(\"%s\")  # %s", set_name, set_name, desc))
    }
  }

  # Declare trimmed sets as empty arrays (so equations that reference them compile)
  if (!is.null(model) && !is.null(model$sets)) {
    trimmed_sets <- names(model$sets)[sapply(model$sets, function(s) isTRUE(s$trimmed))]
    if (length(trimmed_sets) > 0) {
      data_lines <- c(data_lines, "# Trimmed sets (empty)")
      for (set_name in trimmed_sets) {
        data_lines <- c(data_lines, sprintf("%s = String[]  # trimmed", set_name))
      }
    }
  }
  
  # Declare aliases (alternative names for sets)
  if (!is.null(model) && !is.null(model$aliases)) {
    data_lines <- c(data_lines, "# Set aliases")
    for (alias_group in model$aliases) {
      if (length(alias_group) > 1) {
        # First element is the primary set name (ensured by read_gams normalization)
        primary <- alias_group[1]
        # Other elements are aliases pointing to the primary
        aliases <- alias_group[-1]
        for (alias_name in aliases) {
          data_lines <- c(data_lines, sprintf("%s = %s  # Alias for %s", alias_name, primary, primary))
        }
      }
    }
  }
  data_lines <- c(data_lines, "")
  
  # Generate shift dictionaries (collected during constraint generation)
  if (!is.null(shifts_needed) && length(as.list(shifts_needed)) > 0) {
    shifts_list <- as.list(shifts_needed)
    data_lines <- c(data_lines, "# Index shift dictionaries for ordered sets")
    
    for (shift_info in shifts_list) {
      set_name <- shift_info$set_name
      offset <- shift_info$offset
      
      if (offset < 0) {
        dict_name <- paste0(tolower(set_name), "_lag", abs(offset))
        comment <- sprintf("Maps %s to previous element (offset %d)", set_name, offset)
        dict_def <- sprintf("%s = Dict(%s[i] => %s[i%d] for i in %d:length(%s))",
                           dict_name, set_name, set_name, offset, abs(offset) + 1, set_name)
      } else {
        dict_name <- paste0(tolower(set_name), "_lead", offset)
        comment <- sprintf("Maps %s to next element (offset +%d)", set_name, offset)
        dict_def <- sprintf("%s = Dict(%s[i] => %s[i+%d] for i in 1:(length(%s)-%d))",
                           dict_name, set_name, set_name, offset, set_name, offset)
      }
      data_lines <- c(data_lines, paste0(dict_def, "  # ", comment))
    }
    data_lines <- c(data_lines, "")
  }

  # Load mappings
  data_lines <- c(data_lines, "# Load mappings (valid index combinations)")
  if (file.exists(mappings_csv)) {
    mappings_df <- read.csv(mappings_csv, stringsAsFactors = FALSE)
    for (i in 1:nrow(mappings_df)) {
      mapping_name <- mappings_df$name[i]
      desc <- sanitize_description(mappings_df$desc[i], mapping_name)
      data_lines <- c(data_lines, sprintf("%s = _load_mapping_file(\"%s\")  # %s", mapping_name, mapping_name, desc))
    }
  }
  data_lines <- c(data_lines, "")

  # Load parameters
  data_lines <- c(data_lines, "# Load parameters")
  if (file.exists(params_csv)) {
    params_df <- read.csv(params_csv, stringsAsFactors = FALSE)
    for (i in 1:nrow(params_df)) {
      param_name <- params_df$name[i]
      dims_str <- params_df$dims[i]
      desc <- sanitize_description(params_df$desc[i], param_name)

      # Check if parameter has folded data when use_folded is TRUE
      use_folded_for_param <- FALSE
      if (use_folded && !is.null(model) && !is.null(model$parameters[[param_name]])) {
        param_obj <- model$parameters[[param_name]]
        if (!is.null(param_obj$folded_data) && !is.null(param_obj$active_dims)) {
          use_folded_for_param <- TRUE
          # Use active_dims instead of dims for folded parameters
          dims <- sapply(param_obj$active_dims, function(d) d$name)
        }
      }

      # If not using folded data, parse dimensions from CSV
      if (!use_folded_for_param) {
        # Parse dimensions from string like "[region,year]"
        dims_str <- gsub("\\[|\\]", "", dims_str)
        if (nchar(dims_str) == 0 || is.na(dims_str)) {
          dims <- character(0)
        } else {
          dims <- strsplit(dims_str, ",")[[1]]
          dims <- trimws(dims)
        }
      }

      # Format dims for Julia
      if (length(dims) == 0) {
        dims_vec <- "String[]"
      } else {
        dims_vec <- sprintf("[\"%s\"]", paste(dims, collapse = "\", \""))
      }

      # Check for default value
      default_val <- params_df$default_value[i]
      has_default <- !is.na(default_val) && default_val != "NA"
      
      # Check if default is an ast_formula or expression (starts with <formula: or <expr:)
      is_formula <- has_default && grepl("^<(formula|expr):", default_val)
      
      # Check if parameter is symbolic (string type) - need to quote the value
      is_symbolic <- FALSE
      if (!is.null(model) && !is.null(model$parameters[[param_name]])) {
        is_symbolic <- isTRUE(model$parameters[[param_name]]$symbolic)
      }

      # Always generate Def variable (equations expect it for haskey fallback)
      # Use provided default or fall back to 0.0
      # For formulas/expressions, use 0.0 as placeholder (actual values come from data file)
      def_value <- if (is_formula) {
        "0.0  # Formula/expression - see model file"
      } else if (has_default) {
        # Quote string values for symbolic parameters
        if (is_symbolic) {
          sprintf("\"%s\"", default_val)
        } else {
          default_val
        }
      } else {
        "0.0"  # Universal default
      }

      data_lines <- c(data_lines, sprintf("%sDef = %s", param_name, def_value))
      # Pass use_folded_path flag to _load_param_file
      use_folded_str <- if (use_folded_for_param) "true" else "false"
      data_lines <- c(data_lines, sprintf("%s = _load_param_file(\"%s\", %s, %s)  # %s",
                                          param_name, param_name, dims_vec, use_folded_str, desc))
    }
  }
  data_lines <- c(data_lines, "")
  
  data_lines <- c(data_lines, .build_computed_params_block(model, shifts_needed))
  
  data_lines <- c(data_lines, "println(\"Data loaded successfully\")")
  data_lines <- c(data_lines, "")

  if (export_data) {
    data_lines <- c(data_lines, .build_data_export_block(model))
  }

  # Write data.jl file
  data_file <- file.path(model_dir, "solvers", solver_dir, "data.jl")
  writeLines(data_lines, data_file)
  invisible(data_file)
}

.build_computed_params_block <- function(model, shifts_needed) {
  lines <- character()
  if (is.null(model) || is.null(model$parameters)) {
    return(lines)
  }
  computed_params <- Filter(function(p) !is.null(p$formula), model$parameters)
  if (length(computed_params) == 0) {
    return(lines)
  }
  lines <- c(lines, "# Computed parameters (from formulas)")
  for (param_name in names(computed_params)) {
    param_obj <- computed_params[[param_name]]
    dims <- param_obj$dims
    formula_ast <- if (inherits(param_obj$formula, "ast_formula")) param_obj$formula$expr else param_obj$formula
    if (inherits(param_obj$formula, "ast_formula") && !is.null(param_obj$formula$index_vars)) {
      iter_vars <- param_obj$formula$index_vars
      dim_names <- param_obj$formula$index_sets
    } else if (!is.null(dims) && length(dims) > 0) {
      dim_names <- sapply(dims, function(d) d$name)
      iter_vars <- sapply(dim_names, function(set_name) {
        if (!is.null(model$index_aliases) && set_name %in% names(model$index_aliases)) {
          model$index_aliases[[set_name]]
        } else {
          tolower(substring(set_name, 1, 1))
        }
      })
    } else {
      dim_names <- NULL
      iter_vars <- NULL
    }
    if (!is.null(dims) && length(dims) > 0 && !is.null(dim_names) && !is.null(iter_vars)) {
      var_names <- setNames(as.list(dim_names), iter_vars)
      formula_expr <- as_jump(formula_ast, model = model, var_names = var_names, use_haskey = FALSE, shifts_tracker = shifts_needed)
      key_tuple <- if (length(iter_vars) == 1) iter_vars else paste0("(", paste(iter_vars, collapse=","), ")")
      iter_clauses <- paste(iter_vars, "in", dim_names, collapse=", ")
      comp_expr <- sprintf("%s = Dict(%s => %s for %s)", param_name, key_tuple, formula_expr, iter_clauses)
      lines <- c(lines, comp_expr)
    } else {
      formula_expr <- as_jump(formula_ast, model = model, use_haskey = FALSE, shifts_tracker = shifts_needed)
      lines <- c(lines, sprintf("%s = %s", param_name, formula_expr))
    }
  }
  lines <- c(lines, "")
  lines
}

.build_data_export_block <- function(model) {
  lines <- character()
  lines <- c(lines, "# ========== DATA EXPORT ==========")
  lines <- c(lines, 'export_dir = joinpath(@__DIR__, "data_export")')
  lines <- c(lines, "if !isdir(export_dir)")
  lines <- c(lines, "    mkpath(export_dir)")
  lines <- c(lines, "end")
  lines <- c(lines, 'println("Exporting model data to: ", export_dir)')
  lines <- c(lines, "")
  if (!is.null(model$sets) && length(model$sets) > 0) {
    lines <- c(lines, "# Export sets")
    for (set_name in names(model$sets)) {
      set_info <- model$sets[[set_name]]
      if (!is.null(set_info$dims) && length(set_info$dims) > 0) {
        next
      }
      lines <- c(lines, sprintf('CSV.write(joinpath(export_dir, "%s.csv"), DataFrame(%s = collect(%s)))', set_name, set_name, set_name))
    }
    lines <- c(lines, "")
  }
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    lines <- c(lines, "# Export parameters")
    for (param_name in names(model$parameters)) {
      param_info <- model$parameters[[param_name]]
      if (isTRUE(param_info$trimmed)) {
        next
      }
      if (!is.null(param_info$symbolic) && param_info$symbolic) {
        next
      }
      has_default <- !is.null(param_info$defVal)
      default_val <- if (has_default) param_info$defVal else NULL
      dims <- param_info$dims
      if (is.null(dims) || length(dims) == 0) {
        lines <- c(lines, sprintf('CSV.write(joinpath(export_dir, "%s.csv"), DataFrame(value = [%s]))', param_name, param_name))
      } else if (length(dims) == 1) {
        dim_name <- if (is.character(dims[[1]])) dims[[1]] else dims[[1]]$name
        lines <- c(lines, sprintf('# Export %s (1D)', param_name))
        lines <- c(lines, 'begin')
        if (has_default && !inherits(default_val, "ast_formula")) {
          default_str <- if (is.numeric(default_val)) {
            if (default_val == as.integer(default_val)) sprintf("%.1f", default_val) else as.character(default_val)
          } else {
            paste0('"', default_val, '"')
          }
          lines <- c(lines, '    # Iterate over all dimension values, filtering non-zero')
          lines <- c(lines, sprintf('    dim_vals = [k for k in %s if get(%s, k, %s) != 0]', dim_name, param_name, default_str))
          lines <- c(lines, sprintf('    param_vals = [get(%s, k, %s) for k in dim_vals]', param_name, default_str))
          lines <- c(lines, sprintf('    # Create DataFrame with default row first, then append data'))
          lines <- c(lines, sprintf('    df_data = DataFrame(%s = Union{Missing, String}[missing], value = [%s])', dim_name, default_str))
          lines <- c(lines, '    for (k, v) in zip(dim_vals, param_vals)')
          lines <- c(lines, '        push!(df_data, (k, v))')
          lines <- c(lines, '    end')
        } else {
          lines <- c(lines, '    # Export only explicit non-zero values')
          lines <- c(lines, sprintf('    dim_vals = [k for k in %s if haskey(%s, k) && %s[k] != 0]', dim_name, param_name, param_name))
          lines <- c(lines, sprintf('    param_vals = [%s[k] for k in dim_vals]', param_name))
          lines <- c(lines, sprintf('    df_data = DataFrame(%s = dim_vals, value = param_vals)', dim_name))
        }
        lines <- c(lines, sprintf('    CSV.write(joinpath(export_dir, "%s.csv"), df_data)', param_name))
        lines <- c(lines, 'end')
      } else {
        dim_names <- sapply(dims, function(d) if (is.character(d)) d else d$name)
        lines <- c(lines, sprintf('# Export %s (%dD)', param_name, length(dim_names)))
        lines <- c(lines, 'begin')
        iter_vars <- paste0("k", seq_along(dim_names))
        iter_clauses <- paste(sprintf("%s in %s", iter_vars, dim_names), collapse = ", ")
        tuple_access <- paste0("(", paste(iter_vars, collapse = ", "), ")")
        if (has_default && !inherits(default_val, "ast_formula")) {
          default_str <- if (is.numeric(default_val)) {
            if (default_val == as.integer(default_val)) sprintf("%.1f", default_val) else as.character(default_val)
          } else {
            paste0('"', default_val, '"')
          }
          lines <- c(lines, '    # Iterate over all dimension combinations, filtering non-zero')
          lines <- c(lines, sprintf('    all_keys = [%s for %s if get(%s, %s, %s) != 0]', tuple_access, iter_clauses, param_name, tuple_access, default_str))
          lines <- c(lines, sprintf('    all_vals = [get(%s, k, %s) for k in all_keys]', param_name, default_str))
        } else {
          lines <- c(lines, '    # Export only explicit non-zero values')
          lines <- c(lines, sprintf('    all_keys = [%s for %s if haskey(%s, %s) && %s[%s] != 0]', tuple_access, iter_clauses, param_name, tuple_access, param_name, paste(iter_vars, collapse = ", ")))
          lines <- c(lines, sprintf('    all_vals = [%s[k...] for k in all_keys]', param_name))
        }
        lines <- c(lines, '    # Build DataFrame with proper column names')
        if (has_default && !inherits(default_val, "ast_formula")) {
          lines <- c(lines, '    df_data = DataFrame()')
          for (i in seq_along(dim_names)) {
            lines <- c(lines, sprintf('    df_data[!, "%s"] = Union{Missing, String}[missing]', dim_names[i]))
          }
          lines <- c(lines, sprintf('    df_data[!, "value"] = [%s]', default_str))
          lines <- c(lines, '    # Append non-zero values')
          lines <- c(lines, '    for (k, v) in zip(all_keys, all_vals)')
          col_pushes <- paste(sprintf('k[%d]', seq_along(dim_names)), collapse = ", ")
          lines <- c(lines, sprintf('        push!(df_data, (%s, v))', col_pushes))
          lines <- c(lines, '    end')
        } else {
          lines <- c(lines, '    df_data = DataFrame()')
          for (i in seq_along(dim_names)) {
            lines <- c(lines, sprintf('    df_data[!, "%s"] = [k[%d] for k in all_keys]', dim_names[i], i))
          }
          lines <- c(lines, '    df_data[!, "value"] = all_vals')
        }
        lines <- c(lines, sprintf('    CSV.write(joinpath(export_dir, "%s.csv"), df_data)', param_name))
        lines <- c(lines, 'end')
      }
    }
    lines <- c(lines, "")
  }
  lines <- c(lines, 'println("Data export complete\\n")')
  lines <- c(lines, "")
  lines
}

.format_parameter_default_literal <- function(param_obj) {
  def_val <- param_obj$defVal
  is_symbolic <- isTRUE(param_obj$symbolic)
  if (inherits(def_val, "ast_formula") || inherits(def_val, "ast_expression")) {
    return("0.0  # Formula/expression - see model file")
  }
  if (is.null(def_val) || (is.character(def_val) && (is.na(def_val) || !nzchar(def_val)))) {
    return("0.0")
  }
  if (is_symbolic || is.character(def_val)) {
    return(sprintf('"%s"', def_val))
  }
  as.character(def_val)
}

.inline_desc <- function(text, name = NULL) {
  desc_text <- sanitize_description(text, name)
  if (!nzchar(desc_text)) {
    return("")
  }
  paste0("  # ", desc_text)
}

.build_inline_data_lines <- function(model, use_folded = FALSE, shifts_needed = NULL) {
  if (is.null(model)) {
    stop("Inline data layout requires a populated model object")
  }
  lines <- character()
  lines <- c(lines, "# Data loading - inline format")
  lines <- c(lines, "using Arrow, DataFrames, CSV")
  lines <- c(lines, "")
  lines <- c(lines, "# Inline data settings")
  lines <- c(lines, 'DATA_FORMAT = "inline"')
  lines <- c(lines, 'DATA_EXT = ".csv"')
  lines <- c(lines, 'println("Data format: inline (embedded)")')
  lines <- c(lines, "")
  lines <- c(lines, "# Load sets")
  if (!is.null(model$sets) && length(model$sets) > 0) {
    for (set_name in names(model$sets)) {
      set_obj <- model$sets[[set_name]]
      members <- set_obj$members
      if (isTRUE(set_obj$trimmed)) {
        members <- character(0)
      }
      if ((is.null(members) || length(members) == 0) && !is.null(set_obj$data)) {
        data_source <- set_obj$data
        if (is.vector(data_source) || is.factor(data_source)) {
          members <- unique(as.character(data_source))
        } else if (is.data.frame(data_source) && nrow(data_source) > 0) {
          primary_col <- setdiff(colnames(data_source), c("desc", "description"))
          if (length(primary_col) > 0) {
            members <- unique(as.character(data_source[[primary_col[1]]]))
          }
        }
      }
      if ((is.null(members) || length(members) == 0) && !is.null(model)) {
        members <- .extract_set_members(model, set_name)
      }
      set_inline <- set_obj
      set_inline$members <- members
      literal <- as_jump(set_inline, context = "data", model = model)
      desc <- .inline_desc(set_obj$desc, set_name)
      lines <- c(lines, sprintf("%s = %s%s", set_name, literal, desc))
    }
  } else {
    lines <- c(lines, "# No sets defined")
  }
  lines <- c(lines, "")
  if (!is.null(model$aliases)) {
    lines <- c(lines, "# Set aliases")
    for (alias_group in model$aliases) {
      if (length(alias_group) > 1) {
        primary <- alias_group[1]
        aliases <- alias_group[-1]
        for (alias_name in aliases) {
          lines <- c(lines, sprintf("%s = %s  # Alias for %s", alias_name, primary, primary))
        }
      }
    }
    lines <- c(lines, "")
  }
  if (!is.null(shifts_needed) && length(as.list(shifts_needed)) > 0) {
    shifts_list <- as.list(shifts_needed)
    lines <- c(lines, "# Index shift dictionaries for ordered sets")
    for (shift_info in shifts_list) {
      set_name <- shift_info$set_name
      offset <- shift_info$offset
      if (offset < 0) {
        dict_name <- paste0(tolower(set_name), "_lag", abs(offset))
        comment <- sprintf("Maps %s to previous element (offset %d)", set_name, offset)
        dict_def <- sprintf("%s = Dict(%s[i] => %s[i%d] for i in %d:length(%s))", dict_name, set_name, set_name, offset, abs(offset) + 1, set_name)
      } else {
        dict_name <- paste0(tolower(set_name), "_lead", offset)
        comment <- sprintf("Maps %s to next element (offset +%d)", set_name, offset)
        dict_def <- sprintf("%s = Dict(%s[i] => %s[i+%d] for i in 1:(length(%s)-%d))", dict_name, set_name, set_name, offset, set_name, offset)
      }
      lines <- c(lines, paste0(dict_def, "  # ", comment))
    }
    lines <- c(lines, "")
  }
  lines <- c(lines, "# Load mappings")
  if (!is.null(model$mappings) && length(model$mappings) > 0) {
    for (mapping_name in names(model$mappings)) {
      mapping_obj <- model$mappings[[mapping_name]]
      if (isTRUE(mapping_obj$trimmed)) next
      literal <- tryCatch(as_jump(mapping_obj, context = "data", model = model), error = function(e) "Set([])")
      desc <- .inline_desc(mapping_obj$desc, mapping_name)
      lines <- c(lines, sprintf("%s = %s%s", mapping_name, literal, desc))
    }
  } else {
    lines <- c(lines, "# No mappings defined")
  }
  lines <- c(lines, "")
  lines <- c(lines, "# Load parameters")
  if (!is.null(model$parameters) && length(model$parameters) > 0) {
    for (param_name in names(model$parameters)) {
      param_obj <- model$parameters[[param_name]]
      if (isTRUE(param_obj$trimmed)) next
      def_literal <- .format_parameter_default_literal(param_obj)
      lines <- c(lines, sprintf("%sDef = %s", param_name, def_literal))
      param_data_obj <- param_obj
      if (use_folded && !is.null(param_obj$folded_data) && !is.null(param_obj$active_dims)) {
        param_data_obj <- param_obj
        param_data_obj$data <- param_obj$folded_data
        param_data_obj$dims <- param_obj$active_dims
      }
      literal <- tryCatch(as_jump(param_data_obj, context = "data", model = model), error = function(e) NULL)
      desc <- .inline_desc(param_obj$desc, param_name)
      if (!is.null(literal)) {
        lines <- c(lines, sprintf("%s = %s%s", param_name, literal, desc))
      } else if (!is.null(param_obj$formula)) {
        lines <- c(lines, sprintf("%s = Dict()  # Computed parameter placeholder", param_name))
      } else {
        lines <- c(lines, sprintf("%s = Dict()", param_name))
      }
    }
  }
  lines <- c(lines, "")
  lines
}

#' Internal function to write JuMP model
#'
#' @keywords internal
write_jump_internal <- function(model, file = NULL, model_dir = NULL, solver_dir = "jump",
                                 output_name = "model.jl", model_name = "model",
                                 optimizer = "HiGHS.Optimizer", optimizer_attributes = NULL,
                                 use_folded = TRUE, cleanup = TRUE,
                                 export_data = FALSE, export_vars = FALSE,
                                 export_lp = FALSE, export_mps = FALSE,
                                 use_haskey = TRUE, data_mode = c("external", "embedded"), ...) {
  
  data_mode <- match.arg(data_mode)  # Create environment to track shift dictionaries needed
  shifts_needed <- new.env(parent = emptyenv())

  # Determine output file path
  if (is.null(file)) {
    if (!is.null(model_dir)) {
      # Create solvers/solver_dir structure
      solver_path <- file.path(model_dir, "solvers", solver_dir)

      # Cleanup existing solver directory if requested
      if (cleanup && dir.exists(solver_path)) {
        message("Removing existing solver directory: ", solver_path)
        unlink(solver_path, recursive = TRUE)
      }

      dir.create(solver_path, recursive = TRUE, showWarnings = FALSE)
      file <- file.path(solver_path, output_name)
      
      # Note: data.jl will be generated AFTER model.jl so we can collect shift dictionaries
    }
  }

  # Save set data to Arrow files if model_dir is provided
  if (!is.null(model_dir) && !is.null(model$sets) && length(model$sets) > 0) {
    sets_dir <- file.path(model_dir, "sets")
    dir.create(sets_dir, showWarnings = FALSE, recursive = TRUE)

    for (set_name in names(model$sets)) {
      set_obj <- model$sets[[set_name]]
      set_data <- NULL

      # Get set members from data or members field
      if (!is.null(set_obj$data) && length(set_obj$data) > 0) {
        set_data <- set_obj$data
      } else if (!is.null(set_obj$members) && length(set_obj$members) > 0) {
        set_data <- set_obj$members
      }

      # Save to Arrow if data exists
      if (!is.null(set_data) && length(set_data) > 0) {
        set_subdir <- file.path(sets_dir, set_name)
        dir.create(set_subdir, showWarnings = FALSE, recursive = TRUE)

        # Create dataframe with set members in first column
        df <- data.frame(member = as.character(set_data), stringsAsFactors = FALSE)
        arrow::write_feather(df, file.path(set_subdir, "data.arrow"))
      }
    }
  }

  lines <- character()

  # Julia header with imports
  lines <- c(lines, "# Julia/JuMP optimization model")
  if (!is.null(model$name) && !is.na(model$name)) {
    lines <- c(lines, paste0("# Model: ", model$name))
  }
  if (!is.null(model$desc) && !is.na(model$desc)) {
    lines <- c(lines, paste0("# ", model$desc))
  }
  lines <- c(lines, "")

  # Package installation check
  lines <- c(lines, "# Check and install required packages")
  lines <- c(lines, "import Pkg")

  # Determine required packages
  optimizer_package <- sub("\\..*$", "", optimizer)
  required_pkgs <- c("JuMP", "Arrow", "DataFrames", "CSV", "Dates")
  if (optimizer_package != "Optimizer") {
    required_pkgs <- c(required_pkgs, optimizer_package)
  }

  lines <- c(lines, paste0("required_packages = [\"", paste(required_pkgs, collapse = "\", \""), "\"]"))
  lines <- c(lines, "for pkg in required_packages")
  lines <- c(lines, "    if !haskey(Pkg.project().dependencies, pkg)")
  lines <- c(lines, "        println(\"Installing package: $pkg\")")
  lines <- c(lines, "        Pkg.add(pkg)")
  lines <- c(lines, "    end")
  lines <- c(lines, "end")
  lines <- c(lines, "")

  lines <- c(lines, "using JuMP")

  # Add optimizer package
  if (optimizer_package != "Optimizer") {
    lines <- c(lines, paste0("using ", optimizer_package))
  }
  lines <- c(lines, "")

  # Data loading from external file
  lines <- c(lines, "# Load data functions and validation")
  lines <- c(lines, "using Dates")
  lines <- c(lines, 'include("data.jl")')
  lines <- c(lines, "")
  lines <- c(lines, "println(\"Building model... \", Dates.format(now(), \"HH:MM:SS\"))")
  lines <- c(lines, "")

  # Create model
  lines <- c(lines, "# Create optimization model")
  lines <- c(lines, paste0(model_name, " = Model(", optimizer, ")"))
  lines <- c(lines, "")
  
  # Set optimizer attributes if provided
  if (!is.null(optimizer_attributes) && length(optimizer_attributes) > 0) {
    lines <- c(lines, "# Configure optimizer attributes")
    for (attr_name in names(optimizer_attributes)) {
      attr_value <- optimizer_attributes[[attr_name]]
      # Format value appropriately for Julia
      if (is.character(attr_value)) {
        attr_value_str <- paste0('"', attr_value, '"')
      } else if (is.logical(attr_value)) {
        attr_value_str <- tolower(as.character(attr_value))
      } else {
        attr_value_str <- as.character(attr_value)
      }
      lines <- c(lines, paste0('set_optimizer_attribute(', model_name, ', "', attr_name, '", ', attr_value_str, ')'))
    }
    lines <- c(lines, "")
  }
  
  # Auto-enable barrier method for large models (HiGHS specific)
  # Only if optimizer_attributes not explicitly provided and using HiGHS
  if (is.null(optimizer_attributes) && grepl("HiGHS", optimizer, fixed = TRUE)) {
    # We'll add logic after constraints to count them and conditionally enable barrier
    auto_barrier <- TRUE
  } else {
    auto_barrier <- FALSE
  }
  
  lines <- c(lines, "")

  # Sets - already loaded in data.jl
  lines <- c(lines, "# Sets (already loaded from data.jl)")
  lines <- c(lines, "")

  # Mappings - already loaded in data.jl
  lines <- c(lines, "# Mappings (already loaded from data.jl)")
  lines <- c(lines, "")

  # Parameters - already loaded in data.jl
  lines <- c(lines, "# Parameters (already loaded from data.jl)")
  lines <- c(lines, "")

  # Prepare objective metadata (needed before declaring variables so we can skip helper vars)
  objective_helper_var <- NULL
  objective_equation_name <- NULL
  objective_expr <- NULL
  objective_sense <- "Min"
  if (!is.null(model$objectives) && length(model$objectives) > 0) {
    obj_info <- model$objectives[[1]]
    sense <- if (!is.null(obj_info$sense)) tolower(obj_info$sense) else "minimize"
    objective_sense <- if (sense %in% c("min", "minimize")) "Min" else "Max"
    objective_expr <- obj_info$variable
    eq_name <- obj_info$equation
    if (!is.null(eq_name) && nzchar(eq_name)) {
      eq_source <- NULL
      if (use_folded && !is.null(model$folded_equations) && !is.null(model$folded_equations[[eq_name]])) {
        eq_source <- model$folded_equations[[eq_name]]
      } else if (!is.null(model$equations) && !is.null(model$equations[[eq_name]])) {
        eq_source <- model$equations[[eq_name]]
      }
      if (!is.null(eq_source) && !is.null(eq_source$rhs)) {
        expr_candidate <- tryCatch({
          # Use equation-specific dims_index_aliases (e.g., REGION -> r, YEAR -> y)
          # rather than model$index_aliases, so iterators match the equation's original variables
          var_names_for_obj <- if (!is.null(eq_source$dims_index_aliases)) {
            eq_source$dims_index_aliases
          } else {
            NULL
          }
          as_jump(eq_source$rhs, model = model, var_names = var_names_for_obj, 
                  use_haskey = use_haskey, model_dir = model_dir, shifts_tracker = shifts_needed)
        }, error = function(e) {
          warning("Failed to convert objective RHS from equation '", eq_name, "': ", conditionMessage(e))
          NULL
        })
        if (!is.null(expr_candidate) && nzchar(expr_candidate)) {
          objective_expr <- expr_candidate
          objective_helper_var <- obj_info$variable
          objective_equation_name <- eq_name
        }
      }
    }
  }

  # Variables with sparse indexing
  lines <- c(lines, "# Variables")
  if (!is.null(model$variables) && length(model$variables) > 0) {
    for (var_name in names(model$variables)) {

      if (!is.null(objective_helper_var) && identical(var_name, objective_helper_var)) {
        next
      }
      v <- model$variables[[var_name]]

      # Skip trimmed variables
      if (isTRUE(v$trimmed)) {
        lines <- c(lines, paste0("# ", var_name, " trimmed (unused)"))
        next
      }

      # Skip empty/unused variables (domain = character(0))
      if (!is.null(v$domain) && length(v$domain) == 0) {
        lines <- c(lines, paste0("# ", var_name, " is unused in this scenario"))
        next
      }

      # Build variable declaration
      var_decl <- paste0("@variable(", model_name, ", ", var_name)

      # Add indexing if variable has dimensions
      if (!is.null(v$dims) && length(v$dims) > 0) {
        # Extract dimension names
        dim_names <- sapply(v$dims, function(d) {
          if (inherits(d, "symbol")) d$name
          else if (is.character(d)) d
          else as.character(d)
        })

        # Check for sparse domain mapping
        # domain = NULL: use Cartesian product (default)
        # domain = "mappingName": use sparse indexing over mapping
        # domain = character(0): skip (handled above)

        if (!is.null(v$domain) && is.character(v$domain) && length(v$domain) == 1 && nzchar(v$domain)) {
          # Domain specified - use sparse indexing
          mapping_name <- v$domain

          # Verify mapping exists
          if (!is.null(model$mappings) && mapping_name %in% names(model$mappings)) {
            # Sparse: index only over valid combinations from mapping
            index_str <- paste0("[", mapping_name, "]")
          } else {
            # Mapping not found - warn and fall back to Cartesian
            warning("Variable '", var_name, "' specifies domain '", mapping_name,
                    "' but mapping not found. Using Cartesian product instead.")
            index_str <- paste0("[", paste(dim_names, collapse = ", "), "]")
          }
        } else {
          # No domain specified - use Cartesian product (default)
          index_str <- paste0("[", paste(dim_names, collapse = ", "), "]")
        }

        var_decl <- paste0(var_decl, index_str)
      }

      # Add bounds - support both bounds list (lo/up) and old lb/ub fields
      lb <- if (!is.null(v$bounds) && !is.null(v$bounds$lo)) v$bounds$lo
            else if (!is.null(v$bounds) && !is.null(v$bounds$lb)) v$bounds$lb
            else v$lb
      ub <- if (!is.null(v$bounds) && !is.null(v$bounds$up)) v$bounds$up
            else if (!is.null(v$bounds) && !is.null(v$bounds$ub)) v$bounds$ub
            else v$ub

      # Handle "positive" or "positive variables" type (implies >= 0)
      if (!is.null(v$vtype) && tolower(v$vtype) == "positive" && is.null(lb)) {
        lb <- 0
      } else if (!is.null(v$type) && tolower(v$type) == "positive" && is.null(lb)) {
        lb <- 0
      }

      # Add bounds only if they are finite (skip -Inf lower and Inf upper)
      has_lower <- !is.null(lb) && is.numeric(lb) && is.finite(lb)
      has_upper <- !is.null(ub) && is.numeric(ub) && is.finite(ub)
      
      if (has_lower && has_upper) {
        var_decl <- paste0(var_decl, " >= ", lb, ", <= ", ub)
      } else if (has_lower) {
        var_decl <- paste0(var_decl, " >= ", lb)
      } else if (has_upper) {
        var_decl <- paste0(var_decl, " <= ", ub)
      }

      # Add variable type - check v$vtype (GMPL), v$type (new), and v$domain (old backward compat)
      var_type <- v$vtype
      if (is.null(var_type)) var_type <- v$type
      if (is.null(var_type) && !is.null(v$domain)) {
        # Old structure: domain could be "binary" or "integer"
        if (v$domain %in% c("binary", "integer")) {
          var_type <- v$domain
        }
      }

      if (!is.null(var_type)) {
        if (var_type == "binary") {
          var_decl <- paste0(var_decl, ", Bin")
        } else if (var_type == "integer") {
          var_decl <- paste0(var_decl, ", Int")
        }
      }

      var_decl <- paste0(var_decl, ")")

      # Add description as comment
      if (!is.null(v$desc) && nzchar(v$desc)) {
        var_decl <- paste0(var_decl, "  # ", v$desc)
      }

      lines <- c(lines, var_decl)
    }
    lines <- c(lines, "")
    lines <- c(lines, "println(\"Variables declared: \", Dates.format(now(), \"HH:MM:SS\"))")
    lines <- c(lines, "")
  }

  # Objective function
  if (!is.null(model$objectives) && length(model$objectives) > 0) {
    lines <- c(lines, "# Objective function")
    lines <- c(lines, paste0("@objective(", model_name, ", ", objective_sense, ", ", objective_expr, ")"))
    lines <- c(lines, "")
  } else if ("vObjective" %in% names(model$variables)) {
    # Fallback: if vObjective variable exists, minimize it
    lines <- c(lines, "# Objective function (inferred from vObjective variable)")
    lines <- c(lines, paste0("@objective(", model_name, ", Min, vObjective)"))
    lines <- c(lines, "")
  }

  # Constraints with progress tracking
  lines <- c(lines, "# Constraints")
  lines <- c(lines, "println(\"Building constraints...\")")
  lines <- c(lines, "")

  # Initialize CSV logging and diagnostics functions
  lines <- c(lines, "# Initialize CSV files for model statistics")
  lines <- c(lines, "constraint_stats = open(joinpath(@__DIR__, \"constraint_stats.csv\"), \"w\")")
  lines <- c(lines, "println(constraint_stats, \"name,count,dimension,mapping_size,time_seconds,nnz_total,nnz_min,nnz_max,nnz_avg\")")
  lines <- c(lines, "variable_stats = open(joinpath(@__DIR__, \"variable_stats.csv\"), \"w\")")
  lines <- c(lines, "println(variable_stats, \"name,count,dimension,fixed,free,lower_only,upper_only,bounded\")")
  lines <- c(lines, "")

  # Add diagnostics helper functions
  lines <- c(lines, "# Helper functions for model diagnostics")
  lines <- c(lines, "function _parse_con_dim(nm::AbstractString)")
  lines <- c(lines, "    i = findfirst('[', nm)")
  lines <- c(lines, "    i === nothing && return 0")
  lines <- c(lines, "    j = findlast(']', nm)")
  lines <- c(lines, "    (j === nothing || j <= i) && return 0")
  lines <- c(lines, "    inner = String(nm[i+1:j-1])")
  lines <- c(lines, "    return isempty(strip(inner)) ? 0 : length(split(inner, ','))")
  lines <- c(lines, "end")
  lines <- c(lines, "")
  lines <- c(lines, "function _log_constraint(file, name, constr, mapping_size, elapsed)")
  lines <- c(lines, "    cnt = isa(constr, ConstraintRef) ? 1 : length(constr)")
  lines <- c(lines, "    ")
  lines <- c(lines, "    # Handle empty constraints")
  lines <- c(lines, "    if cnt == 0")
  lines <- c(lines, "        map_csv = isnan(mapping_size) ? \"NA\" : string(Int(mapping_size))")
  lines <- c(lines, "        println(\"  $name: 0 x 0, $(round(elapsed, digits=1))s, nnz=0\")")
  lines <- c(lines, "        println(file, \"$name,0,0,$map_csv,$(round(elapsed, digits=3)),0,0,0,0.0\")")
  lines <- c(lines, "        return")
  lines <- c(lines, "    end")
  lines <- c(lines, "    ")
  lines <- c(lines, "    dim = isa(constr, ConstraintRef) ? _parse_con_dim(JuMP.name(constr)) : _parse_con_dim(JuMP.name(first(constr)))")
  lines <- c(lines, "    ")
  lines <- c(lines, "    # Calculate constraint non-zeros statistics")
  lines <- c(lines, "    nnz_vals = Int[]")
  lines <- c(lines, "    if isa(constr, ConstraintRef)")
  lines <- c(lines, "        cobj = JuMP.constraint_object(constr)")
  lines <- c(lines, "        func = cobj.func")
  lines <- c(lines, "        if func isa AffExpr")
  lines <- c(lines, "            push!(nnz_vals, length(func.terms))")
  lines <- c(lines, "        end")
  lines <- c(lines, "    else")
  lines <- c(lines, "        for cref in constr")
  lines <- c(lines, "            cobj = JuMP.constraint_object(cref)")
  lines <- c(lines, "            func = cobj.func")
  lines <- c(lines, "            if func isa AffExpr")
  lines <- c(lines, "                push!(nnz_vals, length(func.terms))")
  lines <- c(lines, "            end")
  lines <- c(lines, "        end")
  lines <- c(lines, "    end")
  lines <- c(lines, "    ")
  lines <- c(lines, "    nnz_total = sum(nnz_vals)")
  lines <- c(lines, "    nnz_min = isempty(nnz_vals) ? 0 : minimum(nnz_vals)")
  lines <- c(lines, "    nnz_max = isempty(nnz_vals) ? 0 : maximum(nnz_vals)")
  lines <- c(lines, "    nnz_avg = isempty(nnz_vals) ? 0.0 : round(sum(nnz_vals) / length(nnz_vals), digits=1)")
  lines <- c(lines, "    ")
  lines <- c(lines, "    # Validate constraint count")
  lines <- c(lines, "    if !isnan(mapping_size) && cnt > mapping_size")
  lines <- c(lines, "        error(\"$name: constraint count ($cnt) exceeds mapping size ($mapping_size)\")")
  lines <- c(lines, "    end")
  lines <- c(lines, "    ")
  lines <- c(lines, "    # Console output")
  lines <- c(lines, "    map_str = isnan(mapping_size) ? \"\" : \" (of $(Int(mapping_size)))\"; ")
  lines <- c(lines, "    println(\"  $name: $cnt x $dim$map_str, $(round(elapsed, digits=1))s, nnz=$nnz_total\")")
  lines <- c(lines, "    ")
  lines <- c(lines, "    # CSV output")
  lines <- c(lines, "    map_csv = isnan(mapping_size) ? \"NA\" : string(Int(mapping_size))")
  lines <- c(lines, "    println(file, \"$name,$cnt,$dim,$map_csv,$(round(elapsed, digits=3)),$nnz_total,$nnz_min,$nnz_max,$nnz_avg\")")
  lines <- c(lines, "end")
  lines <- c(lines, "")
  lines <- c(lines, "function _log_variable(file, name, var, dim)")
  lines <- c(lines, "    cnt = isa(var, VariableRef) ? 1 : length(var)")
  lines <- c(lines, "    ")
  lines <- c(lines, "    # Analyze variable bounds")
  lines <- c(lines, "    n_fixed = 0")
  lines <- c(lines, "    n_free = 0")
  lines <- c(lines, "    n_lower = 0")
  lines <- c(lines, "    n_upper = 0")
  lines <- c(lines, "    n_bounded = 0")
  lines <- c(lines, "    ")
  lines <- c(lines, "    if isa(var, VariableRef)")
  lines <- c(lines, "        vars = [var]")
  lines <- c(lines, "    else")
  lines <- c(lines, "        vars = var")
  lines <- c(lines, "    end")
  lines <- c(lines, "    ")
  lines <- c(lines, "    for v in vars")
  lines <- c(lines, "        lb = has_lower_bound(v) ? lower_bound(v) : -Inf")
  lines <- c(lines, "        ub = has_upper_bound(v) ? upper_bound(v) : Inf")
  lines <- c(lines, "        ")
  lines <- c(lines, "        if is_fixed(v) || (isfinite(lb) && isfinite(ub) && abs(lb - ub) < 1e-10)")
  lines <- c(lines, "            n_fixed += 1")
  lines <- c(lines, "        elseif !isfinite(lb) && !isfinite(ub)")
  lines <- c(lines, "            n_free += 1")
  lines <- c(lines, "        elseif isfinite(lb) && !isfinite(ub)")
  lines <- c(lines, "            n_lower += 1")
  lines <- c(lines, "        elseif !isfinite(lb) && isfinite(ub)")
  lines <- c(lines, "            n_upper += 1")
  lines <- c(lines, "        else")
  lines <- c(lines, "            n_bounded += 1")
  lines <- c(lines, "        end")
  lines <- c(lines, "    end")
  lines <- c(lines, "    ")
  lines <- c(lines, "    println(file, \"$name,$cnt,$dim,$n_fixed,$n_free,$n_lower,$n_upper,$n_bounded\")")
  lines <- c(lines, "end")
  lines <- c(lines, "")

  # Use folded equations if available and requested
  equations_to_write <- if (use_folded && !is.null(model$folded_equations) && length(model$folded_equations) > 0) {
    model$folded_equations
  } else {
    model$equations
  }

  if (!is.null(objective_equation_name) && !is.null(equations_to_write) && length(equations_to_write) > 0) {
    if (!is.null(equations_to_write[[objective_equation_name]])) {
      equations_to_write[[objective_equation_name]] <- NULL
    }
  }

  if (!is.null(equations_to_write) && length(equations_to_write) > 0) {
    for (eq_name in names(equations_to_write)) {
      eq <- equations_to_write[[eq_name]]

      # Skip trimmed equations
      if (isTRUE(eq$trimmed)) {
        lines <- c(lines, paste0("# ", eq_name, " trimmed (unused)"))
        next
      }

      # Start timing
      lines <- c(lines, paste0("_t0 = time()"))

      # Add description as comment
      if (!is.null(eq$desc) && nzchar(eq$desc)) {
        lines <- c(lines, paste0("# ", eq$desc))
      }

      # Generate constraint using as_jump()
      constraint_code <- tryCatch({
        as_jump(eq, model = model, use_haskey = use_haskey, model_dir = model_dir, shifts_tracker = shifts_needed)
      }, error = function(e) {
        stop("Failed to convert equation ", eq_name, ": ", e$message, call. = FALSE)
      })

      lines <- c(lines, constraint_code)

      # Log constraint statistics
      # For indexed constraints (with dims), use length() to count actual constraints created
      # For scalar constraints (no dims), use 1
      if (!is.null(eq$dims) && length(eq$dims) > 0) {
        # Indexed constraint (may be filtered) - use length() to count actual constraints
        lines <- c(lines, paste0("_log_constraint(constraint_stats, \"", eq_name, "\", ", eq_name, ", length(", eq_name, "), time() - _t0)"))
      } else {
        # Scalar constraint (no dims) - use 1 as mapping size
        lines <- c(lines, paste0("_log_constraint(constraint_stats, \"", eq_name, "\", ", eq_name, ", 1, time() - _t0)"))
      }
      lines <- c(lines, "")
    }
    lines <- c(lines, "println(\"All constraints built.\")")
    lines <- c(lines, "close(constraint_stats)")
    lines <- c(lines, "")
    
    # Memory cleanup: drop parameter and mapping data (no longer needed)
    lines <- c(lines, "# Memory cleanup: free parameter and mapping data")
    if (!is.null(model$parameters) && length(model$parameters) > 0) {
      for (param_name in names(model$parameters)) {
        param_obj <- model$parameters[[param_name]]
        if (isTRUE(param_obj$trimmed)) next
        lines <- c(lines, paste0(param_name, " = nothing  # Free parameter data"))
      }
    }
    if (!is.null(model$mappings) && length(model$mappings) > 0) {
      for (mapping_name in names(model$mappings)) {
        mapping_obj <- model$mappings[[mapping_name]]
        if (isTRUE(mapping_obj$trimmed)) next
        lines <- c(lines, paste0(mapping_name, " = nothing  # Free mapping data"))
      }
    }
    lines <- c(lines, "GC.gc()  # Run garbage collection")
    lines <- c(lines, "println(\"Used memory: \", round(Sys.total_memory() / 1e9, digits=1), \" GB total, \", round((Sys.total_memory() - Sys.free_memory()) / 1e9, digits=1), \" GB used\")")
    lines <- c(lines, "")
    
    # Auto-configure barrier method for large models (HiGHS only)
    if (auto_barrier) {
      lines <- c(lines, "# Auto-configure solver for large models")
      lines <- c(lines, paste0("_n_constraints = num_constraints(", model_name, ", AffExpr, MOI.EqualTo{Float64}) + num_constraints(", model_name, ", AffExpr, MOI.LessThan{Float64}) + num_constraints(", model_name, ", AffExpr, MOI.GreaterThan{Float64})"))
      lines <- c(lines, "if _n_constraints > 1_000_000")
      lines <- c(lines, "    println(\"Large model detected (\", _n_constraints, \" constraints). Enabling barrier method for HiGHS.\")")
      lines <- c(lines, paste0("    set_optimizer_attribute(", model_name, ', "solver", "ipm")'))
      lines <- c(lines, paste0("    set_optimizer_attribute(", model_name, ', "parallel", "on")'))
      lines <- c(lines, paste0("    set_optimizer_attribute(", model_name, ', "run_crossover", "off")  # Disable crossover for large models (crossover often takes longer than barrier)'))
      lines <- c(lines, "    # Note: To enable crossover for exact vertex solution, set: optimizer_attributes = list(solver = \"ipm\", parallel = \"on\", run_crossover = \"on\")")
      lines <- c(lines, "else")
      lines <- c(lines, "    println(\"Model size: \", _n_constraints, \" constraints. Using HiGHS default solver.\")")
      lines <- c(lines, "end")
      lines <- c(lines, "")
    }
  } else {
    lines <- c(lines, "# No constraints defined")
    lines <- c(lines, "close(constraint_stats)")
    lines <- c(lines, "")
  }

  # Variable statistics
  if (!is.null(model$variables) && length(model$variables) > 0) {
    lines <- c(lines, "# Log variable statistics")
    for (var_name in names(model$variables)) {
      v <- model$variables[[var_name]]

      if (!is.null(objective_helper_var) && identical(var_name, objective_helper_var)) next

      # Skip trimmed variables
      if (isTRUE(v$trimmed)) next

      dim_count <- if (!is.null(v$dims)) length(v$dims) else 0
      lines <- c(lines, paste0("_log_variable(variable_stats, \"", var_name, "\", ", var_name, ", ", dim_count, ")"))
    }
    lines <- c(lines, "close(variable_stats)")
    lines <- c(lines, "")
  } else {
    lines <- c(lines, "close(variable_stats)")
    lines <- c(lines, "")
  }

  # Export LP/MPS files if requested
  export_lp_enabled <- !isFALSE(export_lp)
  export_mps_enabled <- !isFALSE(export_mps)
  
  if (export_lp_enabled || export_mps_enabled) {
    lines <- c(lines, "# Export model files")
    if (export_lp_enabled) {
      lp_file <- if (is.character(export_lp)) export_lp else "model.lp"
      lines <- c(lines, sprintf('lp_file = joinpath(@__DIR__, "%s")', lp_file))
      lines <- c(lines, sprintf('println("Exporting LP format to: ", lp_file)'))
      lines <- c(lines, paste0('write_to_file(', model_name, ', lp_file; format = MOI.FileFormats.FORMAT_LP)'))
    }
    if (export_mps_enabled) {
      mps_file <- if (is.character(export_mps)) export_mps else "model.mps"
      lines <- c(lines, sprintf('mps_file = joinpath(@__DIR__, "%s")', mps_file))
      lines <- c(lines, sprintf('println("Exporting MPS format to: ", mps_file)'))
      lines <- c(lines, paste0('write_to_file(', model_name, ', mps_file; format = MOI.FileFormats.FORMAT_MPS)'))
    }
    lines <- c(lines, "")
  }

  # Solve and save solution
  lines <- c(lines, "# Solve the model")
  lines <- c(lines, "println(\"Solving... \", Dates.format(now(), \"HH:MM:SS\"))")
  lines <- c(lines, paste0("optimize!(", model_name, ")"))
  lines <- c(lines, "")
  lines <- c(lines, "# Display results")
  lines <- c(lines, "println(\"Solve complete: \", Dates.format(now(), \"HH:MM:SS\"))")
  lines <- c(lines, paste0('println("Termination status: ", termination_status(', model_name, '))'))
  lines <- c(lines, paste0('println("Objective value: ", objective_value(', model_name, '))'))
  lines <- c(lines, "")

  # Add solution saving function
  lines <- c(lines, "# Save solution values")
  lines <- c(lines, "function save_solution(model, output_dir)")
  lines <- c(lines, "    if !isdir(output_dir)")
  lines <- c(lines, "        mkpath(output_dir)")
  lines <- c(lines, "    end")
  lines <- c(lines, "    ")
  lines <- c(lines, "    println(\"\\nSaving solution to: \", output_dir)")
  lines <- c(lines, "    ")
  lines <- c(lines, "    # Determine file extension")
  lines <- c(lines, "    ext = DATA_EXT")
  lines <- c(lines, "    saved_count = 0")
  lines <- c(lines, "    ")
  lines <- c(lines, "    # Get all variables from model")
  lines <- c(lines, "    all_vars = all_variables(model)")
  lines <- c(lines, "    ")
  lines <- c(lines, "    # Group variables by name (remove indices)")
  lines <- c(lines, "    var_groups = Dict{String, Vector{VariableRef}}()")
  lines <- c(lines, "    for var in all_vars")
  lines <- c(lines, "        var_name = name(var)")
  lines <- c(lines, "        # Extract base name (before [)")
  lines <- c(lines, "        base_name = split(var_name, '[')[1]")
  lines <- c(lines, "        if !haskey(var_groups, base_name)")
  lines <- c(lines, "            var_groups[base_name] = VariableRef[]")
  lines <- c(lines, "        end")
  lines <- c(lines, "        push!(var_groups[base_name], var)")
  lines <- c(lines, "    end")
  lines <- c(lines, "    ")
  lines <- c(lines, "    # Save each variable group")
  lines <- c(lines, "    for (var_name, vars) in var_groups")
  lines <- c(lines, "        try")
  lines <- c(lines, "            rows = []")
  lines <- c(lines, "            ")
  lines <- c(lines, "            for var in vars")
  lines <- c(lines, "                val = value(var)")
  lines <- c(lines, "                # Export ALL values when export_variables=TRUE (no filtering)")
  lines <- c(lines, "                full_name = name(var)")
  lines <- c(lines, "                # Parse indices from name like vTechCap[E01,R1,1990]")
  lines <- c(lines, "                if occursin('[', full_name)")
  lines <- c(lines, "                    idx_str = split(split(full_name, '[')[2], ']')[1]")
  lines <- c(lines, "                    indices = [strip(replace(s, r\"[\\\"\\(\\)]\" => \"\")) for s in split(idx_str, ',')]")
  lines <- c(lines, "                    push!(rows, vcat(indices, val))")
  lines <- c(lines, "                else")
  lines <- c(lines, "                    # Scalar variable")
  lines <- c(lines, "                    push!(rows, [val])")
  lines <- c(lines, "                end")
  lines <- c(lines, "            end")
  lines <- c(lines, "            ")
  lines <- c(lines, "            if !isempty(rows)")
  lines <- c(lines, "                # Determine column names")
  lines <- c(lines, "                if length(rows[1]) > 1")
  lines <- c(lines, "                    n_dims = length(rows[1]) - 1")
  lines <- c(lines, "                    col_names = [Symbol(string(\"dim\", i)) for i in 1:n_dims]")
  lines <- c(lines, "                    push!(col_names, :value)")
  lines <- c(lines, "                else")
  lines <- c(lines, "                    col_names = [:value]")
  lines <- c(lines, "                end")
  lines <- c(lines, "                ")
  lines <- c(lines, "                # Create DataFrame")
  lines <- c(lines, "                df = DataFrame([row[i] for row in rows, i in 1:length(rows[1])], col_names)")
  lines <- c(lines, "                ")
  lines <- c(lines, "                # Save directly to solution folder (no subfolders)")
  lines <- c(lines, "                out_file = joinpath(output_dir, var_name * ext)")
  lines <- c(lines, "                ")
  lines <- c(lines, "                if ext == \".csv\"")
  lines <- c(lines, "                    CSV.write(out_file, df)")
  lines <- c(lines, "                else")
  lines <- c(lines, "                    Arrow.write(out_file, df)")
  lines <- c(lines, "                end")
  lines <- c(lines, "                ")
  lines <- c(lines, "                saved_count += 1")
  lines <- c(lines, "            end")
  lines <- c(lines, "        catch e")
  lines <- c(lines, "            println(\"  Warning: Could not save \", var_name, \": \", e)")
  lines <- c(lines, "        end")
  lines <- c(lines, "    end")
  lines <- c(lines, "    ")
  lines <- c(lines, "    println(\"Solution saved: \", saved_count, \" variables with non-zero values\\n\")")
  lines <- c(lines, "end")
  lines <- c(lines, "")
  
  # Save solution if optimal (ALWAYS save for load_results to work)
  lines <- c(lines, "# Save solution if optimal")
  lines <- c(lines, "if termination_status(model) in [MOI.OPTIMAL, MOI.LOCALLY_SOLVED]")
  lines <- c(lines, "    solution_dir = joinpath(@__DIR__, \"solution\")")
  lines <- c(lines, "    save_solution(model, solution_dir)")
  
  # Optional: Export variables to separate directory for debugging
  if (export_vars) {
    lines <- c(lines, "    ")
    lines <- c(lines, "    # Additional export for debugging")
    lines <- c(lines, "    vars_export_dir = joinpath(@__DIR__, \"vars_export\")")
    lines <- c(lines, "    save_solution(model, vars_export_dir)")
  }
  
  lines <- c(lines, "end")
  lines <- c(lines, "")

  # Write to file or return as character vector
  if (!is.null(file)) {
    writeLines(lines, file)
    
    # Now generate data.jl with collected shift dictionaries
    if (!is.null(model_dir)) {
      .generate_data_jl(model_dir, solver_dir, model, use_folded, shifts_needed, export_data, data_mode = data_mode)
    }
    
    invisible(lines)
  } else {
    lines
  }
}
