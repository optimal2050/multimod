#' Solve a JuMP Model
#'
#' Execute a Julia/JuMP optimization model with configurable backend options.
#' Results can be optionally loaded back into the model object.
#'
#' @param model A multimod model object (optional, used for loading results back)
#' @param model_dir Path to the model directory containing solvers/jump/
#' @param method Execution method: "system" (default), "juliacall", or "background"
#' @param verbose Print Julia output to console
#' @param load_results Load solution values back into model$variables
#' @param julia_args Additional arguments to pass to Julia (e.g., "--threads=4")
#' @param timeout Maximum execution time in seconds (NULL for no limit)
#'
#' @return List with solution information:
#'   \item{exit_code}{Julia process exit code (0 = success)}
#'   \item{success}{Logical indicating successful execution}
#'   \item{objective}{Objective function value (if available)}
#'   \item{status}{Solver termination status (if available)}
#'   \item{solve_time}{Solution time in seconds (if available)}
#'   \item{output}{Julia stdout/stderr (if verbose=TRUE)}
#'   \item{constraints}{Constraint statistics data.frame}
#'   \item{variables}{Variable statistics data.frame}
#'
#' @details
#' The function supports three execution methods:
#'
#' \strong{system} (default): Uses system2() to call Julia. No extra dependencies.
#' Simple, robust, works everywhere Julia is installed.
#'
#' \strong{juliacall}: Uses JuliaCall package for R-Julia integration. Allows
#' bi-directional data transfer and access to Julia objects from R. Requires
#' JuliaCall package.
#'
#' \strong{background}: Uses callr package for non-blocking execution. Allows
#' running multiple models in parallel. Requires callr package.
#'
#' Solution values are saved by the Julia model to solvers/jump/solution/
#' (in Arrow IPC or CSV format). If load_results=TRUE and model is provided,
#' solutions are loaded into result$model and moved to model_dir/variables/.
#' A log file (solution_log.txt) in model_dir tracks all solution loads.
#' You must reassign your model variable to access the solutions:
#' 
#' \code{result <- solve_jump(my_model, "path/to/model", load_results = TRUE)}
#' 
#' \code{my_model <- result$model  # Update with loaded solutions}
#'
#' @examples
#' \dontrun{
#' # Generate and solve model
#' model <- as_multimod(read_gams("model.gms"))
#' save_model(model, "tmp/my_model", format = "ipc")
#' write_jump(model, model_dir = "tmp/my_model")
#'
#' # Solve with default settings
#' result <- solve_jump(model_dir = "tmp/my_model")
#'
#' # Solve and load results back into model
#' result <- solve_jump(model, model_dir = "tmp/my_model", load_results = TRUE)
#' 
#' # Access solution values
#' vTechCap_solution <- model$variables$vTechCap$solution
#'
#' # Use different backend
#' result <- solve_jump(model_dir = "tmp/my_model", method = "juliacall")
#'
#' # Parallel execution with timeout
#' result <- solve_jump(model_dir = "tmp/my_model", 
#'                      method = "background", 
#'                      timeout = 3600)
#' }
#'
#' @export
solve_jump <- function(model = NULL,
                       model_dir,
                       method = c("system", "juliacall", "background"),
                       verbose = TRUE,
                       load_results = !is.null(model),
                       julia_args = NULL,
                       timeout = NULL) {
  
  method <- match.arg(method)
  
  # Validate paths
  julia_dir <- file.path(model_dir, "solvers", "jump")
  julia_file <- file.path(julia_dir, "model.jl")
  
  if (!file.exists(julia_file)) {
    stop("Julia model not found: ", julia_file)
  }
  
  # Check format
  format_file <- file.path(model_dir, "format.txt")
  if (file.exists(format_file)) {
    format_type <- tolower(trimws(readLines(format_file, n = 1)))
    data_ext <- if (format_type == "csv") ".csv" else ".arrow"
  } else {
    data_ext <- ".arrow"  # default
  }
  
  # Dispatch to appropriate backend
  result <- switch(method,
    system = solve_jump_system(julia_file, julia_args, verbose, timeout),
    juliacall = solve_jump_juliacall(julia_file, verbose),
    background = solve_jump_background(julia_file, julia_args, verbose, timeout)
  )
  
  # Read CSV diagnostics (always generated)
  con_stats_file <- file.path(julia_dir, "constraint_stats.csv")
  var_stats_file <- file.path(julia_dir, "variable_stats.csv")
  
  if (file.exists(con_stats_file)) {
    result$constraints <- read.csv(con_stats_file, stringsAsFactors = FALSE)
  }
  
  if (file.exists(var_stats_file)) {
    result$variables <- read.csv(var_stats_file, stringsAsFactors = FALSE)
  }
  
  # Load solution values back into model if requested
  if (load_results && !is.null(model) && result$success) {
    result$model <- load_jump_solution(model, model_dir, data_ext)
  }
  
  result
}


#' Solve with system2() backend
#' @noRd
solve_jump_system <- function(julia_file, julia_args, verbose, timeout) {
  
  # Get Julia executable path from config
  julia_exec <- get_multimod_julia()
  
  args <- c(julia_args, julia_file)
  
  # Execute Julia
  start_time <- Sys.time()
  
  if (is.null(timeout)) {
    output <- system2(julia_exec, args = args, 
                     stdout = TRUE, stderr = TRUE,
                     wait = TRUE)
  } else {
    # Use timeout wrapper (platform-specific)
    if (.Platform$OS.type == "windows") {
      # Windows: no built-in timeout, use powershell
      ps_cmd <- sprintf(
        "$proc = Start-Process '%s' -ArgumentList '%s' -PassThru -NoNewWindow -Wait; Start-Sleep -Seconds %d; if (!$proc.HasExited) { $proc.Kill() }",
        julia_exec, paste(args, collapse = " "), timeout
      )
      output <- system2("powershell", args = c("-Command", ps_cmd),
                       stdout = TRUE, stderr = TRUE, wait = TRUE)
    } else {
      # Unix: use timeout command
      output <- system2("timeout", args = c(timeout, julia_exec, args),
                       stdout = TRUE, stderr = TRUE, wait = TRUE)
    }
  }
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (verbose && !is.null(output)) {
    cat(paste(output, collapse = "\n"), "\n")
  }
  
  exit_code <- attr(output, "status") %||% 0
  
  # Parse key information from output
  result <- list(
    exit_code = exit_code,
    success = exit_code == 0,
    elapsed_time = elapsed,
    output = if (verbose) output else NULL
  )
  
  if (!is.null(output)) {
    result <- parse_julia_output(result, output)
  }
  
  result
}


#' Solve with JuliaCall backend
#' @noRd
solve_jump_juliacall <- function(julia_file, verbose) {
  
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("JuliaCall package required for method='juliacall'. Install with: install.packages('JuliaCall')")
  }
  
  # Setup Julia (only first time)
  if (!exists(".julia_initialized", envir = .GlobalEnv)) {
    JuliaCall::julia_setup()
    assign(".julia_initialized", TRUE, envir = .GlobalEnv)
  }
  
  # Change to model directory
  model_dir <- dirname(julia_file)
  JuliaCall::julia_command(sprintf("cd(\"%s\")", gsub("\\\\", "/", model_dir)))
  
  # Execute model
  start_time <- Sys.time()
  
  tryCatch({
    JuliaCall::julia_command(sprintf("include(\"%s\")", basename(julia_file)))
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Try to get objective and status from Julia
    obj <- tryCatch(JuliaCall::julia_eval("objective_value(model)"), error = function(e) NA)
    status <- tryCatch(JuliaCall::julia_eval("string(termination_status(model))"), error = function(e) NA)
    
    list(
      exit_code = 0,
      success = TRUE,
      elapsed_time = elapsed,
      objective = obj,
      status = status,
      output = if (verbose) "Solved via JuliaCall" else NULL
    )
  }, error = function(e) {
    list(
      exit_code = 1,
      success = FALSE,
      elapsed_time = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      error = e$message,
      output = if (verbose) e$message else NULL
    )
  })
}


#' Solve with callr background backend
#' @noRd
solve_jump_background <- function(julia_file, julia_args, verbose, timeout) {
  
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("callr package required for method='background'. Install with: install.packages('callr')")
  }
  
  # Get Julia executable path from config
  julia_exec <- get_multimod_julia()
  
  args <- c(julia_args, julia_file)
  
  # Start background process
  proc <- callr::r_bg(function(julia_exec, args) {
    system2(julia_exec, args = args, stdout = TRUE, stderr = TRUE, wait = TRUE)
  }, args = list(julia_exec = julia_exec, args = args))
  
  # Wait with timeout
  start_time <- Sys.time()
  elapsed <- 0
  
  while (proc$is_alive() && (is.null(timeout) || elapsed < timeout)) {
    Sys.sleep(0.1)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  }
  
  if (proc$is_alive()) {
    proc$kill()
    return(list(
      exit_code = 124,  # timeout exit code
      success = FALSE,
      elapsed_time = elapsed,
      error = sprintf("Timeout after %d seconds", timeout),
      output = if (verbose) sprintf("Process killed after %d seconds", timeout) else NULL
    ))
  }
  
  # Get results
  output <- proc$get_result()
  exit_code <- attr(output, "status") %||% 0
  
  if (verbose && !is.null(output)) {
    cat(paste(output, collapse = "\n"), "\n")
  }
  
  result <- list(
    exit_code = exit_code,
    success = exit_code == 0,
    elapsed_time = elapsed,
    output = if (verbose) output else NULL
  )
  
  if (!is.null(output)) {
    result <- parse_julia_output(result, output)
  }
  
  result
}


#' Parse Julia output for objective and status
#' @noRd
parse_julia_output <- function(result, output) {
  
  output_text <- paste(output, collapse = "\n")
  
  # Parse objective value
  obj_match <- regexpr("Objective value:\\s+([-0-9.e+]+)", output_text, perl = TRUE)
  if (obj_match > 0) {
    obj_str <- regmatches(output_text, obj_match)
    result$objective <- as.numeric(sub(".*Objective value:\\s+([-.0-9e+]+).*", "\\1", obj_str))
  }
  
  # Parse termination status
  status_match <- regexpr("Termination status:\\s+(\\w+)", output_text, perl = TRUE)
  if (status_match > 0) {
    status_str <- regmatches(output_text, status_match)
    result$status <- sub(".*Termination status:\\s+(\\w+).*", "\\1", status_str)
  }
  
  # Parse solve time
  time_match <- regexpr("(HiGHS|Gurobi|CPLEX) run time\\s*:\\s*([-0-9.]+)", output_text, perl = TRUE)
  if (time_match > 0) {
    time_str <- regmatches(output_text, time_match)
    result$solve_time <- as.numeric(sub(".* run time\\s*:\\s*([-.0-9]+).*", "\\1", time_str))
  }
  
  result
}


#' Load JuMP solution values into model
#' @noRd
load_jump_solution <- function(model, model_dir, data_ext) {
  
  # Solution directory is in solvers/jump/solution
  solution_dir <- file.path(model_dir, "solvers", "jump", "solution")
  
  if (!dir.exists(solution_dir)) {
    warning("Solution directory not found: ", solution_dir)
    return(model)
  }
  
  message("Loading solutions from: solvers/jump/solution")
  
  # Destination directory for moved solutions
  dest_dir <- file.path(model_dir, "variables")
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # Count loaded variables for log
  variables_loaded <- character(0)
  
  # Load solution for each variable
  for (var_name in names(model$variables)) {
    var_file <- file.path(solution_dir, var_name, paste0("data", data_ext))
    
    if (file.exists(var_file)) {
      solution_data <- if (data_ext == ".csv") {
        read.csv(var_file, stringsAsFactors = FALSE)
      } else {
        arrow::read_feather(var_file)
      }
      
      # Store in model
      model$variables[[var_name]]$solution <- solution_data
      variables_loaded <- c(variables_loaded, var_name)
      
      # Move solution file to model root variables directory
      dest_var_dir <- file.path(dest_dir, var_name)
      if (!dir.exists(dest_var_dir)) {
        dir.create(dest_var_dir, recursive = TRUE)
      }
      dest_file <- file.path(dest_var_dir, paste0("data", data_ext))
      file.copy(var_file, dest_file, overwrite = TRUE)
    }
  }
  
  # Log the solution loading
  log_solution_load(model_dir, "jump", variables_loaded, verbose = verbose)
  
  # Store solution metadata
  model$solution_metadata <- list(
    loaded_at = Sys.time(),
    variables_loaded = variables_loaded,
    format = data_ext,
    source = "solvers/jump/solution",
    destination = "variables/"
  )
  
  model
}


#' Write JuMP solution values to disk
#'
#' Helper function to save solution values from Julia model.
#' Called from within Julia code after solving.
#'
#' @param model JuMP model object (from Julia)
#' @param output_dir Directory to save solutions
#' @param format Data format ("arrow" or "csv")
#' @param run_name Name for this solution run (default: timestamp)
#'
#' @details
#' This is typically called from within the generated model.jl file.
#' It saves variable values in the same directory structure as input data.
#'
#' @examples
#' \dontrun{
#' # In Julia (model.jl):
#' # After solve...
#' using Dates
#' run_name = Dates.format(now(), "yyyy-mm-dd_HHMMSS")
#' save_jump_solution(model, "../../solutions/" * run_name, DATA_FORMAT)
#' }
#'
#' @export
write_jump_solution_code <- function() {
  # This returns Julia code to be included in model.jl
  julia_code <- '
# Function to save solution values
function save_solution(model, output_dir, data_format)
    using Dates
    using DataFrames
    using CSV
    using Arrow
    
    if !isdir(output_dir)
        mkpath(output_dir)
    end
    
    println("\\nSaving solution to: ", output_dir)
    
    # Determine file extension
    ext = data_format == "csv" ? ".csv" : ".arrow"
    
    # Save each variable
    for (var_name, var_obj) in object_dict(model)
        if !(var_obj isa AbstractArray{VariableRef})
            continue
        end
        
        # Get variable values
        var_vals = value.(var_obj)
        
        # Convert to DataFrame based on dimensions
        if var_obj isa DenseAxisArray
            # Get indices
            indices = axes(var_obj)
            n_dims = length(indices)
            
            if n_dims == 0
                # Scalar variable
                df = DataFrame(value = [var_vals])
            else
                # Multi-dimensional variable
                idx_names = [Symbol("dim$i") for i in 1:n_dims]
                rows = []
                
                for idx in Iterators.product(indices...)
                    val = var_vals[idx...]
                    if abs(val) > 1e-10  # Only save non-zero values
                        push!(rows, (idx..., val))
                    end
                end
                
                if !isempty(rows)
                    df = DataFrame(rows, vcat(idx_names, :value))
                else
                    df = DataFrame()
                end
            end
        else
            # Default handling
            df = DataFrame(value = [var_vals])
        end
        
        # Save to file
        if !isempty(df)
            var_dir = joinpath(output_dir, string(var_name))
            mkpath(var_dir)
            out_file = joinpath(var_dir, "data" * ext)
            
            if ext == ".csv"
                CSV.write(out_file, df)
            else
                Arrow.write(out_file, df)
            end
            
            println("  Saved ", string(var_name), ": ", nrow(df), " values")
        end
    end
    
    println("Solution saved successfully\\n")
end

# Save solution after solving
if termination_status(model) == MOI.OPTIMAL || 
   termination_status(model) == MOI.LOCALLY_SOLVED
    
    run_name = Dates.format(now(), "yyyy-mm-dd_HHMMSS")
    solution_dir = joinpath(@__DIR__, "..", "..", "solutions", run_name)
    save_solution(model, solution_dir, DATA_FORMAT)
end
'
  julia_code
}
