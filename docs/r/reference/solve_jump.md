# Solve a JuMP Model

Execute a Julia/JuMP optimization model with configurable backend
options. Results can be optionally loaded back into the model object.

## Usage

``` r
solve_jump(
  model = NULL,
  model_dir,
  method = c("system", "juliacall", "background"),
  verbose = TRUE,
  load_results = !is.null(model),
  julia_args = NULL,
  timeout = NULL
)
```

## Arguments

- model:

  A multimod model object (optional, used for loading results back)

- model_dir:

  Path to the model directory containing solvers/jump/

- method:

  Execution method: "system" (default), "juliacall", or "background"

- verbose:

  Print Julia output to console

- load_results:

  Load solution values back into model\$variables

- julia_args:

  Additional arguments to pass to Julia (e.g., "–threads=4")

- timeout:

  Maximum execution time in seconds (NULL for no limit)

## Value

List with solution information:

- exit_code:

  Julia process exit code (0 = success)

- success:

  Logical indicating successful execution

- objective:

  Objective function value (if available)

- status:

  Solver termination status (if available)

- solve_time:

  Solution time in seconds (if available)

- output:

  Julia stdout/stderr (if verbose=TRUE)

- constraints:

  Constraint statistics data.frame

- variables:

  Variable statistics data.frame

## Details

The function supports three execution methods:

**system** (default): Uses system2() to call Julia. No extra
dependencies; works wherever Julia is installed.

**juliacall**: Uses JuliaCall package for R-Julia integration. Allows
bi-directional data transfer and access to Julia objects from R.
Requires JuliaCall package.

**background**: Uses callr package for non-blocking execution. Allows
running multiple models in parallel. Requires callr package.

Solution values are saved by the Julia model to solvers/jump/solution/
(in Arrow IPC or CSV format). If load_results=TRUE and model is
provided, solutions are loaded into result\$model and moved to
model_dir/variables/. A log file (solution_log.txt) in model_dir tracks
all solution loads. The solutions are available in the returned model,
which replaces the one passed in:

`result <- solve_jump(my_model, "path/to/model", load_results = TRUE)`

`my_model <- result$model # Update with loaded solutions`

## Examples

``` r
if (FALSE) { # \dontrun{
# needs Julia with JuMP and HiGHS
toy <- example_models$toy
ws <- tempfile("toy-")
write_jump(toy, model_dir = ws)

# Solve with default settings
result <- solve_jump(model_dir = ws)

# Solve and load the results back into the model
result <- solve_jump(toy, model_dir = ws, load_results = TRUE)
toy <- result$model
toy$variables$vCap$solution

# Use a different backend
result <- solve_jump(model_dir = ws, method = "juliacall")

# Background execution with a timeout
result <- solve_jump(model_dir = ws, method = "background", timeout = 3600)
} # }
```
