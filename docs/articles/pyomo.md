# multimod to Pyomo

## Introduction

This vignette demonstrates the complete workflow for converting an
energyRt model to Pyomo (Python Optimization Modeling Objects) format
and solving it with HiGHS, Gurobi, CPLEX, or other solvers. The workflow
includes:

1.  Reading a GAMS model structure
2.  Converting to multimod format
3.  Connecting to energyRt scenario data
4.  Writing Pyomo model files
5.  Solving with Python/Pyomo

We’ll use the UTOPIA base scenario from energyRt as our example.

## Pyomo-Specific Features

### Key Functions

#### 1. `write_pyomo(model, model_dir, cleanup, data_mode, split)`

Generates a complete Python/Pyomo project with model code, data loading,
and optional solver execution.

**Parameters:**

- `model_dir`: Output directory for the project
- `solver = "highs"`: Default solver to use
- `cleanup = FALSE`: Whether to remove existing directory first
- `data_mode = "external"`: Data storage method (see below)
- `split = FALSE`: Whether to create separate `solve.py` (vs embedded in
  `model.py`)
- `export_vars = FALSE`: Export solution values to JSON
- `export_data = FALSE`: Export loaded data to CSV for debugging
- `export_mps = FALSE`: Export model to MPS format

**Generated Files:**

    model_dir/
    ├── format.txt                  # Data format specification
    ├── model.rds                   # Saved R model object
    ├── sets/                       # Set members
    ├── mappings/                   # Set membership data
    ├── parameters/                 # Numeric parameter data
    └── solvers/
        └── pyomo/
            ├── model.py            # Main model file (AbstractModel)
            ├── data.py             # Data loading code
            ├── solve.py            # Solver runner (if split=TRUE)
            ├── model.mps           # MPS export (if export_mps=TRUE)
            └── solution/
                ├── status.json     # Solver status and termination
                └── variables.json  # Solution values (if export_vars=TRUE)

**Key Features:**

- **Abstract Model**: Separates model structure from data
- **Flexible data loading**: External files or embedded literals
- **Multiple solvers**: HiGHS, Gurobi, CPLEX, GLPK, CBC, etc.
- **Type-safe parameters**: Domain specifications (Reals, Any)
- **Clean constraint syntax**: Named constraints with proper indexing
- **Solution export**: JSON format for easy post-processing

### Data Storage Options

The `data_mode` parameter controls how data is provided to Pyomo:

#### **`data_mode = "external"` (Recommended)**

Data loaded from saved files at runtime:

``` python
# In data.py
def build_dataportal(model, base_dir):
    data = DataPortal()
    # Load from files in base_dir/sets/, parameters/, etc.
    data.load(filename=str(path), set=getattr(model, name))
    return data
```

**Pros:** - Flexible: Change data without regenerating model code -
Memory efficient: Data loaded only when needed - Debugging friendly:
Inspect data files directly - Supports large models: Stream from disk

**Cons:** - Requires file system access - Slightly slower startup

#### **`data_mode = "embedded"` (Not Yet Implemented)**

Data embedded as Python literals in the code:

``` python
# Would be in model.py
pDiscountRate = {
    'R1': 0.05,
    'R2': 0.03
}
```

**When implemented, will provide:** - Standalone files: No external
dependencies - Faster startup: No file I/O - Portable: Single file
distribution

**Trade-offs:** - Large files for big models - Must regenerate for data
changes - Not human-friendly for debugging

### Model Structure: Abstract vs Concrete

Pyomo supports two model types. `multimod` uses **AbstractModel**:

``` python
# AbstractModel (what we generate)
model = AbstractModel()
model.REGION = Set()
model.YEAR = Set()
model.pDiscountRate = Param(model.REGION, domain=Reals, default=0.05)

# Data loaded separately via DataPortal
instance = model.create_instance(data)
```

**Why AbstractModel?**

1.  **Separation of concerns**: Model structure independent of data
2.  **Reusability**: Same model code with different datasets
3.  **Debugging**: Inspect model structure before instantiation
4.  **energyRt compatibility**: Matches scenario-based workflow
5.  **Testing**: Easy to validate with small test datasets

ConcreteModel (not used) embeds data directly:

``` python
# ConcreteModel (not used)
model = ConcreteModel()
model.REGION = Set(initialize=['R1', 'R2'])  # Data embedded
```

### Modern Pyomo Code Generation

The generated Pyomo code uses best practices:

``` python
# Sets
model.REGION = Set(doc="Regions")
model.TECHNOLOGY = Set(doc="Technologies")

# Mappings (sets of tuples)
model.mTechSpan = Set(dimen=2, doc="(technology, year)")

# Parameters with defaults and domains
model.pDiscountRate = Param(model.REGION, default=0.05, 
                             domain=Reals, doc="Discount rate")
model.pTechStock = Param(model.TECHNOLOGY, model.YEAR, 
                         default=0, domain=Reals)

# Variables with bounds
model.vTechCap = Var(model.mTechSpan, within=NonNegativeReals)
model.vObjective = Var(within=Reals)  # Free variable
model.vBinaryChoice = Var(model.TECHNOLOGY, within=Binary)

# Constraints with rule functions
def eqTechCap_rule(model, h, y):
    if (h, y) not in model.mTechSpan:
        return Constraint.Skip
    return (
        model.vTechCap[h, y] == 
        model.pTechStock[h, y] +
        sum(model.vTechNewCap[h, yy] 
            for yy in model.YEAR 
            if (h, yy, y) in model.mTechNew)
    )

model.eqTechCap = Constraint(
    model.TECHNOLOGY, model.YEAR,
    rule=eqTechCap_rule
)

# Objective
model.objective = Objective(
    rule=lambda model: model.vObjective,
    sense=minimize
)
```

### Solver Integration

The generated code supports multiple solvers via environment variable:

``` python
# In model.py or solve.py
solver_name = os.environ.get('PYOMO_SOLVER', 'highs')
solver = SolverFactory(solver_name)
results = solver.solve(instance, tee=True)
```

**From R:**

``` r
solve_pyomo(model_dir = "tmp/model", solver = "highs")
```

**From command line:**

``` bash
export PYOMO_SOLVER=gurobi
python model.py
```

**From Python:**

``` python
os.environ['PYOMO_SOLVER'] = 'cplex'
```

## Prerequisites

``` r
library(multimod)
library(energyRt)  # For scenario data
```

**Python Requirements:**

- Python 3.8+
- Pyomo package
- Solver (HiGHS, Gurobi, CPLEX, GLPK, etc.)
- pandas (for data loading)
- pyarrow (for Arrow/IPC format) or CSV support

**Easy Setup:**

``` r
# Automatic setup with conda environment
setup_python_environment(
  env_name = "multimod",
  packages = c("pyomo", "highspy")  # Includes HiGHS solver
)
```

**Manual Python setup:**

``` bash
# Create conda environment
conda create -n multimod python=3.11
conda activate multimod

# Install Pyomo and solver
pip install pyomo
pip install highspy  # For HiGHS
# Or: conda install -c conda-forge gurobi
```

**Verify installation:**

``` r
python_exec <- get_multimod_python()
system2(python_exec, args = c("-c", "import pyomo.environ; print('Pyomo OK')"))
```

## Step 1: Read GAMS Model Structure

``` r
# Path to GAMS model
gams_file <- "C:/Users/admin/Documents/R/multimod/dev/scenarios/BASE_UTOPIA/script/gams_cbc/energyRt.gms"

# Read model structure
cat("Reading GAMS model...\n")
model_struct <- read_gams(gams_file)

# Inspect structure
cat("Model structure:\n")
cat("  Sets:", length(model_struct$sets), "\n")
cat("  Mappings:", length(model_struct$mappings), "\n")
cat("  Parameters:", length(model_struct$parameters), "\n")
cat("  Variables:", length(model_struct$variables), "\n")
cat("  Equations:", length(model_struct$equations), "\n")
```

## Step 2: Convert to Multimod Format

``` r
cat("\nConverting to multimod...\n")
model <- as_multimod(model_struct)

# Extract domain information from GAMS comments
model <- en_extract_domains_from_comments(model, verbose = TRUE)

# Populate default values from energyRt conventions
model <- populate_defvals_from_energyrt(model)

cat("\nModel ready for Pyomo export\n")
```

## Step 3: Load and Import Scenario Data

``` r
# Load energyRt scenario
cat("\nLoading energyRt scenario...\n")
scen_file <- "C:/Users/admin/Documents/R/multimod/dev/scenarios/BASE_UTOPIA/scen.RData"
load(scen_file)

cat("Scenario class:", class(scen), "\n")
```

### Import with Comprehensive Logging

``` r
# Import all data (sets, parameters, mappings, bounds)
cat("\nImporting energyRt scenario data...\n")
model <- import_energyRt_data(
  model, 
  scen, 
  inMemory = TRUE,  # Load data into R memory
  log_file = "tmp/import_log.csv"
)

cat("\nImport complete!\n")

# Verify data population
n_params <- length(model$parameters)
n_with_data <- sum(sapply(model$parameters, function(p) {
  !is.null(p$data) && nrow(p$data) > 0
}))

cat("Parameters with data:", n_with_data, "/", n_params, "\n")
```

## Step 4: Save Model Data

Save the model data to disk for Pyomo to load:

``` r
# Create output directory
test_dir <- "tmp/utopia_pyomo"

# Save model data in Arrow IPC format (recommended)
cat("\nSaving model data (Arrow IPC format)...\n")
save_model(model, test_dir, format = "ipc", verbose = TRUE)

# Alternative: CSV format (slower, but human-readable)
# save_model(model, test_dir, format = "csv", verbose = TRUE)

cat("\nData saved to:", test_dir, "\n")
```

**Format Comparison:**

| Format      | Speed       | Size       | Python Support  | Human-readable |
|-------------|-------------|------------|-----------------|----------------|
| IPC (Arrow) | ⚡⚡⚡ Fast | 📦 Compact | pyarrow         | ❌ No          |
| CSV         | 🐌 Slow     | 📦📦 Large | pandas (native) | ✅ Yes         |

## Step 5: Generate Pyomo Code

``` r
# Generate complete Pyomo project
cat("\nGenerating Pyomo code...\n")
write_pyomo(
  model, 
  model_dir = test_dir,
  data_mode = "external",      # Load data from files
  split = FALSE,               # Embed solver in model.py
  export_vars = TRUE,          # Export solution to JSON
  export_data = FALSE,         # Skip data export (for debugging)
  export_mps = TRUE            # Export MPS for comparison
)

cat("✓ Pyomo code generated\n")

# Show generated files
pyomo_dir <- file.path(test_dir, "solvers", "pyomo")
cat("\nGenerated files:\n")
print(list.files(pyomo_dir, recursive = FALSE))
```

### Write Options Explained

``` r
# Minimal export (fastest)
write_pyomo(model, model_dir = test_dir)

# With MPS export for debugging
write_pyomo(model, model_dir = test_dir, export_mps = TRUE)

# With solution export
write_pyomo(model, model_dir = test_dir, export_vars = TRUE)

# With data export for verification
write_pyomo(model, model_dir = test_dir, export_data = TRUE)

# Separate solve.py file (3-file structure)
write_pyomo(model, model_dir = test_dir, split = TRUE)

# Future: Embedded data (when implemented)
# write_pyomo(model, model_dir = test_dir, data_mode = "embedded")
```

### Inspect Generated Code

``` r
# Show first 100 lines of model.py
model_file <- file.path(pyomo_dir, "model.py")
cat("\n=== model.py (first 100 lines) ===\n")
model_lines <- readLines(model_file, n = 100)
cat(paste(model_lines, collapse = "\n"), "\n")
```

Key features in model.py:

``` python
# AbstractModel declaration
model = AbstractModel()

# Set declarations
model.REGION = Set(doc="Regions")
model.TECHNOLOGY = Set(doc="Technologies")
model.mTechSpan = Set(dimen=2, doc="Technology-Year mapping")

# Parameter declarations with defaults
model.pDiscountRate = Param(model.REGION, default=0.05, 
                             domain=Reals, doc="Discount rate")

# Variable declarations with bounds
model.vTechCap = Var(model.mTechSpan, within=NonNegativeReals)

# Constraint definitions
def eqTechCap_rule(model, h, y):
    # Constraint logic here
    pass

model.eqTechCap = Constraint(model.TECHNOLOGY, model.YEAR,
                              rule=eqTechCap_rule)

# Objective
model.objective = Objective(
    rule=lambda model: model.vObjective,
    sense=minimize
)
```

### Inspect Data Loading Code

``` r
# Show data loading code
data_file <- file.path(pyomo_dir, "data.py")
cat("\n=== data.py (first 80 lines) ===\n")
data_lines <- readLines(data_file, n = 80)
cat(paste(data_lines, collapse = "\n"), "\n")
```

Key features in data.py:

``` python
DATA_MODE = 'external'

def _read_format(base_dir):
    # Auto-detect CSV vs Arrow IPC
    fmt_file = base_dir / 'format.txt'
    if fmt_file.exists():
        return fmt_file.read_text().strip().lower()
    return 'csv'

def build_dataportal(model, base_dir):
    data = DataPortal()
    
    # Load sets
    for name in set_names:
        path = base_dir / 'sets' / name / f'data{ext}'
        if path.exists():
            data.load(filename=str(path), set=getattr(model, name))
    
    # Load parameters with proper indexing
    for param_name, index_tuple in param_specs:
        path = base_dir / 'parameters' / param_name / f'data{ext}'
        if path.exists():
            data.load(filename=str(path), 
                     param=getattr(model, param_name),
                     index=index_tuple)
    
    return data
```

This handles: - ✅ Automatic format detection (CSV vs Arrow) - ✅ Folded
data preference (uses `folded_data/` if available) - ✅ Proper parameter
indexing for multi-dimensional data - ✅ Optional parameters (skips if
file doesn’t exist)

## Step 6: Solve with Pyomo

### Option A: Solve from R (Recommended)

``` r
# Solve using R interface
cat("\n=== SOLVING WITH PYOMO ===\n\n")

result <- solve_pyomo(
  model_dir = test_dir,
  solver = "highs",           # Or "gurobi", "cplex", "glpk"
  verbose = TRUE,             # Show solver output
  ensure_env = TRUE,          # Use/create Python environment
  env_name = "multimod"       # Conda environment name
)

# Check results
cat("\nSolver status:", result$status$status, "\n")
cat("Termination:", result$status$termination, "\n")
cat("Solve time:", result$elapsed_time, "seconds\n")
```

### Option B: Run from Terminal

``` bash
# Activate Python environment
conda activate multimod

# Run model
cd path/to/tmp/utopia_pyomo/solvers/pyomo
python model.py

# Or with different solver
export PYOMO_SOLVER=gurobi
python model.py
```

### Option C: Run from Python

``` python
# In Python
import os
os.chdir('path/to/tmp/utopia_pyomo/solvers/pyomo')

# Set solver (optional)
os.environ['PYOMO_SOLVER'] = 'highs'

# Run model
exec(open('model.py').read())
```

### Solver Options

``` r
# Use different solver
solve_pyomo(model_dir = test_dir, solver = "gurobi")

# With timeout
solve_pyomo(model_dir = test_dir, timeout = 3600)  # 1 hour

# Quiet mode
solve_pyomo(model_dir = test_dir, verbose = FALSE)

# Use custom Python
solve_pyomo(
  model_dir = test_dir,
  python = "C:/Python310/python.exe",
  ensure_env = FALSE
)
```

## Step 7: Analyze Results

### Read Solution Status

``` r
# Read solution status
status_file <- file.path(pyomo_dir, "solution", "status.json")
if (file.exists(status_file)) {
  cat("\n=== SOLUTION STATUS ===\n")
  status <- jsonlite::read_json(status_file)
  
  cat("Solver:", status$solver, "\n")
  cat("Status:", status$status, "\n")
  cat("Termination:", status$termination, "\n")
}
```

### Read Solution Values

``` r
# Read variable values (if export_vars=TRUE)
var_file <- file.path(pyomo_dir, "solution", "variables.json")
if (file.exists(var_file)) {
  cat("\n=== SOLUTION VALUES ===\n")
  vars <- jsonlite::read_json(var_file)
  
  # Show variable summary
  cat("Variables with non-zero values:\n")
  for (vname in names(vars)) {
    vals <- unlist(vars[[vname]])
    nonzero <- sum(abs(vals) > 1e-6)
    if (nonzero > 0) {
      cat(sprintf("  %s: %d non-zero values\n", vname, nonzero))
    }
  }
  
  # Example: Extract vTechCap values
  if ("vTechCap" %in% names(vars)) {
    tech_cap <- vars$vTechCap
    cat("\nvTechCap sample:\n")
    print(head(tech_cap, 10))
  }
}
```

### Compare with MPS File

If you exported MPS format, you can compare with other solvers:

``` r
mps_file <- file.path(pyomo_dir, "model.mps")
if (file.exists(mps_file)) {
  cat("\nMPS file created:", mps_file, "\n")
  
  # Get model statistics
  mps_lines <- readLines(mps_file, n = 50)
  cat("\nMPS header:\n")
  cat(paste(head(mps_lines, 20), collapse = "\n"), "\n")
  
  # File size
  file_size <- file.size(mps_file) / 1024 / 1024
  cat(sprintf("\nMPS file size: %.2f MB\n", file_size))
}
```

### Verify Data Export (if enabled)

``` r
# If export_data=TRUE, check exported data
data_export_dir <- file.path(pyomo_dir, "data_export")
if (dir.exists(data_export_dir)) {
  cat("\n=== EXPORTED DATA ===\n")
  
  # List exported files
  exported <- list.files(data_export_dir, pattern = "\\.csv$")
  cat("Exported", length(exported), "data files\n")
  
  # Check a parameter
  discount_file <- file.path(data_export_dir, "pDiscountRate.csv")
  if (file.exists(discount_file)) {
    cat("\npDiscountRate data:\n")
    discount <- read.csv(discount_file)
    print(head(discount))
  }
}
```

## Step 8: Load Results Back to R

``` r
# TODO: Implement result loading function
# result_model <- load_pyomo_solution(model, test_dir)

# For now, parse JSON manually
if (file.exists(var_file)) {
  cat("\n=== LOADING RESULTS TO R ===\n")
  
  solution_data <- jsonlite::read_json(var_file)
  
  # Convert to data frames
  for (var_name in names(solution_data)) {
    var_vals <- solution_data[[var_name]]
    
    if (length(var_vals) > 0) {
      # Parse indices from strings like "(tech, year)"
      # and create data frame with columns: index columns + value
      cat("Processing variable:", var_name, "\n")
      # ... parsing logic ...
    }
  }
}
```

## Troubleshooting

### Python/Pyomo Not Found

``` r
# Check Python configuration
get_multimod_python()
multimod_config_show()

# Set up environment
setup_python_environment()
```

### Solver Not Found

``` r
# Install solver in Python environment
python_exec <- get_multimod_python()
system2(python_exec, args = c("-m", "pip", "install", "highspy"))
```

### Model Generation Errors

``` r
# Check model structure
str(model$sets, max.level = 1)
str(model$parameters, max.level = 1)

# Verify data import
summary(model)
```

### Infeasible Model

``` r
# Export MPS and compare with JuMP/GMPL
write_pyomo(model, model_dir = test_dir, export_mps = TRUE)
write_jump(model, model_dir = test_dir, export_mps = TRUE)

# Compare constraint counts
# ...
```

## Comparison: Pyomo vs JuMP

| Feature        | Pyomo               | JuMP           |
|----------------|---------------------|----------------|
| Language       | Python              | Julia          |
| Model type     | Abstract            | Concrete       |
| Data loading   | DataPortal          | Dict/DataFrame |
| Syntax         | Rules + constraints | Direct         |
| Performance    | Good                | Excellent      |
| Solvers        | Many (via plugins)  | Many (via MOI) |
| Ecosystem      | SciPy, pandas       | DataFrames.jl  |
| Learning curve | Moderate            | Moderate       |

### When to Use Pyomo

- ✅ Python ecosystem integration (pandas, matplotlib, scikit-learn)
- ✅ Abstract model separation (structure vs data)
- ✅ Existing Python codebase
- ✅ Prototyping and teaching
- ✅ Complex data preprocessing

### When to Use JuMP

- ✅ Maximum performance
- ✅ Large-scale models
- ✅ Cleaner constraint syntax
- ✅ Better type safety
- ✅ Faster iteration during development

## Summary

Complete Pyomo workflow:

``` r
library(multimod)
library(energyRt)

# 1. Setup Python environment (one-time)
setup_python_environment()

# 2. Load and convert model
model_struct <- read_gams("path/to/model.gms")
model <- as_multimod(model_struct)
model <- en_extract_domains_from_comments(model)
model <- populate_defvals_from_energyrt(model)

# 3. Import scenario data
load("path/to/scenario.RData")
model <- import_energyRt_data(model, scen, inMemory = TRUE)

# 4. Save and export
save_model(model, "tmp/model", format = "ipc")
write_pyomo(model, model_dir = "tmp/model", 
            export_mps = TRUE, export_vars = TRUE)

# 5. Solve
result <- solve_pyomo(model_dir = "tmp/model", 
                      solver = "highs", verbose = TRUE)

# 6. Analyze results
print(result$status)
```

## Next Steps

- **Explore solvers**: Try Gurobi, CPLEX, SCIP, etc.
- **Optimize data**: Use Arrow format for large models
- **Compare results**: Export MPS and solve with multiple tools
- **Integrate with Python**: Use Pyomo’s advanced features directly
- **Production deployment**: Set up via YAML config and Docker

See also: -
[`vignette("configuration")`](https://optimal2050.github.io/multimod/articles/configuration.md) -
Configure solver paths -
[`vignette("jump")`](https://optimal2050.github.io/multimod/articles/jump.md) -
JuMP export workflow -
[`?write_pyomo`](https://optimal2050.github.io/multimod/reference/write_pyomo.md) -
Full documentation
