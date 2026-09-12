# Configuring Solver Paths in multimod

``` r
library(multimod)
```

## Overview

The `multimod` package interfaces with multiple external solvers and
programming languages:

- **Python** (for Pyomo models)
- **Julia** (for JuMP models)
- **GLPK/glpsol** (for GMPL models)

This vignette explains how to configure paths to these executables so
`multimod` can find and use them correctly.

## Configuration Priority

`multimod` searches for solver executables in the following order
(highest to lowest priority):

1.  **R options** - Session-level settings via
    [`options()`](https://rdrr.io/r/base/options.html) or setter
    functions
2.  **Environment variables** - System-wide settings like
    `MULTIMOD_PYTHON`
3.  **YAML config file** - Project or user-level configuration files
4.  **Intelligent defaults** - Automatic detection (reticulate for
    Python, system PATH for others)

## Method 1: R Options (Recommended for Interactive Use)

### Quick Setup

Use the setter functions for easy configuration:

``` r
# Configure Python
set_multimod_python("C:/Python310/python.exe")

# Configure Julia
set_multimod_julia("C:/Program Files/Julia-1.10.0/bin/julia.exe")

# Configure glpsol
set_multimod_glpsol("C:/glpk-5.0/bin/glpsol.exe")
```

### Get Current Configuration

``` r
# Get individual solver paths
get_multimod_python()
get_multimod_julia()
get_multimod_glpsol()

# Or view all settings at once
multimod_config_show()
```

### Platform-Specific Examples

**Windows:**

``` r
set_multimod_python("C:/Users/username/.conda/envs/multimod/python.exe")
set_multimod_julia("C:/Program Files/Julia-1.10.0/bin/julia.exe")
set_multimod_glpsol("C:/Program Files/GnuWin32/bin/glpsol.exe")
```

**macOS:**

``` r
set_multimod_python("/Users/username/miniconda3/envs/multimod/bin/python")
set_multimod_julia("/Applications/Julia-1.10.app/Contents/Resources/julia/bin/julia")
set_multimod_glpsol("/usr/local/bin/glpsol")
```

**Linux:**

``` r
set_multimod_python("/home/username/miniconda3/envs/multimod/bin/python")
set_multimod_julia("/usr/local/bin/julia")
set_multimod_glpsol("/usr/bin/glpsol")
```

## Method 2: .Rprofile (Persistent Across Sessions)

For settings that persist across R sessions, add configuration to your
`.Rprofile`:

``` r
# Edit your .Rprofile
usethis::edit_r_profile()
```

Add these lines:

``` r
# multimod solver configuration
options(
  multimod.python_path = "C:/Users/me/.conda/envs/multimod/python.exe",
  multimod.julia_path = "C:/Julia-1.10/bin/julia.exe",
  multimod.glpsol_path = "C:/glpk/bin/glpsol.exe"
)
```

Restart R for changes to take effect.

### Project-Specific .Rprofile

Create a `.Rprofile` in your project directory for project-specific
settings:

``` r
# In your project's .Rprofile
options(
  multimod.python_path = "venv/Scripts/python.exe",  # Relative to project
  multimod.julia_path = "C:/Julia-1.10/bin/julia.exe"
)
```

## Method 3: Environment Variables (System-Wide)

Environment variables work across all R sessions and even other tools:

### Windows (PowerShell)

``` powershell
# Temporary (current session)
$env:MULTIMOD_PYTHON = "C:\Python310\python.exe"
$env:MULTIMOD_JULIA = "C:\Julia-1.10\bin\julia.exe"
$env:MULTIMOD_GLPSOL = "C:\glpk\bin\glpsol.exe"

# Permanent (requires admin - use System Properties > Environment Variables)
[System.Environment]::SetEnvironmentVariable("MULTIMOD_PYTHON", "C:\Python310\python.exe", "User")
```

### macOS/Linux (bash/zsh)

Add to `~/.bashrc` or `~/.zshrc`:

``` bash
export MULTIMOD_PYTHON="/usr/local/bin/python3"
export MULTIMOD_JULIA="/usr/local/bin/julia"
export MULTIMOD_GLPSOL="/usr/bin/glpsol"
```

Then reload: `source ~/.bashrc`

### Check Environment Variables in R

``` r
Sys.getenv("MULTIMOD_PYTHON")
Sys.getenv("MULTIMOD_JULIA")
Sys.getenv("MULTIMOD_GLPSOL")
```

## Method 4: YAML Configuration File

For teams or reproducible workflows, use a YAML config file:

### Create Config File

``` r
# User-level config (affects all projects)
multimod_config_write(
  config = list(
    python = list(
      path = "C:/Python310/python.exe",
      env_name = "multimod"
    ),
    julia = list(
      path = "C:/Julia-1.10/bin/julia.exe"
    ),
    glpsol = list(
      path = "C:/glpk/bin/glpsol.exe"
    )
  ),
  global = TRUE  # Writes to ~/.multimod/config.yml
)

# Project-level config (only for current project)
multimod_config_write(
  config = list(
    python = list(path = "./venv/Scripts/python.exe"),
    julia = list(path = "C:/Julia-1.10/bin/julia.exe")
  ),
  global = FALSE  # Writes to .multimod.yml in current directory
)
```

### Read Config File

``` r
# Read configuration
config <- multimod_config_read()
print(config)

# Find config file location
multimod_config_path()
multimod_config_path(global = TRUE)
```

### Config File Format

The YAML file structure is:

``` yaml
python:
  path: "C:/Users/me/.conda/envs/multimod/python.exe"
  env_name: "multimod"
julia:
  path: "C:/Julia-1.10/bin/julia.exe"
glpsol:
  path: "C:/glpk/bin/glpsol.exe"
highs:
  path: "C:/highs/bin/highs.exe"
```

You can edit this file manually or use
[`multimod_config_write()`](https://optimal2050.github.io/multimod/reference/multimod-config.md).

### Version Control

Add project-level config to `.gitignore` if paths are user-specific:

    .multimod.yml

Or commit it if all team members use the same paths (e.g., Docker
environments).

## Setting Up Python for Pyomo

### Option 1: Automatic Setup (Recommended)

Let `multimod` handle everything:

``` r
# Creates conda environment "multimod" with Pyomo and HiGHS solver
setup_python_environment()
```

This will: 1. Install Miniconda if needed 2. Create a conda environment
named “multimod” 3. Install Pyomo and highspy (solver) 4. Activate the
environment

### Option 2: Manual Setup

If you have an existing Python environment:

``` r
# Point to your Python executable
set_multimod_python("/path/to/your/python")

# Or configure via environment
setup_python_environment(
  env_name = "my_pyomo_env",
  packages = c("pyomo", "highspy"),
  reinstall = FALSE
)
```

### Verify Python Configuration

``` r
# Check what Python will be used
get_multimod_python()

# Test Pyomo installation
python_exec <- get_multimod_python()
system2(python_exec, args = c("-c", "import pyomo.environ; print('Pyomo OK')"))
```

## Setting Up Julia for JuMP

### Installation

1.  Download Julia from <https://julialang.org/downloads/>
2.  Install required packages:

``` julia
using Pkg
Pkg.add("JuMP")
Pkg.add("HiGHS")
```

### Configure Path

``` r
# Windows
set_multimod_julia("C:/Program Files/Julia-1.10.0/bin/julia.exe")

# macOS
set_multimod_julia("/Applications/Julia-1.10.app/Contents/Resources/julia/bin/julia")

# Linux
set_multimod_julia("/usr/local/bin/julia")
```

### Verify Julia Configuration

``` r
# Check configured path
get_multimod_julia()

# Test Julia installation
julia_exec <- get_multimod_julia()
system2(julia_exec, args = c("-e", "println(\"Julia OK\")"))
```

## Setting Up GLPK/glpsol

### Installation

**Windows:** - Download from <https://winglpk.sourceforge.net/> - Or use
MSYS2: `pacman -S mingw-w64-x86_64-glpk`

**macOS:**

``` bash
brew install glpk
```

**Linux (Ubuntu/Debian):**

``` bash
sudo apt-get install glpk-utils
```

### Configure Path

``` r
# Windows
set_multimod_glpsol("C:/Program Files/GnuWin32/bin/glpsol.exe")

# macOS (Homebrew)
set_multimod_glpsol("/usr/local/bin/glpsol")

# Linux
set_multimod_glpsol("/usr/bin/glpsol")
```

### Verify glpsol Configuration

``` r
# Check configured path
get_multimod_glpsol()

# Test glpsol installation
glpsol_exec <- get_multimod_glpsol()
system2(glpsol_exec, args = "--version")
```

## Troubleshooting

### Check All Configuration

``` r
multimod_config_show()
```

This displays: - Current effective paths for all solvers - R options
values - Environment variable values - YAML config file location

### Python Not Found

``` r
# Check search order
get_multimod_python()  # Shows what's being used

# Set explicitly
set_multimod_python("C:/Python310/python.exe")

# Or use setup helper
setup_python_environment(env_name = "multimod")
```

### Julia Not Found

``` r
# Check if Julia is in PATH
Sys.which("julia")

# Set explicitly
set_multimod_julia("C:/Julia-1.10/bin/julia.exe")
```

### glpsol Not Found

``` r
# Check if glpsol is in PATH
Sys.which("glpsol")

# Set explicitly
set_multimod_glpsol("C:/glpk/bin/glpsol.exe")
```

### Clear Configuration

``` r
# Clear R options
set_multimod_python(NULL)
set_multimod_julia(NULL)
set_multimod_glpsol(NULL)

# Remove config file
file.remove(multimod_config_path())
```

## Docker/Container Environments

For reproducible environments, use a Dockerfile:

``` dockerfile
FROM rocker/r-ver:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    python3-pip \
    julia \
    glpk-utils

# Install R packages
RUN R -e "install.packages('multimod')"

# Install Python packages
RUN pip3 install pyomo highspy

# Set environment variables
ENV MULTIMOD_PYTHON=/usr/bin/python3
ENV MULTIMOD_JULIA=/usr/bin/julia
ENV MULTIMOD_GLPSOL=/usr/bin/glpsol
```

## Best Practices

### 1. Use .Rprofile for Personal Settings

Store your personal solver paths in `~/.Rprofile`:

``` r
options(
  multimod.python_path = "/Users/me/miniconda3/envs/multimod/bin/python"
)
```

### 2. Use Project Config for Team Settings

For projects with standard setups (Docker, etc.), commit
`.multimod.yml`:

``` yaml
python:
  path: "/usr/bin/python3"
julia:
  path: "/usr/bin/julia"
```

### 3. Use Environment Variables for CI/CD

In GitHub Actions, GitLab CI, etc.:

``` yaml
env:
  MULTIMOD_PYTHON: /usr/bin/python3
  MULTIMOD_JULIA: /usr/bin/julia
  MULTIMOD_GLPSOL: /usr/bin/glpsol
```

### 4. Document Requirements

Include solver requirements in your project README:

``` markdown
## Requirements

- Python 3.8+ with Pyomo
- Julia 1.10+ with JuMP
- GLPK 5.0+

Configure paths using:
\`\`\`r
source("config.R")  # Sets up multimod solver paths
\`\`\`
```

## Example Workflow

Complete example of setting up and using multimod:

``` r
library(multimod)

# 1. Configure solvers (one-time setup)
setup_python_environment()  # Auto-setup Python/Pyomo
set_multimod_julia("C:/Julia-1.10/bin/julia.exe")
set_multimod_glpsol("C:/glpk/bin/glpsol.exe")

# 2. Verify configuration
multimod_config_show()

# 3. Create and export model
model <- create_simple_model()
save_model(model, "tmp/model", format = "ipc")

# 4. Write solver-specific files
write_pyomo(model, model_dir = "tmp/model")
write_jump(model, model_dir = "tmp/model")
write_gmpl(model, model_dir = "tmp/model")

# 5. Solve with different solvers
result_pyomo <- solve_pyomo(model, model_dir = "tmp/model")
result_jump <- solve_jump(model, model_dir = "tmp/model")

# Paths are automatically used - no need to specify explicitly!
```

## Summary

| Method | Scope | Persistence | Best For |
|----|----|----|----|
| R options (`set_multimod_*()`) | Session | Until R closes | Interactive work |
| `.Rprofile` | User/Project | Permanent | Personal defaults |
| Environment variables | System | Permanent | System-wide, CI/CD |
| YAML config | Project | Permanent | Team collaboration |

Choose the method that best fits your workflow! For most users, **R
options + .Rprofile** provide the best balance of convenience and
persistence.
