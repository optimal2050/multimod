# Configuration management for multimod solver paths

These functions manage solver executable paths for multimod.
Configuration can be set via R options, environment variables, or a YAML
config file.

**Priority order** (highest to lowest):

1.  R option: `getOption("multimod.<solver>_path")`

2.  Environment variable: `MULTIMOD_<SOLVER>`

3.  YAML config file: the per-user directory given by
    `tools::R_user_dir("multimod", "config")` or `.multimod.yml`

4.  Intelligent defaults (reticulate for Python, system PATH for others)

## Usage

``` r
set_multimod_python(path = NULL)

get_multimod_python(env_name = "multimod")

set_multimod_julia(path = NULL)

get_multimod_julia()

set_multimod_glpsol(path = NULL)

get_multimod_glpsol()

set_multimod_solver(solver_name, path = NULL)

get_multimod_solver(solver_name)

multimod_config_write(config, global = FALSE)

multimod_config_read(global = FALSE)

multimod_config_path(global = FALSE)

multimod_config_show()
```

## Arguments

- path:

  Character path to the solver executable. If `NULL`, clears the
  setting.

- env_name:

  For Python: name of conda/virtual environment to use. Defaults to
  `"multimod"`.

- solver_name:

  Name of the solver (e.g., "python", "julia", "glpsol", "highs")

- config:

  Named list of configuration values to write to YAML file.

- global:

  Logical; if `TRUE`, writes to the per-user directory given by
  `tools::R_user_dir("multimod", "config")` (user-wide). If `FALSE`
  (default), writes to `.multimod.yml` in current working directory.

## Value

- `get_multimod_python()`, `get_multimod_julia()`,
  `get_multimod_glpsol()` and `get_multimod_solver()`: the resolved
  path, or the bare executable name (`"python"`, `"julia"`, `"glpsol"`,
  or `solver_name`) to be found on the `PATH` when nothing is
  configured.

- `set_multimod_*()` and `set_multimod_solver()`: invisibly, the path
  set.

- `multimod_config_write()`: invisibly, the path of the file written.

- `multimod_config_read()`: the configuration as a list, or `NULL` when
  no file is found.

- `multimod_config_path()`: the path of the configuration file, marked
  "(not found)" when it does not exist.

- `multimod_config_show()`: called for its printed summary; returns
  `NULL` invisibly.

## Examples

``` r
# where the configuration would be read from, and what is resolved now
multimod_config_path()
#> [1] "C:/Users/admin/Documents/R/multimod/docs/r/reference/.multimod.yml (not found)"
multimod_config_show()
#> Current multimod configuration:
#> 
#> Python:
#>   Path:     C:\Users\admin\.conda\envs\multimod\python.exe 
#>   Option:   (not set) 
#>   Env var:  (not set) 
#> 
#> Julia:
#>   Path:     julia 
#>   Option:   (not set) 
#>   Env var:  (not set) 
#> 
#> glpsol:
#>   Path:     C:/Users/admin/AppData/Local/R/win-library/4.5/glpsol/bin/x64/glpsol.exe 
#>   Option:   (not set) 
#>   Env var:  (not set) 
#> 
#> YAML config file: (none found)

if (FALSE) { # \dontrun{
# Set paths via R options (session-persistent)
set_multimod_python("C:/Python310/python.exe")
set_multimod_julia("C:/Julia-1.10/bin/julia.exe")
set_multimod_glpsol("C:/glpk/bin/glpsol.exe")

# Get configured paths
get_multimod_python()
get_multimod_julia()
get_multimod_glpsol()

# Set in .Rprofile for persistence across sessions:
options(
  multimod.python_path = "C:/Users/me/.conda/envs/multimod/python.exe",
  multimod.julia_path = "C:/Julia-1.10/bin/julia.exe"
)

# Or use environment variables (system-wide):
Sys.setenv(MULTIMOD_PYTHON = "C:/Python310/python.exe")

# Or use YAML config file (the config.yml under tools::R_user_dir("multimod", "config")):
multimod_config_write(list(
  python = list(path = "C:/Python310/python.exe", env_name = "multimod"),
  julia = list(path = "C:/Julia-1.10/bin/julia.exe"),
  glpsol = list(path = "C:/glpk/bin/glpsol.exe")
))

# View current configuration
multimod_config_show()
} # }
```
