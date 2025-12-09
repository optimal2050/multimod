# Configuration management for multimod solver paths

These functions manage solver executable paths for multimod.
Configuration can be set via R options, environment variables, or a YAML
config file.

**Priority order** (highest to lowest):

1.  R option: `getOption("multimod.<solver>_path")`

2.  Environment variable: `MULTIMOD_<SOLVER>`

3.  YAML config file: `~/.multimod/config.yml` or `.multimod.yml`

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

  Logical; if `TRUE`, writes to `~/.multimod/config.yml` (user-wide). If
  `FALSE` (default), writes to `.multimod.yml` in current working
  directory.

## Examples

``` r
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

# Or use YAML config file (~/.multimod/config.yml):
multimod_config_write(list(
  python = list(path = "C:/Python310/python.exe", env_name = "multimod"),
  julia = list(path = "C:/Julia-1.10/bin/julia.exe"),
  glpsol = list(path = "C:/glpk/bin/glpsol.exe")
))

# View current configuration
multimod_config_show()
} # }
```
