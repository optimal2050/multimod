# Configuration management for multimod solver paths

These functions manage solver executable paths for multimod.
Configuration can be set via R options, environment variables, or a YAML
config file.

**Priority order** (highest to lowest):

1.  R option: `getOption("multimod.<solver>_path")`

2.  Environment variable: `MULTIMOD_<SOLVER>`

3.  YAML config file: `.multimod.yml` in the working directory, then
    `config.yml` in the per-user directory
    `tools::R_user_dir("multimod", "config")`

4.  Defaults: the conda environment `env_name` for Python (looked up,
    not started), the system `PATH` for the others

`multimod_config_write()` writes only to the `file` it is given. To
write the user-level file that `multimod_config_read()` finds, pass
`file.path(tools::R_user_dir("multimod", "config"), "config.yml")`.

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

multimod_config_write(config, file)

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

- file:

  Path of the YAML file to write; required.

- global:

  Logical; for `multimod_config_read()` and `multimod_config_path()`:
  when `TRUE`, the per-user file under
  `tools::R_user_dir("multimod", "config")` comes first; when `FALSE`
  (default), `.multimod.yml` in the working directory.

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
# where the configuration would be read from
multimod_config_path()
#> [1] "C:/Users/admin/Documents/R/multimod/docs/r/reference/.multimod.yml (not found)"

# write a configuration file, here to a temporary location
if (requireNamespace("yaml", quietly = TRUE)) {
  f <- multimod_config_write(list(julia = list(path = "julia")),
                             file = tempfile(fileext = ".yml"))
  readLines(f)
}
#> Configuration written to: C:\Users\admin\AppData\Local\Temp\Rtmp6Z6Hhw\file11ea41b767828.yml
#> [1] "julia:"        "  path: julia"

# \donttest{
# what is resolved now; probes the machine for Python, Julia and glpsol
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
#>   Path:     C:/Users/admin/AppData/Local/R/win-library/4.6/glpsol/bin/x64/glpsol.exe 
#>   Option:   (not set) 
#>   Env var:  (not set) 
#> 
#> YAML config file: (none found)
# }

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

# Or a YAML file, here the user-level one multimod_config_read() finds:
multimod_config_write(list(
  python = list(path = "C:/Python310/python.exe", env_name = "multimod"),
  julia = list(path = "C:/Julia-1.10/bin/julia.exe"),
  glpsol = list(path = "C:/glpk/bin/glpsol.exe")
), file = file.path(tools::R_user_dir("multimod", "config"), "config.yml"))

# View current configuration
multimod_config_show()
} # }
```
