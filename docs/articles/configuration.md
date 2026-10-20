# Configuring solver paths

``` r

library(multimod)
```

The matrix route
([`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md),
[`solve_highs()`](https://optimal2050.github.io/multimod/reference/solve_highs.md))
needs nothing beyond the ‘highs’ R package. The file routes run external
toolchains, and multimod has to find three executables:

| Route | Executable | Looked up by |
|----|----|----|
| MathProg, [`solve_gmpl()`](https://optimal2050.github.io/multimod/reference/solve_gmpl.md) | `glpsol` | [`get_multimod_glpsol()`](https://optimal2050.github.io/multimod/reference/multimod-config.md) |
| JuMP, [`solve_jump()`](https://optimal2050.github.io/multimod/reference/solve_jump.md) | `julia` | [`get_multimod_julia()`](https://optimal2050.github.io/multimod/reference/multimod-config.md) |
| Pyomo, [`solve_pyomo()`](https://optimal2050.github.io/multimod/reference/solve_pyomo.md) | `python` | [`get_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md) |

Each lookup checks, in this order, and stops at the first hit:

1.  an R option, set with
    [`set_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
    [`set_multimod_julia()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
    [`set_multimod_glpsol()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
    (or `options(multimod.python_path = ...)` and friends);
2.  an environment variable: `MULTIMOD_PYTHON`, `MULTIMOD_JULIA`,
    `MULTIMOD_GLPSOL`;
3.  a YAML configuration file, project-level `.multimod.yml` first, then
    the user-level file under `tools::R_user_dir("multimod", "config")`;
4.  automatic detection: for Python the conda environment named
    `multimod` through reticulate; for glpsol the ‘glpsol’ R package,
    then the conda environment, then the `PATH`; for Julia the `PATH`.

[`multimod_config_show()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
prints what each lookup currently resolves to and where the value comes
from.

``` r

multimod_config_show()
```

## Setting a path

For the current session:

``` r

set_multimod_python("C:/Users/me/.conda/envs/multimod/python.exe")
set_multimod_julia("C:/Julia-1.11/bin/julia.exe")
set_multimod_glpsol("C:/glpk/bin/glpsol.exe")

get_multimod_julia()
```

To make it permanent for one user, put the options in `~/.Rprofile`
([`usethis::edit_r_profile()`](https://usethis.r-lib.org/reference/edit.html)
opens it):

``` r

options(
  multimod.python_path = "C:/Users/me/.conda/envs/multimod/python.exe",
  multimod.julia_path  = "C:/Julia-1.11/bin/julia.exe",
  multimod.glpsol_path = "C:/glpk/bin/glpsol.exe"
)
```

Environment variables suit CI and containers. In a shell:

``` sh
export MULTIMOD_PYTHON=/usr/bin/python3
export MULTIMOD_JULIA=/usr/local/bin/julia
export MULTIMOD_GLPSOL=/usr/bin/glpsol
```

A YAML file suits a project shared by several people:

``` r

multimod_config_write(
  config = list(
    python = list(path = "C:/Users/me/.conda/envs/multimod/python.exe",
                  env_name = "multimod"),
    julia  = list(path = "C:/Julia-1.11/bin/julia.exe"),
    glpsol = list(path = "C:/glpk/bin/glpsol.exe")
  ),
  global = FALSE   # .multimod.yml in the working directory; TRUE = user level
)
multimod_config_path()        # where the project file is
multimod_config_read()
```

The file is plain YAML and can be edited by hand:

``` yaml
python:
  path: "C:/Users/me/.conda/envs/multimod/python.exe"
  env_name: "multimod"
julia:
  path: "C:/Julia-1.11/bin/julia.exe"
glpsol:
  path: "C:/glpk/bin/glpsol.exe"
```

Add `.multimod.yml` to `.gitignore` when the paths are user-specific.

To go back to automatic detection, clear the option:

``` r

set_multimod_python(NULL)
```

## GLPK

The simplest way to get `glpsol` is the ‘glpsol’ R package, which ships
the executable and is found automatically:

``` r

install.packages("glpsol")
get_multimod_glpsol()
```

A system GLPK works as well: `brew install glpk` on macOS,
`apt-get install glpk-utils` on Debian and Ubuntu, winglpk or MSYS2 on
Windows. The conda environment that
[`mm_setup_python()`](https://optimal2050.github.io/multimod/reference/mm_setup_python.md)
creates also holds a copy. Check what will be used:

``` r

system2(get_multimod_glpsol(), "--version")
```

## Julia

Install Julia from [julialang.org](https://julialang.org/downloads/) and
make sure the executable resolves:

``` r

get_multimod_julia()
system2(get_multimod_julia(), c("-e", "println(VERSION)"))
```

The generated `model.jl` needs the packages JuMP, HiGHS, Arrow,
DataFrames and CSV. It installs any that are missing on its first run,
which takes a few minutes; to do it beforehand:

``` julia
using Pkg
Pkg.add(["JuMP", "HiGHS", "Arrow", "DataFrames", "CSV"])
```

## Python

[`mm_setup_python()`](https://optimal2050.github.io/multimod/reference/mm_setup_python.md)
creates a conda environment named `multimod` with Python 3.14, installs
pyomo, highspy, pyarrow and pandas with pip and GLPK with conda, and
activates it through reticulate. Miniconda is installed first if there
is none.

``` r

mm_setup_python()
mm_check_python()
```

[`mm_check_python()`](https://optimal2050.github.io/multimod/reference/mm_check_python.md)
reports the interpreter that will be used, its version and the packages
that fail to import.
[`solve_pyomo()`](https://optimal2050.github.io/multimod/reference/solve_pyomo.md)
calls it before bootstrapping, so a usable environment is not
reinstalled on every solve.

To use an existing interpreter instead, point to it and check it carries
the packages a route needs:

``` r

set_multimod_python("/path/to/python")
mm_check_python(packages = c("pyomo", "highspy", "pyarrow"))
```

## When a tool is not found

- [`multimod_config_show()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  says which source each path comes from. A stale option or environment
  variable wins over everything below it.
- `Sys.which("julia")` and `Sys.which("glpsol")` show what the `PATH`
  resolves. On Windows the conda environment’s `bin` directory is not on
  the `PATH`; multimod looks inside it for glpsol, but a shell will not.
- `solve_pyomo(ensure_env = FALSE)` skips the environment check when the
  interpreter is managed elsewhere.
