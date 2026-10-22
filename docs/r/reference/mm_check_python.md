# Report on the Python environment multimod would use

Resolves the interpreter the way
[`get_multimod_python()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md)
does and reports whether it exists and carries the packages a route
needs.
[`solve_pyomo()`](https://optimal2050.github.io/multimod/r/reference/solve_pyomo.md)
calls this before bootstrapping, so an environment that is already
usable is not reinstalled on every solve.

## Usage

``` r
mm_check_python(env_name = "multimod", packages = "pyomo", python = NULL)
```

## Arguments

- env_name:

  Conda environment to look for. Defaults to `"multimod"`.

- packages:

  Python packages that must import. Defaults to `"pyomo"`.

- python:

  Optional explicit interpreter, bypassing resolution.

## Value

A list with `ok`, `path`, `version` and `missing`.

## See also

[`mm_setup_python()`](https://optimal2050.github.io/multimod/r/reference/mm_setup_python.md),
[`set_multimod_python()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md)
