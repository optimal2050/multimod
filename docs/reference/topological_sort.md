# Topological sort of parameters by dependencies

Topological sort of parameters by dependencies

## Usage

``` r
topological_sort(needs_eval, verbose = FALSE)
```

## Value

A character vector of parameter names in evaluation order, dependencies
first. Errors on circular dependencies.
