# Model workspace and data

``` r

library(multimod)
```

A model’s data can be large. multimod keeps the model structure and the
data apart on disk:
[`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md)
writes a *workspace*, a directory with the R object for the structure
and one data file per set, mapping and parameter, in CSV, Arrow IPC or
Parquet.
[`load_model()`](https://optimal2050.github.io/multimod/reference/load_model.md)
reads it back, with or without the data, and
[`get_data()`](https://optimal2050.github.io/multimod/reference/get_data.md)
fetches a table from disk on demand. The generated JuMP and Pyomo code
reads the same files at run time, which is why
[`write_jump()`](https://optimal2050.github.io/multimod/reference/write_jump.md)
and
[`write_pyomo()`](https://optimal2050.github.io/multimod/reference/write_pyomo.md)
create a workspace when they are given an empty `model_dir`.

## Layout

    <model_dir>/
      model.rds              the model structure, as saved
      metadata.json          counts and format, for readers outside R
      format.txt             "csv", "ipc" or "parquet"
      sets/<name>/data.*     one file per set, column `member`
      mappings/<name>/data.* one file per mapping, one column per dimension
      parameters/<name>/data.*          dimension columns plus `value`
      parameters/<name>/folded_data/data.*   if the parameter is folded
      variables/<name>/data.*           solution values, after a solve
      solution_log.txt       one line per solution load
      solvers/<backend>/     generated code and the backend's output

## Save and load

``` r

toy <- example_models$toy
ws <- tempfile("toy-")
toy <- save_model(toy, ws, format = "csv", verbose = FALSE)
list.files(ws, recursive = TRUE)[1:8]
#> [1] "format.txt"                 "mappings/mappings.csv"     
#> [3] "mappings/mR/data.csv"       "mappings/mTR/data.csv"     
#> [5] "metadata.json"              "model.rds"                 
#> [7] "parameters/parameters.csv"  "parameters/pAvail/data.csv"
toy$base_path == ws
#> [1] TRUE
```

[`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md)
returns the model with `base_path` set to the workspace. With
`format = "ipc"` or `"parquet"` the data files are Arrow files, which
are typically smaller than CSV and faster to read; both need the ‘arrow’
package, and the generated Julia and Python read them as readily as CSV.

``` r

ws_ipc <- tempfile("toy-ipc-")
save_model(toy, ws_ipc, format = "ipc", verbose = FALSE)
list.files(file.path(ws_ipc, "parameters"), recursive = TRUE)
#> [1] "parameters.csv"      "pAvail/data.arrow"   "pCapCost/data.arrow"
#> [4] "pDemand/data.arrow"  "pVarCost/data.arrow"
```

[`load_model()`](https://optimal2050.github.io/multimod/reference/load_model.md)
reads the structure and, by default, every data table:

``` r

m <- load_model(ws, verbose = FALSE)
head(m$parameters$pAvail$data)
#>   tech region value
#> 1 coal  north  0.85
#> 2  gas  north  0.85
#> 3 wind  north  0.90
#> 4 coal  south  0.90
#> 5  gas  south  0.35
#> 6 wind  south  0.25
```

## Data on demand

A large model can be loaded without its data and its tables read one at
a time.
[`get_data()`](https://optimal2050.github.io/multimod/reference/get_data.md)
reads the table from the workspace and caches it in the model object it
returns from.

``` r

m <- load_model(ws, load_data = FALSE, verbose = FALSE)
is.null(m$parameters$pAvail$data)
#> [1] FALSE
get_data(m, "pAvail", type = "parameter")
#>   tech region value
#> 1 coal  north  0.85
#> 2  gas  north  0.85
#> 3 wind  north  0.90
#> 4 coal  south  0.90
#> 5  gas  south  0.35
#> 6 wind  south  0.25
get_data(m, "mTR", type = "mapping")
#>   tech region
#> 1 coal  north
#> 2  gas  north
#> 3 wind  north
#> 4 coal  south
#> 5  gas  south
#> 6 wind  south
get_data(m, "tech", type = "set")
#> [1] "coal" "gas"  "wind"
```

## Changing data

[`update_parameter()`](https://optimal2050.github.io/multimod/reference/update_parameter.md)
replaces a parameter’s table; with `auto_save = TRUE` the file in the
workspace is rewritten at the same time.

``` r

d <- get_data(m, "pDemand", type = "parameter")
d$value <- d$value * 1.1
m <- update_parameter(m, "pDemand", d, auto_save = TRUE)
read.csv(file.path(ws, "parameters", "pDemand", "data.csv"))
#>   region value
#> 1  north  1100
#> 2  south   660
```

The structure is unchanged, so the model can be re-solved on the new
data through any route, for example in R:

``` r

m <- load_model(ws, verbose = FALSE)
solve_highs(m, attach = FALSE)$objective
#> [1] 217800
```

## Solutions

The code-generation routes write their results under
`solvers/<backend>/solution/`. With `load_results = TRUE` (the default
when a model is passed),
[`solve_gmpl()`](https://optimal2050.github.io/multimod/reference/solve_gmpl.md),
[`solve_jump()`](https://optimal2050.github.io/multimod/reference/solve_jump.md)
and
[`solve_pyomo()`](https://optimal2050.github.io/multimod/reference/solve_pyomo.md)
read the variable values into `model$variables[[name]]$solution`, copy
them to `variables/`, and append a line to `solution_log.txt`, so the
workspace holds the latest loaded solution and a record of when it was
loaded.

``` r

r <- solve_jump(m, ws)
r$model$variables$vGen$solution
r$model$solution_metadata$variables_loaded
readLines(file.path(ws, "solution_log.txt"))
```

## Folded data

A folded parameter (see [Folding and
trimming](https://optimal2050.github.io/multimod/articles/folding.md))
keeps both tables: the original under `data.*` and the reduced one under
`folded_data/data.*`.
[`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md)
writes both,
[`load_model()`](https://optimal2050.github.io/multimod/reference/load_model.md)
reads both, and the writers take the folded table when the model is
folded.

## Reading the files elsewhere

The data files are plain CSV or Arrow and need nothing from multimod:

``` python
import pyarrow.feather as feather
demand = feather.read_feather("ws/parameters/pDemand/data.arrow")
```

``` julia
using Arrow, DataFrames
demand = DataFrame(Arrow.Table("ws/parameters/pDemand/data.arrow"))
```

`metadata.json` lists the counts and the format so a reader can orient
itself without R.

``` r

cat(readLines(file.path(ws, "metadata.json")), sep = "\n")
#> {
#>   "name": "toy",
#>   "desc": "two-region capacity expansion toy model",
#>   "saved": "2026-10-21 09:32:34",
#>   "format": "csv",
#>   "compression": null,
#>   "compression_level": null,
#>   "n_sets": 2,
#>   "n_parameters": 4,
#>   "n_mappings": 2,
#>   "n_equations": 3,
#>   "n_variables": 3
#> }
```
