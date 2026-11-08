# Export the bundled energyRt model as a solver workspace

Writes the energyRt model shipped in `example_models` to a directory, as
a model workspace (see
[`save_model()`](https://optimal2050.github.io/multimod/r/reference/save_model.md))
with one sub-directory per requested backend under `solvers/`. The GAMS
source is the file the model was parsed from; the MathProg, JuMP and
Pyomo code is rendered from the parsed model with
[`write_gmpl()`](https://optimal2050.github.io/multimod/r/reference/write_gmpl.md),
[`write_jump()`](https://optimal2050.github.io/multimod/r/reference/write_jump.md)
and
[`write_pyomo()`](https://optimal2050.github.io/multimod/r/reference/write_pyomo.md).

## Usage

``` r
export_model_source(
  dataset = c("energyrt"),
  dir,
  format = "all",
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  Name of the dataset to export. Currently only `"energyrt"`.

- dir:

  Directory path for the workspace. Created if it does not exist.

- format:

  Which backend to export: `"gams"`, `"gmpl"`, `"jump"`, `"pyomo"`, or
  `"all"` (default).

- overwrite:

  Logical. If `TRUE`, existing solver directories are rewritten. Default
  is `FALSE`: a backend whose directory exists is skipped.

- verbose:

  Logical. If `TRUE`, print progress messages. Default is `TRUE`.

## Value

Invisibly, a character vector of the files written.

## Details

The workspace layout is the one every `solve_*()` function expects:

- `model.rds`, `sets/`, `mappings/`, `parameters/`: the saved model and
  its data (CSV), written once by
  [`save_model()`](https://optimal2050.github.io/multimod/r/reference/save_model.md).

- `solvers/gams/model.gms`: the GAMS source of the energyRt model.

- `solvers/gmpl/model.mod` and `data.dat`: MathProg, run with
  `glpsol -m model.mod -d data.dat` from that directory.

- `solvers/jump/model.jl` and `data.jl`: JuMP, run with
  `julia model.jl`.

- `solvers/pyomo/model.py` and `data.py`: Pyomo, run with
  `python model.py`.

- `README.txt`: provenance and licence of the model.

## Examples

``` r
# \donttest{
ws <- file.path(tempdir(), "energyrt_ws")
files <- export_model_source("energyrt", ws, format = "gmpl")
#> Created directory: C:\Users\admin\AppData\Local\Temp\Rtmp6Z6Hhw/energyrt_ws
#> Wrote: model workspace (model.rds, sets/, mappings/, parameters/)
#> Wrote: README.txt
#> Wrote: solvers/gmpl/ (model.mod, data.dat)
#> Export complete: 4 file(s) written to C:\Users\admin\AppData\Local\Temp\Rtmp6Z6Hhw\energyrt_ws
basename(files)
#> [1] "model.rds"  "README.txt" "model.mod"  "data.dat"  

# the model section; its data file `data.dat` is the second argument of
# read_gmpl(), which takes longer to parse
model <- read_gmpl(file.path(ws, "solvers", "gmpl", "model.mod"))
model
#> Model Structure (gmpl)
#>   Sets:        332 
#>   Mappings:    0 
#>   Aliases:     10 
#>   Parameters:  218 
#>   Variables:   93 
#>   Equations:   153 
#>   Source:      C:\Users\admin\AppData\Local\Temp\Rtmp6Z6Hhw\energyrt_ws\solvers\gmpl\model.mod 
#>   Language:    gmpl 
unlink(ws, recursive = TRUE)
# }
```
