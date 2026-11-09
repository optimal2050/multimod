# Solve an MPS file and write a solution file

Reads an MPS file written by
[`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.md),
solves it and writes a solution file that
[`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md)
decodes against the index tables.

## Usage

``` r
solve_mps(
  file,
  sol_file = NULL,
  control = list(),
  require_optimal = TRUE,
  verbose = FALSE,
  solver = c("highs", "julia_highs", "cuopt"),
  where = c("local", "wsl")
)
```

## Arguments

- file:

  Path to the MPS file. HiGHS in this process reads it uncompressed;
  cuOpt reads `.gz` as well.

- sol_file:

  Path for the solution file; default `file` with a `.sol` extension, in
  the directory of `file`.

- control:

  Solver options; see Details.

- require_optimal:

  Logical; error unless the run ends optimal.

- verbose:

  Logical; report progress.

- solver:

  `"highs"`, `"julia_highs"` or `"cuopt"`.

- where:

  `"local"` or `"wsl"`.

## Value

A list with `status_message`, `objective`, `sol_file` and `run_time`
(seconds).

## Details

Solvers and places:

- `"highs"`, `"local"`: HiGHS in this R process (the `highs` package).

- `"julia_highs"`, `"local"`: HiGHS through Julia (HiGHS.jl), whose
  build carries the HiPO interior point method. Julia is found by
  [`get_multimod_julia()`](https://optimal2050.github.io/multimod/r/reference/multimod-config.md).

- `"cuopt"`, `"local"`: NVIDIA cuOpt's `cuopt_cli` on this machine
  (Linux).

- `"cuopt"`, `"wsl"`: `cuopt_cli` inside WSL on a local GPU (Windows);
  the solution is written inside WSL and copied out in one piece.

`control` holds the solver's options. HiGHS (both places): named HiGHS
options, e.g. `list(solver = "ipm")`. cuOpt: `method` (default `1`,
PDLP), `time_limit` (seconds; `cuopt_cli`'s own default is short),
`presolve`, `options` (further `cuopt_cli` flags as one string or a
character vector); for `"wsl"` also `wsl_user` and `cuopt_cli` (path
inside WSL), else the environment variables `MULTIMOD_WSL_USER` and
`MULTIMOD_CUOPT_CLI`, else WSL's default user and `cuopt_cli` on its
`PATH`.

A cuOpt run (any place) can go through cuOpt's Python interface instead
of `cuopt_cli` (`control$api = "python"`). It writes the solution file
whatever the status: at a time limit the current iterate is kept (status
`TimeLimit`), where `cuopt_cli` writes no values.
`control$save_warm_start = TRUE` also returns PDLP's warm-start data as
`warm_start.pkl` beside `sol_file`; `control$warm_start` (a path to such
a file) continues from it. Both run PDLP in its `Stable2` mode, the one
cuOpt warm-starts, and without cuOpt's presolve (unless
`control$presolve` is set): the warm-start data has the size of the
problem PDLP solved. Locally and in WSL the Python is
`control$cuopt_python`, else the environment variable
`MULTIMOD_CUOPT_PYTHON`, else `python3` beside `cuopt_cli`.

Scripts and parameter files a solver needs go to temporary files and are
removed afterwards; the cuOpt log goes to `control$log_file`, else to a
temporary file. Only the solution (and, when asked, the warm-start data)
is written beside `sol_file`.

## Examples

``` r
if (requireNamespace("highs", quietly = TRUE)) {
  dir <- tempfile("mps-")
  dir.create(dir)
  mps <- file.path(dir, "toy.mps")
  write_mps(example_models$toy, mps, format = "csv")
  s <- solve_mps(mps)
  s$objective
  sol <- read_mps_solution(s$sol_file, index_dir = dir, format = "csv")
  head(sol$primal)
}
#> WARNING: Column names are not present, or contain duplicates: using names with prefix "c", beginning with suffix 0
#> WARNING: Row    names are not present, or contain duplicates: using names with prefix "r", beginning with suffix 0
#> Writing the model to C:\Users\admin\AppData\Local\Temp\Rtmp8QjY7E\mps-e76c22ea1556/toy.mps
#>        j symbol     i1     i2    lo    up    vtype     value reduced_cost
#>    <int> <char> <char> <char> <num> <num>   <char>     <num>        <num>
#> 1:     1   vCap   coal  north     0   Inf positive    0.0000            0
#> 2:     2   vCap    gas  north     0   Inf positive    0.0000            0
#> 3:     3   vCap   wind  north     0   Inf positive 1111.1111            0
#> 4:     4   vCap   coal  south     0   Inf positive  666.6667            0
#> 5:     5   vCap    gas  south     0   Inf positive    0.0000            0
#> 6:     6   vCap   wind  south     0   Inf positive    0.0000            0
```
