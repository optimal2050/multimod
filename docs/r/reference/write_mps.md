# Write a model to MPS, with the index tables needed to read results back

HiGHS writes MPS quickly precisely because it does not carry symbolic
names: the file uses positional ones (`c0`, `r0`, ...). The row and
column index tables written alongside carry the mapping back to
`(symbol, index tuple)`, so a solution vector returned by a remote
solver can be interpreted.

## Usage

``` r
write_mps(
  model,
  file,
  index_dir = dirname(file),
  lp = NULL,
  format = c("parquet", "ipc", "csv"),
  verbose = FALSE,
  presolve = getOption("multimod.presolve", FALSE)
)
```

## Arguments

- model:

  A multimod model with data attached.

- file:

  Path of the `.mps` file to write.

- index_dir:

  Directory for the index tables; defaults to the directory of `file`.

- lp:

  Optional pre-built LP from
  [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md).

- format:

  Storage format for the index tables, passed to the package's
  [`.save_dataframe()`](https://optimal2050.github.io/multimod/r/reference/dot-save_dataframe.md):
  `"parquet"` (default), `"ipc"` or `"csv"`.

- verbose:

  Logical; report progress.

- presolve:

  Logical; eliminate proportional columns with
  [`lp_presolve()`](https://optimal2050.github.io/multimod/r/reference/lp_presolve.md)
  before writing.
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md)
  restores them. Defaults to `getOption("multimod.presolve", FALSE)`.

## Value

Invisibly, a list of the paths written.

## Details

**The `.mps` and its index files must travel together** - the file alone
cannot be mapped back to model symbols.

## Examples

``` r
if (requireNamespace("highs", quietly = TRUE)) {
  dir <- tempfile("mps-")
  dir.create(dir)
  mps <- file.path(dir, "toy.mps")
  write_mps(example_models$toy, mps, format = "csv")
  list.files(dir)
}
#> WARNING: Column names are not present, or contain duplicates: using names with prefix "c", beginning with suffix 0
#> 
#> WARNING: Row    names are not present, or contain duplicates: using names with prefix "r", beginning with suffix 0
#> 
#> Writing the model to C:\Users\admin\AppData\Local\Temp\RtmpgRRJWR\mps-ba245d30da/toy.mps
#> 
#> [1] "toy.mps"           "toy_col_index.csv" "toy_dim_names.csv"
#> [4] "toy_row_index.csv"
```
