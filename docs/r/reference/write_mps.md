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
  presolve = getOption("multimod.presolve", FALSE),
  stream = FALSE,
  buckets = 64L,
  substitute = getOption("multimod.substitute")
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
  `"parquet"` (default; zstd level 3), `"ipc"` or `"csv"`.

- verbose:

  Logical; report progress.

- presolve:

  Logical; eliminate proportional columns with
  [`lp_presolve()`](https://optimal2050.github.io/multimod/r/reference/lp_presolve.md)
  before writing.
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md)
  restores them. Defaults to `getOption("multimod.presolve", FALSE)`.

- stream:

  Logical; build the matrix block by block and write the file without
  holding the matrix in memory. The entries are spooled to bucket files
  in `<file>.parts/` by column range and merged into the file at the
  end; the file is equivalent to the one HiGHS writes. With
  `presolve = TRUE` the blocks are built twice, a scan pass that finds
  the doubleton rows and an emit pass that applies the substitution.
  `lp` cannot be given with `stream = TRUE`.

- buckets:

  Integer; with `stream = TRUE`, the number of bucket files (one is read
  back into memory at a time).

- substitute:

  Character vector of definition equations to substitute while the
  matrix is built (`stream = TRUE` only). A definition is an equality
  whose domain is the index set of one of its variables, linear in the
  others: its rows are not written, the variable has no columns on its
  domain, and every other reference to it there is replaced by the
  definition. The definitions are written beside the index tables
  (`<base>_subst/`) and
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md)
  rebuilds the variable from them. The objective's own definition can be
  among them (see
  [`objective_chain()`](https://optimal2050.github.io/multimod/r/reference/objective_chain.md)):
  the objective is then written on the columns the definitions resolve
  to, and its constant part, kept beside the index tables
  (`<base>_objective.csv`), is added back by
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/r/reference/read_mps_solution.md).
  Defaults to `getOption("multimod.substitute")`.

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
#> WARNING: Row    names are not present, or contain duplicates: using names with prefix "r", beginning with suffix 0
#> Writing the model to C:\Users\admin\AppData\Local\Temp\RtmpOuni4X\mps-315424fc6fd/toy.mps
#> [1] "toy.mps"           "toy_col_index.csv" "toy_dim_names.csv"
#> [4] "toy_row_index.csv"
```
