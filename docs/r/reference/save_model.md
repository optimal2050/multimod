# Save model data on disk

Save model data on disk

## Usage

``` r
save_model(
  model,
  path,
  format = c("parquet", "ipc", "csv"),
  compression = "zstd",
  compression_level = 3,
  overwrite = TRUE,
  keep_in_memory = TRUE,
  save_structure = TRUE,
  structure = c("rds", "json", "both"),
  verbose = TRUE
)
```

## Arguments

- model:

  A multimod model object

- path:

  Path to save the model files

- format:

  Character. Data format: "parquet" (default), "ipc" (Arrow IPC), or
  "csv"

- compression:

  Compression method to use (`"zstd"`)

- compression_level:

  Compression level; zstd 1-22, default 3. Levels up to 12 give the same
  size on model data; 15 is a third smaller on sorted tables at several
  times the time

- overwrite:

  Logical; if TRUE, overwrite existing files

- keep_in_memory:

  Logical; if TRUE, keep data in memory after saving

- save_structure:

  Logical; if TRUE, save model structure separately

- structure:

  How the structure (sets, mappings, parameters, variables, equations)
  is written: `"rds"` (the default) as `model.rds`, `"json"` as
  `model.json` through
  [`model_to_json()`](https://optimal2050.github.io/multimod/r/reference/model_to_json.md)
  for readers outside R, or `"both"`.
  [`load_model()`](https://optimal2050.github.io/multimod/r/reference/load_model.md)
  reads either.

- verbose:

  Logical; if TRUE, print progress messages

## Value

Invisibly, the model with storage metadata attached.

## Details

Directory structure created (the extension follows `format`):

    path/
      model.rds                       model structure (without data when
                                      keep_in_memory = FALSE)
      model.json                      the structure in the JSON encoding of
                                      model_to_json(), with structure = "json"
                                      or "both"
      metadata.json                   counts and format, for readers outside R
      format.txt                      "csv", "ipc" or "parquet"
      sets/sets.csv                   index of the sets
      sets/<name>/data.*              one file per set, column `member`
      mappings/mappings.csv           index of the mappings
      mappings/<name>/data.*          one column per dimension
      parameters/parameters.csv       index of the parameters
      parameters/<name>/data.*        dimension columns plus `value`
      parameters/<name>/folded_data/data.*   if the parameter is folded

The solvers write their generated code and output under
`path/solvers/<backend>/`; see the "Model workspace and data" article.

## Examples

``` r
m <- example_models$energyRt$multimod
ws <- file.path(tempdir(), "energyRt_ws")

# CSV needs no further package
m <- save_model(m, ws, format = "csv", verbose = FALSE)

# The default parquet format needs 'arrow'; here the data is also dropped
# from memory, and get_data() reloads it from disk
if (requireNamespace("arrow", quietly = TRUE)) {
  m_light <- save_model(m, ws, keep_in_memory = FALSE, verbose = FALSE)
  head(get_data(m_light, "pTechOlife"))
}
#>   tech region value
#> 1 ECOA     C1    30
#> 2 ECOA     C2    30
#> 3 ECOA     C3    30
#> 4 ECOA     C4    30
#> 5 ECOA     C5    30
#> 6 ECOA     W1    30
```
