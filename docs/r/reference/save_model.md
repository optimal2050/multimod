# Save model data on disk

Save model data on disk

## Usage

``` r
save_model(
  model,
  path,
  format = c("csv", "ipc", "parquet"),
  compression = "zstd",
  compression_level = 15,
  overwrite = TRUE,
  keep_in_memory = TRUE,
  save_structure = TRUE,
  verbose = TRUE
)
```

## Arguments

- model:

  A multimod model object

- path:

  Path to save the model files

- format:

  Character. Data format: "csv" (default), "ipc" (Arrow IPC/Feather), or
  "parquet"

- compression:

  Compression method to use

- compression_level:

  Compression level (1-9)

- overwrite:

  Logical; if TRUE, overwrite existing files

- keep_in_memory:

  Logical; if TRUE, keep data in memory after saving

- save_structure:

  Logical; if TRUE, save model structure separately

- verbose:

  Logical; if TRUE, print progress messages

## Value

Invisibly, the model with storage metadata attached.

## Details

Directory structure created (the extension follows `format`):


    path/
      model.rds                       model structure (without data when
                                      keep_in_memory = FALSE)
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

# Save with the default CSV format
m <- save_model(m, ws, verbose = FALSE)

# Save and drop the data from memory; get_data() reloads it from disk
m_light <- save_model(m, ws, keep_in_memory = FALSE, verbose = FALSE)
head(get_data(m_light, "pTechOlife"))
#>   tech region value
#> 1 ECOA     C1    30
#> 2 ECOA     C2    30
#> 3 ECOA     C3    30
#> 4 ECOA     C4    30
#> 5 ECOA     C5    30
#> 6 ECOA     W1    30
```
