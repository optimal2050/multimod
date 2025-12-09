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

  File format for saving ("csv", "rds", "parquet", etc.)

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

Invisibly returns NULL

## Examples

``` r
if (FALSE) { # \dontrun{
save_model(model, path = "model_data", format = "csv")
} # }
```
