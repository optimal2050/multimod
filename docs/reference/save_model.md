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

Directory structure created:

    model_path/
      model.rds              # Model structure (thinned if keep_in_memory=FALSE)
      metadata.json          # Human-readable metadata
      format.txt             # Storage format
      sets/
        set_name.txt         # Set members (one per line)
      parameters/
        param_name/
          data.{csv|arrow|parquet}   # Parameter data
          metadata.rds               # Parameter metadata
      mappings/
        mapping_name/
          data.{csv|arrow|parquet}
      folded_data/           # If folding applied
        param_name/
          folded_data.{csv|arrow|parquet}
      equations/
        equations.rds        # All equations
      variables/
        variables.rds        # All variables

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
