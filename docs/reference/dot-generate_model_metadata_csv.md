# Save multimod model workspace to disk

Save a complete multimod model to a workspace directory with efficient
Arrow storage for data frames. The directory structure mirrors the model
object structure for consistency and easy navigation.

## Usage

``` r
.generate_model_metadata_csv(model, model_dir)
```

## Arguments

- model:

  A multimod model object

- path:

  Character. Directory path for model workspace

- format:

  Character. Data format: "csv" (default), "ipc" (Arrow IPC/Feather), or
  "parquet"

- compression:

  Character. Compression codec for Arrow formats: "zstd" (default),
  "lz4", "snappy", or "uncompressed"

- compression_level:

  Integer. Compression level (default: 15 for zstd)

- overwrite:

  Logical. Overwrite existing model workspace directory

- keep_in_memory:

  Logical. Keep data in memory after saving (default: TRUE)

- save_structure:

  Logical. Save equations, variables, and other structure (default:
  TRUE)

- verbose:

  Logical. Print progress messages

## Value

Modified model object with storage metadata

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
if (FALSE) { # \dontrun{
# Save with default CSV format
model <- save_model(model, "models/utopia")

# Save with Arrow IPC (fastest)
model <- save_model(model, "models/utopia", format = "ipc")

# Save and free memory
model <- save_model(model, "models/utopia",
                    format = "ipc",
                    keep_in_memory = FALSE)
} # }
```
