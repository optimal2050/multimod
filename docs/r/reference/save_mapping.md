# Save mapping data to disk and free memory

Save mapping data to disk and free memory

## Usage

``` r
save_mapping(model, mapping_name, format = "parquet", keep_in_memory = FALSE)
```

## Arguments

- model:

  Multimod model

- mapping_name:

  Name of mapping to save

- format:

  Data format: "parquet", "ipc" (Arrow IPC) or "csv"

- keep_in_memory:

  Keep data in memory after saving?

## Value

Modified model
