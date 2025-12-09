# Save parameter data to disk and free memory

Save parameter data to disk and free memory

## Usage

``` r
save_parameter(model, param_name, format = "parquet", keep_in_memory = FALSE)
```

## Arguments

- model:

  Multimod model

- param_name:

  Name of parameter to save

- format:

  Data format: "parquet", "feather", "ipc", "csv"

- keep_in_memory:

  Keep data in memory after saving?

## Value

Modified model
