# Save data to disk using Arrow

Save data to disk using Arrow

## Usage

``` r
save_arrow_data(data, path, format = "parquet", overwrite = FALSE)
```

## Arguments

- data:

  Data frame or data.table to save

- path:

  Path to save to

- format:

  Format: "parquet", "feather", "ipc", "csv"

- overwrite:

  Logical. Overwrite existing data?
