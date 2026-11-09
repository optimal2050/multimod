# Detect Arrow data format

Detect Arrow data format

## Usage

``` r
detect_arrow_format(path)
```

## Arguments

- path:

  Path to data directory or file

## Value

Character format name: "parquet", "ipc", "csv", or NULL. Files named
`.feather` are Arrow IPC files and detected as "ipc".
