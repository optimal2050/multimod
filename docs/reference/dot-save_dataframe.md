# Internal: Save dataframe to disk

Internal: Save dataframe to disk

## Usage

``` r
.save_dataframe(
  data,
  path,
  format,
  compression = "zstd",
  compression_level = 15,
  verbose = FALSE,
  label = NULL
)
```

## Value

Invisibly, the path of the written file, or `NULL` when `data` is empty.
