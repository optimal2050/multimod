# Export import log to CSV file

Export import log to CSV file

## Usage

``` r
export_import_log(import_log, log_file)
```

## Arguments

- import_log:

  The import log list produced by
  [`import_gmpl_data()`](https://optimal2050.github.io/multimod/reference/import_gmpl_data.md).

- log_file:

  Path of the CSV file to write.

## Value

`NULL`; called for its side effect of writing `log_file`.
