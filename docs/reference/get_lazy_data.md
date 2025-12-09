# Get lazy data with automatic loading

Get lazy data with automatic loading

## Usage

``` r
get_lazy_data(obj, base_path = NULL, collect = TRUE)
```

## Arguments

- obj:

  Parameter or mapping object

- base_path:

  Base path for relative paths

- collect:

  Logical. Collect into memory?

## Value

Data (data.table if collect=TRUE, Arrow dataset if FALSE)
