# The memo of tables read from disk

[`get_data()`](https://optimal2050.github.io/multimod/r/reference/get_data.md)
keeps the tables it reads from the workspace or from a symbol's own
reference in a session-wide memo, keyed by the table's content hash when
the symbol carries one and by its path otherwise, so that a mapping
several symbols share is read once per session rather than once per use.
The memo holds at most `getOption("multimod.data_cache_gb", 8)`
gigabytes, dropping the least recently used table first.

## Usage

``` r
clear_data_cache()

data_cache_info()
```

## Value

`clear_data_cache()` returns the number of tables dropped, invisibly.
`data_cache_info()` returns a data frame with one row per table: `key`,
`rows`, `bytes`.

## Details

`clear_data_cache()` empties it; `data_cache_info()` lists what it
holds.

## Examples

``` r
data_cache_info()
#> [1] rows  bytes
#> <0 rows> (or 0-length row.names)
clear_data_cache()
```
