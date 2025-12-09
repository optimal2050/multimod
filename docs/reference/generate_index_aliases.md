# Generate dummy variable names for a list of sets

Creates unique dummy variable names for a collection of sets

## Usage

``` r
generate_index_aliases(
  set_names,
  all_set_names = set_names,
  all_symbols = NULL
)
```

## Arguments

- set_names:

  Character vector. Names of the sets

- all_set_names:

  Character vector. All set names in model (for alias detection)

- all_symbols:

  List. All symbols in model (from build_symbols_list)

## Value

Named character vector. Dummy variable names with set names as names

## Examples

``` r
generate_index_aliases(c("tech", "region", "comm", "commp", "year", "slice"))
#>   tech region   comm  commp   year  slice 
#>    "h"    "r"    "c"   "cp"    "y"   "ts" 
```
