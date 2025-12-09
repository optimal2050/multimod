# Generate dummy variable name from set name

Creates a short dummy variable name from a set name following
conventions:

- Uses preferred names for common sets (tech -\> h, slice -\> ts, etc.)

- First letter of the set name (lowercase) for others

- If it's an alias, use two letters (checking against all_set_names)

- Ensures uniqueness by checking against all symbols in model

## Usage

``` r
generate_index_alias(
  set_name,
  used_names = character(0),
  all_set_names = character(0),
  all_symbols = NULL
)
```

## Arguments

- set_name:

  Character. The name of the set

- used_names:

  Character vector. Names already used (to avoid conflicts)

- all_set_names:

  Character vector. All set names in the model (to detect aliases)

- all_symbols:

  List. All symbols in model (sets, parameters, variables, etc.) to
  avoid conflicts

## Value

Character. A short dummy variable name

## Examples

``` r
generate_index_alias("tech") # "h"
#> [1] "h"
generate_index_alias("slice") # "ts"
#> [1] "ts"
generate_index_alias("commp", all_set_names = c("comm", "commp")) # "cp"
#> [1] "cp"
```
