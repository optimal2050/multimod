# Create a multimod mapping object

Create a multimod mapping object

## Usage

``` r
new_mapping(
  name,
  desc = NULL,
  subset_of = NULL,
  dims = NULL,
  active_dims = NULL,
  data = NULL,
  auto_fold = FALSE,
  symbols = NULL
)
```

## Arguments

- name:

  character, name of the mapping

- desc:

  character, description of the mapping

- subset_of:

  character, name of the parent mapping

- dims:

  character vector, names of the declared dimensions

- active_dims:

  character vector, names of the active dimensions (defaults to `dims`)

- data:

  data frame, data for the mapping

- auto_fold:

  logical, whether to automatically fold dimensions

- symbols:

  optional named list of known symbols (sets, aliases) used to resolve
  dimension names in
  [`ast_dims()`](https://optimal2050.github.io/multimod/r/reference/ast_dims.md)

## Value

An object of class `mapping`

## Examples

``` r
pairs <- expand.grid(
  tech = c("coal", "wind"),
  region = c("north", "south"),
  stringsAsFactors = FALSE
)
new_mapping("mTR", desc = "technology x region pairs",
            dims = c("tech", "region"), data = pairs)
#> <AST mapping> mTR
#>   dims:  [tech,region] 
```
