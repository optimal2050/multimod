# Create a multimod parameter

Create a multimod parameter

## Usage

``` r
new_parameter(
  name,
  desc = NULL,
  dims,
  active_dims = NULL,
  data = NULL,
  defVal = NULL,
  defInt = NULL,
  symbolic = FALSE,
  formula = NULL,
  comment = NULL,
  auto_fold = FALSE,
  symbols = NULL
)
```

## Arguments

- name:

  character, name of the parameter

- desc:

  character, description of the parameter

- dims:

  character vector, names of the declared dimensions

- active_dims:

  character vector, names of the active dimensions

- data:

  data frame, data for the parameter

- defVal:

  numeric, default value for elements not present in `data`

- defInt:

  numeric, default value used for integer-valued elements (reserved;
  `NULL` in the packaged models)

- symbolic:

  logical, whether the parameter is defined by `formula` rather than by
  `data`

- formula:

  AST expression defining the parameter when `symbolic = TRUE`

- comment:

  character, comment line from source file (e.g., \*@ domain hint)

- auto_fold:

  logical, whether to automatically fold dimensions

- symbols:

  optional named list of known symbols (sets, aliases) used to resolve
  dimension names in
  [`ast_dims()`](https://optimal2050.github.io/multimod/r/reference/ast_dims.md)

## Value

a parameter object

## Examples

``` r
new_parameter("pDemand", desc = "annual demand, MWh", dims = "region",
              data = data.frame(region = c("north", "south"), value = c(1000, 600)))
#> <AST parameter> pDemand
#>   dims:  [region] 
```
