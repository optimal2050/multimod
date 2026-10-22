# Create a multimod variable

Create a multimod variable

## Usage

``` r
new_variable(
  name,
  desc = NULL,
  dims,
  active_dims = NULL,
  domain = NULL,
  vtype = NULL,
  bounds = NULL,
  comment = NULL,
  data = NULL,
  auto_fold = FALSE,
  symbols = NULL
)
```

## Arguments

- name:

  character, name of the variable

- desc:

  character, description of the variable

- dims:

  character vector, names of the declared dimensions

- active_dims:

  character vector, names of the active dimensions

- domain:

  character, domain mapping name for sparse indexing (NULL = Cartesian,
  character(0) = unused)

- vtype:

  character, variable type as declared in the source model (e.g.
  `"positive"`, `"binary"`, `"integer"`)

- bounds:

  list with optional elements `lo` and `up` giving the lower and upper
  bounds

- comment:

  character, comment line from source file (e.g., \*@ domain hint)

- data:

  data frame, data for the variable

- auto_fold:

  logical, whether to automatically fold dimensions

- symbols:

  optional named list of known symbols (sets, aliases) used to resolve
  dimension names in
  [`ast_dims()`](https://optimal2050.github.io/multimod/r/reference/ast_dims.md)

## Value

a variable object

## Examples

``` r
new_variable("vGen", desc = "generation, MWh", dims = c("tech", "region"),
             domain = "mTR", vtype = "positive", bounds = list(lo = 0))
#> <AST variable> vGen
#>   dims:  [tech,region] 
```
