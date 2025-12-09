# Create a multimod variable

Create a multimod variable

## Usage

``` r
new_multimod_variable(
  name,
  desc = NULL,
  dims,
  data = NULL,
  active_dims = NULL,
  domain = "continuous",
  auto_fold = TRUE
)
```

## Arguments

- name:

  character, name of the variable

- dims:

  character vector, names of the declared dimensions

- data:

  data frame, data for the variable

- active_dims:

  character vector, names of the active dimensions

- domain:

  character, domain of the variable (e.g., "continuous", "integer",
  "binary")

- auto_fold:

  logical, whether to automatically fold dimensions

## Value

a multimod_variable object
