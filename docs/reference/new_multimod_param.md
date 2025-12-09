# Create a multimod parameter

Create a multimod parameter

## Usage

``` r
new_multimod_param(
  name,
  desc = NULL,
  dims,
  active_dims = NULL,
  data = NULL,
  auto_fold = FALSE
)
```

## Arguments

- name:

  character, name of the parameter

- dims:

  character vector, names of the declared dimensions

- active_dims:

  character vector, names of the active dimensions

- data:

  data frame, data for the parameter

- auto_fold:

  logical, whether to automatically fold dimensions

## Value

a multimod_param object
