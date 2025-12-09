# Create a multimod mapping object

Create a multimod mapping object

## Usage

``` r
new_multimod_mapping(
  name,
  desc = NULL,
  subset_of = NULL,
  dims = NULL,
  active_dims = NULL,
  data = NULL,
  auto_fold = FALSE
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

- data:

  data frame, data for the mapping

## Value

An object of class `multimod_mapping`
