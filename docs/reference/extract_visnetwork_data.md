# Extract visNetwork-compatible nodes and edges from a multimod_equation

Extract visNetwork-compatible nodes and edges from a multimod_equation

## Usage

``` r
extract_visnetwork_data(eqn, alias_map = NULL)
```

## Arguments

- eqn:

  A `multimod_equation` object

- alias_map:

  Optional named list to rename variable/parameter/index names

## Value

A list with elements: `nodes`, `edges`, `title`, `subtitle`
