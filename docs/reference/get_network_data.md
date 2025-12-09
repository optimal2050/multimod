# Get network data for a multimod equation

Get network data for a multimod equation

## Usage

``` r
get_network_data(x, alias_map = NULL, show_dims = TRUE)
```

## Arguments

- alias_map:

  A named list for alias mapping (optional)

- show_dims:

  Logical, whether to show dimensions in the label (default: FALSE)

- eq:

  A multimod equation object

## Value

A list with two data frames: nodes and edges to be used with visNetwork
or similar packages for visualization.
