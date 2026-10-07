# Convert a multimod ast or equation to a visNetwork object

Convert a multimod ast or equation to a visNetwork object

## Usage

``` r
as_visNetwork(x, ...)

# S3 method for class 'ast'
as_visNetwork(
  x,
  main = x$name,
  submain = x$desc,
  alias_map = NULL,
  show_dims = TRUE,
  ...
)
```

## Arguments

- x:

  A multimod object

- ...:

  Passed on to methods.

- main:

  Title of the network plot (default: the object's name).

- submain:

  Subtitle of the network plot (default: the object's description).

- alias_map:

  A named list for alias mapping (optional).

- show_dims:

  Logical, whether to show dimensions in node labels.

## Value

A `visNetwork` htmlwidget.
