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
  show_dims = TRUE
)
```

## Arguments

- x:

  A multimod object
