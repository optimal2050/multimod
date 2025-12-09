# Resolve alias to full set name

Resolve alias to full set name

## Usage

``` r
resolve_full_name(
  alias,
  base = base_aliases,
  extended = extended_aliases,
  default = NULL
)
```

## Arguments

- alias:

  A character vector of alias names (e.g., "r", "yp").

- base:

  Named list of base aliases.

- extended:

  Named list of extended aliases.

- default:

  Fallback if alias not found.

## Value

Character vector of full names.
