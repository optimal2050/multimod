# Resolve a dimension name to its alias

Resolve a dimension name to its alias

## Usage

``` r
resolve_alias(
  name,
  base = base_aliases,
  extended = extended_aliases,
  default = NULL
)
```

## Arguments

- name:

  A character string or vector of dimension names.

- base:

  Named list of base aliases.

- extended:

  Named list of extended aliases.

- default:

  If no alias is found, return `name`, `NA`, or `"?"`.

## Value

A character vector of resolved aliases.
