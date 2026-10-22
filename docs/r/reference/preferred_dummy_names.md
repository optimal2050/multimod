# Preferred short names for common sets to avoid conflicts

Preferred short names for common sets to avoid conflicts

## Usage

``` r
preferred_dummy_names
```

## Format

A named list mapping set names to preferred short iterator names.

## Value

A named list of character scalars, one short iterator name per set name;
consulted by
[`generate_index_alias()`](https://optimal2050.github.io/multimod/r/reference/generate_index_alias.md)
before falling back to the first letter of the set name.
