# Extract the member values of a set

Extract the member values of a set

## Usage

``` r
extract_set_members(set_obj)
```

## Value

A character or numeric vector of the set's members (numeric when every
member is a number),
[`character()`](https://rdrr.io/r/base/character.html) for a set with no
members, or `NULL` when `set_obj` or its data is `NULL`.
