# Recursively extract elements by name from a nested structure

Recursively extract elements by name from a nested structure

## Usage

``` r
extract_ast_elements(obj, name, recursive = TRUE)
```

## Arguments

- obj:

  A nested list or S3 object (e.g., multimod or ast node)

- name:

  Character string of the slot/element to extract (e.g., "when")

- recursive:

  Logical, whether to search recursively through nested objects

## Value

A list of all matching elements

## Examples

``` r
extract_elements_by_name(model, "when")
#> Error in extract_elements_by_name(model, "when"): could not find function "extract_elements_by_name"
```
