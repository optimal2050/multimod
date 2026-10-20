# Register an index alias for a set

Adds `alias_name` to a named alias collection under `set_name`. Skips
empty values, an alias already registered for that set, and set names
that are themselves alias values (which would chain `l` to `l1`).

## Usage

``` r
register_index_alias(collection, set_name, alias_name)
```

## Arguments

- collection:

  Named character vector of aliases (set name -\> alias).

- set_name:

  Name of the set to alias.

- alias_name:

  Alias to register.

## Value

The collection with the alias appended, or unchanged.
