# Collect declared symbols from a model object

Collect declared symbols from a model object

## Usage

``` r
collect_model_symbols(
  model,
  include_aliases = TRUE,
  include_index_aliases = TRUE
)
```

## Arguments

- model:

  A `multimod` model

- include_aliases:

  Logical; include set alias information

- include_index_aliases:

  Logical; include index alias information

## Value

A list containing the symbol table, lookup map, and collision info
