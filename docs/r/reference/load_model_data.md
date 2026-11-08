# Bring a model's tables into memory

Fills `$data` of every parameter and mapping that holds no table in
memory but has one on disk, through
[`get_data()`](https://optimal2050.github.io/multimod/r/reference/get_data.md)
(and so through its memo). The writers that render a model to text read
`$data` directly; they call this first, so a model whose tables are
references renders the same as one that holds them.

## Usage

``` r
load_model_data(model, symbols = NULL, rules = TRUE)
```

## Arguments

- model:

  A multimod model.

- symbols:

  Names of the symbols to load; `NULL` for all.

- rules:

  Enumerate rule mappings
  ([`mapping_rule()`](https://optimal2050.github.io/multimod/r/reference/mapping_rule.md))
  as tables too; `FALSE` leaves them as rules.

## Value

The model with the tables attached.

## Examples

``` r
m <- load_model_data(example_models$toy)
```
