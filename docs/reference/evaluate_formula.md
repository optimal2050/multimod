# Evaluate formula AST over all dimension combinations

Evaluate formula AST over all dimension combinations

## Usage

``` r
evaluate_formula(
  formula_ast,
  dims,
  dims_index_aliases,
  all_data,
  model,
  verbose = FALSE
)
```

## Value

A numeric scalar (no dimensions), a named numeric vector (one dimension)
or a numeric array with `dimnames` (several dimensions).
