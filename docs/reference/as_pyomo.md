# Convert a multimod AST node to Pyomo syntax

This helper mirrors
[`as_jump()`](https://optimal2050.github.io/multimod/reference/as_jump.md)
but emits Python/Pyomo-friendly strings. The implementation focuses on
linear expressions (sum/prod, arithmetic operators, variable/parameter
references) which cover the current OSeMOSYS workflow. Unsupported node
types raise informative errors so we can extend coverage incrementally.

## Usage

``` r
as_pyomo(x, ...)
```

## Arguments

- x:

  An AST node (e.g. equation, expression, variable, parameter).

- ...:

  Additional arguments used internally:

  - `model`: multimod model for alias lookups.

  - `var_names`: named vector mapping base set names to iterator aliases
    (used when emitting constraint bodies).

  - `scope`: local iterator scope for nested aggregations.

## Value

Character string containing Pyomo-compatible code.
