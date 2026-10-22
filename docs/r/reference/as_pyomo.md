# Convert a multimod AST node to Pyomo syntax

This helper mirrors
[`as_jump()`](https://optimal2050.github.io/multimod/r/reference/as_jump.md)
but emits Python/Pyomo-friendly strings. The implementation focuses on
linear expressions (sum/prod, arithmetic operators, variable/parameter
references). Unsupported node types raise informative errors so coverage
can be extended incrementally.

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

## Examples

``` r
toy <- example_models$toy
cat(as_pyomo(toy$equations$eqDemand, model = toy))
#> def _eqDemand_rule(model, r):
#>     if not (r in model.mR):
#>         return Constraint.Skip
#>     _expr = sum(model.vGen[tech, r] for tech in model.tech) >= model.pDemand[r]
#>     if _expr is True or _expr is False:
#>         return Constraint.Feasible if _expr else Constraint.Infeasible
#>     return _expr
#> 
#> model.eqDemand = Constraint(model.region, rule=_eqDemand_rule)
```
