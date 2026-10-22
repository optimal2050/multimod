# Render a node that sits in condition position

A mapping in condition position is a membership test. Rendered as a
plain reference it becomes a bare `model.mX`, which Pyomo evaluates as a
truthy Set object, so the gate always passes and the expression indexes
variables outside their domain.

## Usage

``` r
pyomo_condition(node, model = NULL, var_names = NULL, scope = NULL, ...)
```

## Arguments

- node:

  The condition node.

- model, var_names, scope:

  Passed through to
  [`as_pyomo()`](https://optimal2050.github.io/multimod/r/reference/as_pyomo.md).

- ...:

  Further arguments for
  [`as_pyomo()`](https://optimal2050.github.io/multimod/r/reference/as_pyomo.md).

## Value

A Pyomo boolean expression.
