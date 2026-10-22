# Render ord()/card() as a model-supplied position parameter

Pyomo has no ord()/card(), so they go through the same position and
cardinality parameters the MathProg writer uses - `ord(year)` becomes
`model.ordYear[y]`. Alias sets resolve to their base, so ord(yearp) and
ord(yeare) name the same parameter.

## Usage

``` r
pyomo_ord_card(x, model = NULL, var_names = NULL, scope = NULL)
```

## Arguments

- x:

  A `func` node named `ord` or `card`.

- model:

  The model, for alias resolution and the existence check.

- var_names, scope:

  Passed through for iterator resolution.

## Value

A Pyomo parameter reference.
