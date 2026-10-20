# Convert an equation node to Pyomo rule definition (internal helper)

Convert an equation node to Pyomo rule definition (internal helper)

## Usage

``` r
render_pyomo_equation(eq, model = NULL)
```

## Value

A list with `rule`, the Python source lines of the rule function, and
`assignment`, the line adding the constraint to the model.
