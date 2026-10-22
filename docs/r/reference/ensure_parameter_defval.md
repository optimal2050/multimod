# Ensure parameters retain a scalar default after evaluation

Ensure parameters retain a scalar default after evaluation

## Usage

``` r
ensure_parameter_defval(param)
```

## Value

`param` with a scalar `defVal` (the mean of its data, else 0) and a
`misc$defVal_fallback` record when a fallback was applied.
