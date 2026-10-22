# Convert multimod object to GAMS syntax

Render a `multimod` object as a GAMS code string.

## Usage

``` r
as_gams(x, ...)

# S3 method for class 'func'
as_gams(x, ...)

# S3 method for class 'call'
as_gams(x, ...)
```

## Arguments

- x:

  A `multimod` object.

- ...:

  Additional arguments (not used).

## Value

A character string with valid GAMS syntax.

## Examples

``` r
toy <- example_models$toy
as_gams(toy$equations$eqDemand)
#> [1] "eqDemand[region]$mR[region]..\n  sum(tech, vGen[tech,region]) =g=\n  pDemand[region];"
```
