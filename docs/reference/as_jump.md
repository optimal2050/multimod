# Convert multimod object to Julia/JuMP syntax

Render a `multimod` object as a JuMP code string.

## Usage

``` r
as_jump(x, ...)
```

## Arguments

- x:

  A `multimod` object.

- ...:

  Additional arguments (not used).

## Value

A character string with valid Julia/JuMP syntax.

## Examples

``` r
toy <- example_models$toy
as_jump(toy$equations$eqDemand, model = toy)
#> [1] "@constraint(\n    model,\n    eqDemand[(r) in mR],\n    sum(vGen[(h, r)] for h in tech) >= get(pDemand, r, pDemandDef)\n)"
```
