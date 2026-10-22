# Check if model or object is trimmed

Check if model or object is trimmed

## Usage

``` r
is_trimmed(x)
```

## Arguments

- x:

  Model object, or individual set/parameter/mapping/variable/equation

## Value

Logical indicating if trimmed

## Examples

``` r
m <- example_models$energyRt$multimod
is_trimmed(m)
#> [1] FALSE

m_trimmed <- trim_model(m, verbose = FALSE)
is_trimmed(m_trimmed)
#> [1] TRUE
is_trimmed(m_trimmed$parameters$pTechOlife)
#> [1] FALSE
```
