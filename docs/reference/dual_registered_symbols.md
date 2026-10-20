# Names registered as both a set and a mapping

A subset declaration over a known set (`sup(process)`) is written
exactly like a single-dimension mapping (`mUpComm(comm)`), so
[`read_gams()`](https://optimal2050.github.io/multimod/reference/read_gams.md)
registers such a name on both sides and resolves each occurrence by
usage - bare as a set, parenthesised as a mapping. Both registrations
name ONE symbol, so a writer declares it once.

## Usage

``` r
dual_registered_symbols(model)
```

## Arguments

- model:

  A `multimod` model

## Value

Character vector of names.
