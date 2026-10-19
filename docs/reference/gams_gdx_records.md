# GDX symbol records for gdxtools

Builds the list `wgdx()` takes. Written here rather than via
[`gdxtools::write.gdx()`](https://rdrr.io/pkg/gdxtools/man/write.gdx.html)
because that drops zero-valued parameter records, and those are needed
as EPS.

## Usage

``` r
gams_gdx_records(tbl)
```

## Arguments

- tbl:

  Output of
  [`gams_gdx_tables()`](https://optimal2050.github.io/multimod/reference/gams_gdx_tables.md).

## Value

A list of GDX symbol records.
