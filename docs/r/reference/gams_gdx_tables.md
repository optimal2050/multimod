# Symbol tables for a GDX

Sets and mappings become one data frame of key columns; parameters gain
a trailing `value` column. Empty symbols are included as zero-row
frames, because a symbol that is declared and never assigned fails a
GAMS solve.

## Usage

``` r
gams_gdx_tables(model, use_folded = FALSE)
```

## Arguments

- model:

  A `multimod` model with data attached.

- use_folded:

  Logical; prefer folded data where a parameter carries it.

## Value

A list with `sets` and `params`, each a named list of data frames.
