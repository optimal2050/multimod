# Write a model's data to a GDX for GAMS

Companion to
[`write_gams()`](https://optimal2050.github.io/multimod/reference/write_gams.md):
the generated model declares the symbols and `$load`s them from this
file. Pass the same `use_folded` to both.

## Usage

``` r
write_gams_data(
  model,
  file,
  use_folded = FALSE,
  gams_dir = NULL,
  verbose = FALSE
)
```

## Arguments

- model:

  A `multimod` model with data attached.

- file:

  Path of the GDX to write. NEOS decodes its input GDX to `in.gdx`.

- use_folded:

  Logical; prefer folded data where a parameter carries it.

- gams_dir:

  GAMS system directory for the GDX library. `NULL` lets
  `gdxtools::igdx()` find one.

- verbose:

  Logical; report what was written.

## Value

Invisibly, the character vector of symbol names written.

## Details

Needs the `gdxtools` package and a GAMS installation for the GDX
library. Writing a GDX does not require a GAMS licence.

Zero-valued parameter records are written as GAMS `EPS`. A GDX stores no
zeros for parameters, so a stored zero would otherwise be
indistinguishable from an absent one and would be overwritten by the
default that
[`write_gams()`](https://optimal2050.github.io/multimod/reference/write_gams.md)
assigns after loading.

## Testing status

This route is exercised against an older GDX library and gdxtools 0.7,
which does not install on R \>= 4.6, with the solve run on NEOS.
Contributions testing current GDX libraries, a newer gdxtools and other
GAMS entry points are welcome.

## See also

[`write_gams()`](https://optimal2050.github.io/multimod/reference/write_gams.md)
