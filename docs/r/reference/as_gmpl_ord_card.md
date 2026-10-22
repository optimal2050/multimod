# Render GAMS `ord()` / `card()` for GMPL

GAMS `ord(s)` is the 1-based position of a member within its set, and
`card(s)` the set's cardinality. **MathProg has neither function.** The
only faithful translation is a parameter carrying those values, which
the model must supply - substituting the member's own value is wrong
whenever set members are not consecutive integers (e.g. years 2030,
2040, 2050), and `ord(y) < pOlife + ord(yp)` is position arithmetic, not
value arithmetic.

## Usage

``` r
as_gmpl_ord_card(x, model = NULL, ...)
```

## Arguments

- x:

  A `func` AST node named `ord` or `card`.

- model:

  The model, used to resolve set aliases and to check that the position
  parameter actually exists.

- ...:

  Additional arguments passed to `as_gmpl`.

## Value

A GMPL parameter reference, e.g. `ordYear[y]`.

## Details

The expected parameter follows the convention `ord<Set>` / `card<Set>`
on the *base* set, so `ord(year)`, `ord(yearp)` and `ord(yeare)` all
render as `ordYear[<iterator>]`. Alias sets are resolved to their base
before naming.
