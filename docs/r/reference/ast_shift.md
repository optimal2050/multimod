# Create an index shift AST node for ordered set references

Constructs a shifted index reference for ordered sets (e.g., YEAR,
SEASON). Used for GAMS lag/lead operators like `y-1` (previous year) or
`y+1` (next year). Negative offset represents backward shift (lag),
positive represents forward shift (lead).

## Usage

``` r
ast_shift(symbol, offset, circular = FALSE, ...)
```

## Arguments

- symbol:

  A character string representing the index symbol (e.g., "y", "ls",
  "ld").

- offset:

  An integer offset. Negative for backward shift (e.g., -1 for y-1),
  positive for forward shift (e.g., +1 for y+1).

- circular:

  Logical. `FALSE` (default): a shift past either end of the set refers
  to no member, so the term is dropped. `TRUE`: the shift wraps around
  the ends of the set (GAMS `y--1`, `y++1`).

- ...:

  Additional fields stored on the node (passed to
  [`new_ast()`](https://optimal2050.github.io/multimod/r/reference/new_ast.md)).

## Value

A `shift` S3 object (subclass of `ast`).

## Examples

``` r
ast_shift("y", -1)  # y-1 (previous year)
#> <ast> 
#> [1] "y"  "-1"
ast_shift("y", 1)   # y+1 (next year)
#> <ast> 
#> [1] "y" "1"
ast_shift("ls", -1) # ls-1 (previous season)
#> <ast> 
#> [1] "ls" "-1"
```
