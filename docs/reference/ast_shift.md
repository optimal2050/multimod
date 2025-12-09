# Create an index shift AST node for ordered set references

Constructs a shifted index reference for ordered sets (e.g., YEAR,
SEASON). Used for GAMS lag/lead operators like `y-1` (previous year) or
`y+1` (next year). Negative offset represents backward shift (lag),
positive represents forward shift (lead).

## Usage

``` r
ast_shift(symbol, offset, ...)
```

## Arguments

- symbol:

  A character string representing the index symbol (e.g., "y", "ls",
  "ld").

- offset:

  An integer offset. Negative for backward shift (e.g., -1 for y-1),
  positive for forward shift (e.g., +1 for y+1).

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
