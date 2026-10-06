# Quote GAMS explanatory text when it needs it

A description is free text after the symbol, and GAMS reads a comma in
it as the separator before the next declaration -
`ratio,hours,lower bound` declared symbols named `hours` and `lower`.
Quoting keeps it one token.

## Usage

``` r
gams_desc(x)
```

## Arguments

- x:

  Description text.

## Value

The text, quoted if it carries a comma, semicolon or quote.
