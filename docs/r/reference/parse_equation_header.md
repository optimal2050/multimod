# Parse a GAMS equation header

Splits the part of an equation before `..` into its name, index list and
optional `$` condition.

## Usage

``` r
parse_equation_header(line)
```

## Arguments

- line:

  A single line of GAMS source ending in `..`.

## Value

A list with the header's components, or `NULL` if `line` is not an
equation header.

## Details

Exported because consumers that compile their own GAMS-flavoured IR into
multimod equations need exactly this parse. energyRt's user constraints
(`newConstraint()`) arrive as GAMS strings whose headers look like
`eqCnsX(region, year)$mCnsForEachX(region, year)..`, and re-implementing
the split would let the two parsers disagree.

## See also

[`read_gams()`](https://optimal2050.github.io/multimod/r/reference/read_gams.md),
[`parse_gams_equation()`](https://optimal2050.github.io/multimod/r/reference/parse_gams_equation.md),
[`remove_ontext_offtext()`](https://optimal2050.github.io/multimod/r/reference/remove_ontext_offtext.md)

## Examples

``` r
parse_equation_header("eqCost(region, year).. ")
#> $name
#> [1] "eqCost"
#> 
#> $dims
#> [1] "region" "year"  
#> 
#> $condition
#> NULL
#> 
```
