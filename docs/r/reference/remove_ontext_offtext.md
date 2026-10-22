# Strip GAMS block comments

Removes everything between `$ontext` and `$offtext`, inclusive.

## Usage

``` r
remove_ontext_offtext(lines)
```

## Arguments

- lines:

  Character vector of GAMS source lines.

## Value

The lines with block comments removed.

## Details

Exported because consumers that parse GAMS sources of their own -
energyRt's model code, for one - need the same treatment before scanning
for domain hints, and duplicating it would let the two drift.

## See also

[`read_gams()`](https://optimal2050.github.io/multimod/r/reference/read_gams.md),
[`parse_equation_header()`](https://optimal2050.github.io/multimod/r/reference/parse_equation_header.md)

## Examples

``` r
remove_ontext_offtext(c("a;", "$ontext", "note", "$offtext", "b;"))
#> [1] "a;" "b;"
```
