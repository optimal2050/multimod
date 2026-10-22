# Check if a character is a special character

Check if a character is a special character

## Usage

``` r
is_special(ch)
```

## Arguments

- ch:

  A character string to check.

## Value

A logical value indicating whether the character is a special character.

## Examples

``` r
is_special("!") # TRUE
#> [1] TRUE
is_special("a") # FALSE
#> [1] FALSE
is_special("1") # FALSE
#> [1] FALSE
is_special(1) # FALSE
#> [1] FALSE
is_special("#") # TRUE
#> [1] TRUE
```
