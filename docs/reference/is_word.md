# Check if a string is a word (alphabetic characters only)

This function tests whether the input string consists entirely of
letters (a–z, A–Z) with no digits, punctuation, or special characters.

## Usage

``` r
is_word(ch)
```

## Arguments

- ch:

  A character string or vector of strings to test.

## Value

A logical vector the same length as `ch`, where each element is `TRUE`
if the corresponding string consists only of letters, `FALSE` otherwise.

## Examples

``` r
is_word("alpha")     # TRUE
#> [1] TRUE
is_word("123")       # FALSE
#> [1] FALSE
is_word("var_1")     # FALSE
#> [1] FALSE
is_word(c("a", "B", "C3"))  # TRUE, TRUE, FALSE
#> [1]  TRUE  TRUE FALSE
```
