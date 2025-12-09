# Check if a string is alphanumeric (letters, digits, or underscores)

This function tests whether the input string consists entirely of
letters (`a–z`, `A–Z`), digits (`0–9`), or underscores (`_`).

## Usage

``` r
is_word_num(ch)
```

## Arguments

- ch:

  A character string or vector of strings to test.

## Value

A logical vector the same length as `ch`, where each element is `TRUE`
if the corresponding string contains only alphanumeric characters and
underscores, `FALSE` otherwise.

## Examples

``` r
is_word_num("alpha123")     # TRUE
#> [1] TRUE
is_word_num("var_1")        # TRUE
#> [1] TRUE
is_word_num("a-b")          # FALSE
#> [1] FALSE
is_word_num(c("abc", "123", "a_b", "x-y"))  # TRUE, TRUE, TRUE, FALSE
#> [1]  TRUE  TRUE  TRUE FALSE
```
