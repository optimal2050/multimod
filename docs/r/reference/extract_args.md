# Extract Arguments from AST

Recursively extracts all symbol names (potential arguments) from an AST
node. This is used to automatically determine function arguments when
converting AST to R functions.

## Usage

``` r
extract_args(x, args = character())
```

## Arguments

- x:

  An AST node

- args:

  Character vector of accumulated argument names

## Value

Character vector of unique argument names
