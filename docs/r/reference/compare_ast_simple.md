# Compare AST objects using compare package with custom handling

Alternative comparison using the compare package. Simpler but may be
more verbose for complex AST structures.

## Usage

``` r
compare_ast_simple(ast1, ast2, verbose = getOption("multimod.verbose", TRUE))
```

## Arguments

- ast1:

  First AST object

- ast2:

  Second AST object

- verbose:

  Print the report? Defaults to the option `multimod.verbose` (`TRUE`).

## Value

Invisibly, the result of
[`compare::compare()`](https://rdrr.io/pkg/compare/man/compare.html).
