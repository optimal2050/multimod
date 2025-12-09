# Parse GMPL Expression to AST

Convert a GMPL expression string to multimod AST format

## Usage

``` r
gmpl_symbol_node(name, symbols)
```

## Arguments

- symbols:

  Named list of known symbols (sets, parameters, variables)

- expr:

  Character string containing GMPL expression

- depth:

  Current recursion depth (internal use)

- max_depth:

  Maximum recursion depth

## Value

AST node
