# Build an AST node for a GMPL identifier

Looks up a GMPL identifier among the known symbols and returns a typed
AST node (variable, parameter, set) with canonical casing, or a plain
`symbol` node when the identifier is unknown.

## Usage

``` r
gmpl_symbol_node(name, symbols)
```

## Arguments

- name:

  Character string; identifier as it appears in the GMPL source

- symbols:

  Named list of known symbols (sets, parameters, variables)

## Value

AST node
