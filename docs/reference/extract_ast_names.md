# Extract names of specific node types from AST

Recursively traverses an AST and collects the names of nodes matching
the specified types (e.g., "variable", "parameter", "mapping").

## Usage

``` r
extract_ast_names(ast, types = c("variable", "parameter", "mapping"))
```

## Arguments

- ast:

  An AST object (equation LHS/RHS, or any expression node)

- types:

  Character vector of node types to extract (e.g., c("variable",
  "parameter"))

## Value

Character vector of unique names found
