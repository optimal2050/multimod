# Recursively fold AST nodes

Walks AST tree and removes folded dimensions from parameter indexing

## Usage

``` r
fold_ast_node(node, folded_params, model = NULL, debug = FALSE)
```

## Arguments

- node:

  AST node

- folded_params:

  Named list of parameter names -\> removed dimensions

- model:

  Model object (for alias resolution)

## Value

Updated AST node
