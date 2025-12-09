# Collect all AST node classes from model or AST

Walks through an AST or model structure and collects all unique AST node
classes encountered. Non-AST objects are ignored.

## Usage

``` r
collect_ast_classes(obj)
```

## Arguments

- obj:

  An AST node, model object, or nested structure containing AST nodes

## Value

Character vector of unique AST classes found

## Examples

``` r
if (FALSE) { # \dontrun{
gmpl <- read_gmpl("model.mod")
classes <- collect_ast_classes(gmpl)
# Returns: c("symbol", "constant", "expression", "setmin", ...)
} # }
```
