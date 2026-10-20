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
m <- example_models$energyRt$multimod
collect_ast_classes(m$equations$eqTechSng2Sng)
#> [1] "mm_equation"  "dims"         "symbol"       "mm_mapping"   "mm_variable" 
#> [6] "expression"   "mm_parameter"

# All node classes used anywhere in the model
collect_ast_classes(m)
#>  [1] "mm_set"       "mm_mapping"   "dims"         "symbol"       "mm_parameter"
#>  [6] "mm_variable"  "mm_equation"  "expression"   "func"         "when"        
#> [11] "constant"     "unary"       
```
