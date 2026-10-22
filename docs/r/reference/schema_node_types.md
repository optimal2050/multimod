# Node types known to the schema

Node types known to the schema

## Usage

``` r
schema_node_types()
```

## Value

Character vector of node type names.

## See also

[`ast_schema()`](https://optimal2050.github.io/multimod/r/reference/ast_schema.md)

## Examples

``` r
schema_node_types()
#>  [1] "set_decl"       "mapping_decl"   "parameter_decl" "variable_decl" 
#>  [5] "equation_decl"  "set_ref"        "mapping_ref"    "parameter_ref" 
#>  [9] "variable_ref"   "symbol"         "dims"           "constant"      
#> [13] "expression"     "unary"          "when"           "func"          
#> [17] "sum"            "prod"           "setmin"         "setmax"        
#> [21] "shift"          "call"           "where"         
```
