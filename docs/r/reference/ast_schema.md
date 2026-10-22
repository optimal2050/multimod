# The multimod AST schema

A declarative description of every AST node type: its kind (`decl`,
`ref` or `expr`), the S3 class it corresponds to, and its fields with
their types and whether they are required.

## Usage

``` r
ast_schema()
```

## Value

A list with `version`, `nodes` and `model`.

## Details

Serialisation
([`model_to_json()`](https://optimal2050.github.io/multimod/r/reference/model_to_json.md),
[`model_from_json()`](https://optimal2050.github.io/multimod/r/reference/model_from_json.md)),
validation
([`validate_ast()`](https://optimal2050.github.io/multimod/r/reference/validate_ast.md))
and the emitted JSON Schema
([`ast_json_schema()`](https://optimal2050.github.io/multimod/r/reference/ast_json_schema.md))
all read this rather than restating the node shapes, so there is one
definition to keep correct.

## See also

[`schema_type()`](https://optimal2050.github.io/multimod/r/reference/schema_type.md),
[`ast_json_schema()`](https://optimal2050.github.io/multimod/r/reference/ast_json_schema.md),
[`validate_ast()`](https://optimal2050.github.io/multimod/r/reference/validate_ast.md)

## Examples

``` r
s <- ast_schema()
s$version
#> [1] "1.0"
names(s$nodes)
#>  [1] "set_decl"       "mapping_decl"   "parameter_decl" "variable_decl" 
#>  [5] "equation_decl"  "set_ref"        "mapping_ref"    "parameter_ref" 
#>  [9] "variable_ref"   "symbol"         "dims"           "constant"      
#> [13] "expression"     "unary"          "when"           "func"          
#> [17] "sum"            "prod"           "setmin"         "setmax"        
#> [21] "shift"          "call"           "where"         
s$nodes$expression$fields
#> $op
#> $op$type
#> [1] "string"
#> 
#> $op$required
#> [1] TRUE
#> 
#> 
#> $lhs
#> $lhs$type
#> [1] "node"
#> 
#> $lhs$required
#> [1] TRUE
#> 
#> 
#> $rhs
#> $rhs$type
#> [1] "node"
#> 
#> $rhs$required
#> [1] TRUE
#> 
#> 
#> $brackets
#> $brackets$type
#> [1] "logical"
#> 
#> $brackets$required
#> [1] FALSE
#> 
#> 
```
