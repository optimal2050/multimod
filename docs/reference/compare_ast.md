# Compare two AST objects and report differences

Recursively compare two AST objects to identify structural and value
differences. Useful for debugging changes in model structure or equation
transformations.

## Usage

``` r
compare_ast(ast1, ast2, path = "root", max_diffs = 50)
```

## Arguments

- ast1:

  First AST object to compare

- ast2:

  Second AST object to compare

- path:

  Character vector tracking the path to current node (for reporting)

- max_diffs:

  Maximum number of differences to report (default 50)

## Value

A list with:

- identical:

  Logical, TRUE if ASTs are identical

- differences:

  data.frame with columns: path, field, value1, value2

- summary:

  Character string summarizing key differences

## Examples

``` r
m <- example_models$energyRt$multimod
eq1 <- m$equations$eqTechSng2Sng
eq2 <- m$equations$eqTechGrp2Sng
result <- compare_ast(eq1, eq2)
print(result)
#> Found 9 difference(s) across 9 node(s) 
#> 
#> Differences:
#>   [1] value @ root$name
#>       AST1: eqTechSng2Sng
#>       AST2: eqTechGrp2Sng
#> 
#>   [2] value @ root$desc
#>       AST1: Technology input to output
#>       AST2: Technology group input to output
#> 
#>   [3] value @ root$domain$name
#>       AST1: meqTechSng2Sng
#>       AST2: meqTechGrp2Sng
#> 
#>   [4] class @ root$lhs
#>       AST1: mm_variable, ast
#>       AST2: expression, ast
#> 
#>   [5] value @ root$rhs$op
#>       AST1: *
#>       AST2: /
#> 
#>   [6] value @ root$rhs$lhs$op
#>       AST1: *
#>       AST2: /
#> 
#>   [7] class @ root$rhs$lhs$lhs
#>       AST1: expression, ast
#>       AST2: mm_variable, ast
#> 
#>   [8] value @ root$rhs$lhs$rhs$name
#>       AST1: pTechCact2cout
#>       AST2: pTechUse2cact
#> 
#>   [9] class @ root$rhs$rhs
#>       AST1: mm_variable, ast
#>       AST2: mm_parameter, ast
#> 

# Compare specific parts
compare_ast(eq1$rhs, eq2$rhs, path = "rhs")
#> Found 5 difference(s) across 5 node(s) 
#> 
#> Differences:
#>   [1] value @ rhs$op
#>       AST1: *
#>       AST2: /
#> 
#>   [2] value @ rhs$lhs$op
#>       AST1: *
#>       AST2: /
#> 
#>   [3] class @ rhs$lhs$lhs
#>       AST1: expression, ast
#>       AST2: mm_variable, ast
#> 
#>   [4] value @ rhs$lhs$rhs$name
#>       AST1: pTechCact2cout
#>       AST2: pTechUse2cact
#> 
#>   [5] class @ rhs$rhs
#>       AST1: mm_variable, ast
#>       AST2: mm_parameter, ast
#> 
```
