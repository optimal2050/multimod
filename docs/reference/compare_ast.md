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
if (FALSE) { # \dontrun{
# Compare two equation ASTs
eq1 <- model_old$equations$eqTechCap
eq2 <- model_new$equations$eqTechCap
result <- compare_ast(eq1, eq2)
print(result)

# Compare specific parts
result <- compare_ast(eq1$rhs, eq2$rhs, path = "eqTechCap$rhs")
} # }
```
