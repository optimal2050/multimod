# Create AST Formula Node

Wraps an expression AST with metadata about which parameter field it
populates. Used for both default expressions and parameter formulas.

## Usage

``` r
new_ast_formula(
  target,
  field,
  expr,
  index_vars = character(),
  index_sets = character(),
  args = character()
)
```

## Arguments

- target:

  Character, name of parameter this formula belongs to

- field:

  Character, which field: "defVal" or "formula"

- expr:

  AST node representing the expression

- index_vars:

  Character vector of index variable names used in expr

- index_sets:

  Character vector of set names corresponding to index_vars

- args:

  Character vector of parameter names this expr depends on

## Value

ast_formula object
