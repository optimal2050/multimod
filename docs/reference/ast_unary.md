# Create a unary expression AST node

Represents a unary operator such as "-" or "not" applied to a single
argument.

## Usage

``` r
ast_unary(op, rhs)
```

## Arguments

- op:

  Character string. The unary operator, e.g., "-" or "not".

- rhs:

  The operand (a `ast` object).

## Value

An object of class `ast` and `unary`.

## Examples

``` r
ast <- ast_unary("-", ast_symbol("x"))
str(ast)
#> List of 2
#>  $ op : chr "-"
#>  $ rhs:List of 1
#>   ..$ name: chr "x"
#>   ..- attr(*, "class")= chr [1:2] "symbol" "ast"
#>  - attr(*, "class")= chr [1:2] "unary" "ast"
ast_unary("not", ast_symbol("x"))
#> <AST unary>
#>   Operator: not
#>   Argument:
#> <AST symbol> x
```
