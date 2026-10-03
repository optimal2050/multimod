# Constructors under the cross-language names

Aliases for the AST and model constructors, spelled the way the same
type is spelled in C++ and Python (`Set("tech")` rather than
`new_set("tech")`). They call the existing constructors and return the
same objects; the original names are unaffected.

## Usage

``` r
Set(...)

Mapping(...)

Parameter(...)

Variable(...)

Equation(...)

SetRef(...)

MappingRef(...)

ParameterRef(...)

VariableRef(...)

Symbol(...)

Dims(...)

Constant(...)

Expression(...)

Unary(...)

When(...)

Func(...)

Sum(...)

Prod(...)

SetMin(...)

SetMax(...)

Shift(...)

Call(...)

Where(...)
```

## Arguments

- ...:

  Arguments passed to the underlying constructor.

## Value

An AST node.

## Details

Declarations take the bare name (`Set()`, `Parameter()`, `Variable()`,
`Mapping()`, `Equation()`); references take a `Ref` suffix (`SetRef()`,
`ParameterRef()`, `VariableRef()`, `MappingRef()`). The two are
structurally different objects - see
[`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md).

## See also

[`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md),
[`new_set()`](https://optimal2050.github.io/multimod/reference/new_set.md),
[`ast_set()`](https://optimal2050.github.io/multimod/reference/new_ast.md)

## Examples

``` r
Set("tech")                      # a declaration
#> <AST set> tech
SetRef("tech")                   # a reference
#> <AST set> tech
Expression("*", Symbol("a"), Symbol("b"))
#> <AST expression> *
#>   lhs :
#> <AST symbol> a
#>   rhs:
#> <AST symbol> b
```
