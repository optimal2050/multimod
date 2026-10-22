# Is this node a declaration rather than a reference?

The AST uses one class name for two structurally different things: a
declaration
([`new_set()`](https://optimal2050.github.io/multimod/r/reference/new_set.md),
carrying `data`, `desc` and friends) and a reference
([`ast_set()`](https://optimal2050.github.io/multimod/r/reference/new_ast.md),
carrying a name and the dims it is used at). The `"multimod"` class
marker is what separates them - measured across the whole packaged
model, it sits on every declaration and on nothing else.

## Usage

``` r
is_declaration(x)
```

## Arguments

- x:

  An AST node.

## Value

`TRUE` for a declaration, `FALSE` otherwise.

## Details

Prefer this to `inherits(x, "multimod")`, which says what the marker
*is* rather than what it *means*.

## See also

[`ast_schema()`](https://optimal2050.github.io/multimod/r/reference/ast_schema.md),
[`schema_type()`](https://optimal2050.github.io/multimod/r/reference/schema_type.md)

## Examples

``` r
is_declaration(new_set("tech"))   # TRUE
#> [1] TRUE
is_declaration(ast_set("tech"))   # FALSE
#> [1] FALSE
```
