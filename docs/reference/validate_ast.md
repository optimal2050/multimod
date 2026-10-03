# Validate an AST node or model against the schema

Checks that a node's class is known, that every required field is
present and non-`NULL`, and that it carries no field the schema does not
declare.

## Usage

``` r
validate_ast(x, path = "", recursive = TRUE)
```

## Arguments

- x:

  An AST node or a model.

- path:

  Internal; the path reported in messages.

- recursive:

  Logical; also check nested nodes (default `TRUE`).

## Value

A character vector of problems, empty if `x` is valid.

## Details

The same check runs automatically inside the constructors, so a
malformed node is normally refused where it is built rather than
surfacing later as a renderer that silently emits nothing - the trap
described in `CLAUDE.md`. Use this function to check a model that
already exists, for instance one read back from disk. Set
`options(multimod.validate = FALSE)` to switch the constructor-time
check off.

## See also

[`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md),
[`schema_type()`](https://optimal2050.github.io/multimod/reference/schema_type.md)

## Examples

``` r
validate_ast(ast_expression("*", ast_symbol("a"), ast_symbol("b")))
#> character(0)

# a node missing a required field
bad <- ast_expression("*", ast_symbol("a"), ast_symbol("b"))
bad$rhs <- NULL
validate_ast(bad)
#> [1] " (expression): required field 'rhs' is missing"
```
