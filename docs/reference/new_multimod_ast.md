# Create an AST node representing a set

Constructs a set object used as an index domain for other symbols (e.g.,
mappings, parameters, variables, equations).

## Usage

``` r
ast_set(name)
```

## Arguments

- name:

  Character. Name of the set (e.g., "b").

## Value

An object of class `multimod_ast` and `ast_set`

## Examples

``` r
ast_set("b")                     # standalone set
#> <AST set> b
ast_set("b", domain = ast_set("a"))  # subset declaration b ⊆ a
#> Error in ast_set("b", domain = ast_set("a")): unused argument (domain = ast_set("a"))
```
