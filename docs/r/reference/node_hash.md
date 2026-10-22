# Generate a stable hash for an AST or multimod object (excluding internal hash fields)

Generate a stable hash for an AST or multimod object (excluding internal
hash fields)

## Usage

``` r
node_hash(node, algo = NULL)
```

## Arguments

- node:

  An `ast` object.

- algo:

  Hashing algorithm. If NULL, uses `options("multimod.hash_algo")`.

## Value

A character hash (e.g., "42fca0dd").

## Examples

``` r
ast <- ast_expression("+", ast_variable("x"), ast_constant(5))
hash <- node_hash(ast)
```
