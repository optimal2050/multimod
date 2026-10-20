# Resolve the parsing language for AST helpers

Resolve the parsing language for AST helpers

## Usage

``` r
resolve_ast_language(language = NULL, symbols = NULL, context = NULL)
```

## Arguments

- language:

  Explicit language override (e.g., "gams", "gmpl").

- symbols:

  Symbol table or similar object that may carry a `language`
  field/attribute.

- context:

  Optional object (model structure, builder) that exposes `$language` or
  a `language` attribute.

## Value

A lower-case language identifier, e.g. `"gams"` or `"gmpl"`.
