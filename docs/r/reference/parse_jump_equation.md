# Parse one JuMP constraint into a multimod equation

Parse one JuMP constraint into a multimod equation

## Usage

``` r
parse_jump_equation(text, symbols, model = NULL, desc = "")
```

## Arguments

- text:

  One `@constraint(...)` statement, as
  [`write_jump()`](https://optimal2050.github.io/multimod/r/reference/write_jump.md)
  emits it or as a modeller writes it. Newlines are allowed.

- symbols:

  Symbol table from
  [`build_symbols_list()`](https://optimal2050.github.io/multimod/r/reference/build_symbols_list.md).

- model:

  The model, used to resolve a mapping's declared index names so that
  JuMP's short aliases come back as the names the model uses.

- desc:

  Description to attach to the equation.

## Value

An equation node.

## See also

[`mm_equation()`](https://optimal2050.github.io/multimod/r/reference/mm_equation.md)
reaches this with `lang = "jump"`.
