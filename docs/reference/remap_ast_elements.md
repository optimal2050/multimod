# Replace AST elements with "ast_where" nodes

This function replaces elements of a given AST object with "ast_where"
nodes. It is useful for long expressions to improve readability of latex
output and network trees. The "ast_where" stores the original expression
in a `$content` slot, and ignored by the parsers to GAMS, Julia, and
other languages.

## Usage

``` r
remap_ast_elements(
  obj,
  ast_type = list(when = "condition"),
  name_prefix = "m",
  n = 0L,
  latex_max = 10,
  ...
)
```

## Arguments

- obj:

  An object of class `ast`, `multimod`, or a list.

- ast_type:

  A character vector of AST types to be replaced (e.g., "when",
  "mapping").

- name_prefix:

  A character string prefix for the new names of the replaced elements.
  Defaults to "m".

## Value

Modified AST object with remapped elements
