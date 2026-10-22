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

- n:

  Integer; starting value of the counter used to number the replaced
  elements (default 0).

- latex_max:

  Numeric; elements whose rendered LaTeX is shorter than this estimated
  length are left in place (default 10).

- ...:

  Further arguments, currently ignored.

## Value

Modified AST object with remapped elements
