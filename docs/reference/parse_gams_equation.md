# Parse a GAMS equation into an AST object.

Parse a GAMS equation into an AST object.

## Usage

``` r
parse_gams_equation(eqn_info, symbols)
```

## Arguments

- eqn_info:

  A GAMS-code snippet containing the equation information, including its
  name, GAMS body, domain, and description.

- symbols:

  A list of symbols (sets, parameters, variables) used in the GAMS
  model, which will be used to resolve dimensions and expressions.

## Value

An `equation` object containing the parsed equation structure, including
its name, dimensions, left-hand side (LHS), right-hand side (RHS),
relation, domain, and description.
