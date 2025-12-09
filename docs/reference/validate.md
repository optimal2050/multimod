# Validate multimod or AST objects

Performs structural validation on low-level AST nodes and multimod
objects (parameters, variables, equations, etc.) as well as whole model
objects. These helpers are intended to catch malformed objects early
(e.g., dimensions stored as raw character vectors instead of
`ast_dims`). Call with `stop_on_error = FALSE` to collect diagnostics
without raising errors.

## Usage

``` r
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# Default S3 method
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# S3 method for class 'ast'
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# S3 method for class 'dims'
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# S3 method for class 'set'
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# S3 method for class 'symbol'
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# S3 method for class 'mapping'
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# S3 method for class 'ast_formula'
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# S3 method for class 'parameter'
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# S3 method for class 'variable'
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# S3 method for class 'equation'
validate(x, context = NULL, stop_on_error = TRUE, recursive = TRUE, ...)

# S3 method for class 'model'
validate(x, verbose = FALSE, stop_on_error = TRUE, ...)
```

## Arguments

- x:

  A multimod model object

- context:

  Optional character label identifying the object (used in error
  messages for AST or component validators)

- stop_on_error:

  Logical; if TRUE, stop with an error if validation fails. Default is
  TRUE.

- recursive:

  Logical; when TRUE (default) validators recursively inspect known
  child AST objects (e.g., dims, formulas). Set to FALSE to limit
  validation to the top-level object only (ignored for model
  validation).

- ...:

  Additional arguments (currently unused)

- verbose:

  Logical; if TRUE, print detailed progress messages for all checks.
  Default is FALSE.

## Value

A list containing `valid`, `errors`, `warnings`, `info`, and `context`
when `stop_on_error = FALSE`. Invisibly returns the same result when
`stop_on_error = TRUE` and validation passes.

A list with components:

- valid:

  Logical indicating if model passed all checks

- errors:

  Character vector of error messages (critical issues)

- warnings:

  Character vector of warning messages (potential issues)

- info:

  Character vector of informational messages

## Details

Current checks include:

- Presence of objective function

- Collision detection between set aliases and iterator variable names

- Verification that base sets referenced by aliases exist

- Detection of potential ambiguous naming

## Methods (by class)

- `validate(model)`: Validate a multimod model

  Checks a multimod model for structural issues, naming conflicts, and
  other potential problems that could cause issues during export or
  solving.

## Examples

``` r
if (FALSE) { # \dontrun{
m <- read_gmpl("model.mod", data_file = "data.dat")
validation <- validate(m)
if (!validation$valid) {
  cat("Errors found:\n")
  cat(paste(validation$errors, collapse = "\n"))
}
} # }
```
