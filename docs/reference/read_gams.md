# Read GAMS model file and parse its core structure.

This function reads a GAMS model file or text, expands any included
files, removes comments, and parses the core structure into sets,
parameters, variables, equations, and aliases, returning a
`model_structure` object.

## Usage

``` r
read_gams(
  file_or_text,
  include = TRUE,
  interim_file = NULL,
  strict = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- file_or_text:

  A path to a GAMS file, or a character vector containing GAMS code
  (either as a single string with newlines, or as a vector with one line
  per element).

- include:

  Logical indicating whether to expand included files (default is
  `TRUE`). Only applies when reading from a file path.

- interim_file:

  Optional path to an interim file where the processed GAMS code
  prepended for parsing will be saved. If `NULL`, no interim file is
  created.

- strict:

  Logical indicating whether to enforce strict parsing rules (default is
  `TRUE`). If `FALSE`, some undeclared equations may be added to the
  model structure.

- verbose:

  Logical indicating whether to print verbose output during parsing
  (default is `FALSE`).

- ...:

  Additional arguments passed to other functions, currently unused.

## Value

A `model_structure` object containing the parsed GAMS model structure,
including sets, mappings, aliases, parameters, variables, and equations.
