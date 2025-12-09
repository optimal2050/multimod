# Read GMPL/MathProg Model File

Parse a GLPK/MathProg (GMPL) model file and return a model_structure
object. This is the intermediate representation that can be converted to
multimod format using as_multimod().

## Usage

``` r
register_index_alias(collection, set_name, alias_name)
```

## Arguments

- model_file:

  Path to GMPL model file (.txt or .mod), or a character vector
  containing the model code lines (for in-memory parsing)

- data_file:

  Optional path to separate .dat data file, or character vector
  containing data lines (not yet implemented for parsing, but can be
  provided)

- as_multimod:

  Logical. If TRUE (default), convert directly to multimod_model object.
  If FALSE, return model_structure object.

- verbose:

  Logical. If TRUE, print parsing progress (default FALSE)

## Value

A model_structure object (or multimod_model if as_multimod=TRUE)

## Examples

``` r
if (FALSE) { # \dontrun{
# From file paths
mod_struct <- read_gmpl("osemosys.txt", as_multimod = FALSE)

# From in-memory character vectors (e.g., package data)
data(example_models)
model <- read_gmpl(
  model_file = example_models$OSeMOSYS$gmpl$model,
  data_file = example_models$OSeMOSYS$gmpl$data
)

# Convert to multimod
model <- as_multimod(mod_struct)

# Or directly
model <- read_gmpl("osemosys.txt", as_multimod = TRUE)
} # }
```
