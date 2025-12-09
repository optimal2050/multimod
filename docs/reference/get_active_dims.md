# Get active dimensions from a parameter

Returns the dimensions that are actually present in a parameter's data.
For folded parameters, returns the reduced dimensions. For unfolded
parameters, returns the original dimensions.

## Usage

``` r
get_active_dims(param, folded = FALSE)
```

## Arguments

- param:

  A parameter object with `dims` and optionally `active_dims`

- folded:

  Logical; if TRUE, returns folded dimensions only (returns NULL for
  unfolded parameters). If FALSE (default), returns active dimensions
  regardless of folding state.

## Value

A dims object (list of set objects), or NULL if folded=TRUE and
parameter is not folded

## Details

During folding, parameters store `active_dims` which reflects the
reduced dimensionality. This function provides a consistent way to
retrieve the appropriate dimensions for code generation and validation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get active dimensions (works for folded or unfolded)
dims <- get_active_dims(param)

# Check if parameter is folded
folded_dims <- get_active_dims(param, folded = TRUE)
is_folded <- !is.null(folded_dims)
} # }
```
