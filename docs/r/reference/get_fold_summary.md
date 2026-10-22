# Get folding statistics summary

Retrieves comprehensive folding statistics including list of folded
parameters, dimension reductions, compression ratios, and removed
dimensions.

## Usage

``` r
get_fold_summary(
  model,
  format = c("list", "data.frame", "text"),
  verbose = FALSE
)
```

## Arguments

- model:

  A multimod model object

- format:

  Character string specifying output format: "list" (default) returns
  structured list, "data.frame" returns tabular format, "text" prints
  formatted summary

- verbose:

  Logical; if `TRUE` and `format = "text"`, print a note when the model
  has no folding applied (default: FALSE)

## Value

Depending on format:

- "list": List with fold_summary and statistics

- "data.frame": Data frame with one row per folded parameter

- "text": Prints formatted summary and returns invisibly

## Details

The function extracts folding statistics stored in
model\$misc\$fold_summary during fold_model() execution. Each folded
parameter includes:

- Original and folded dimensions

- Removed dimension names

- Original and folded row counts

- Compression ratio

## Examples

``` r
m <- example_models$energyRt$multimod
spec <- create_fold_spec(m, fold_dims = "timeslice", verbose = FALSE)
m_folded <- fold_model(m, fold_spec = spec, verbose = FALSE)

# Structured list
stats <- get_fold_summary(m_folded)

# As a data frame
df <- get_fold_summary(m_folded, format = "data.frame")

# Formatted text
get_fold_summary(m_folded, format = "text")
```
