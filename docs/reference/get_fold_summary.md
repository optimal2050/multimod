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
if (FALSE) { # \dontrun{
# Get structured list
stats <- get_fold_summary(model_folded)

# Get as data frame
df <- get_fold_summary(model_folded, format = "data.frame")

# Print formatted summary
get_fold_summary(model_folded, format = "text")
} # }
```
