# Get trim summary statistics

Get trim summary statistics

## Usage

``` r
get_trim_summary(model, format = c("list", "data.frame", "text"))
```

## Arguments

- model:

  A multimod model object

- format:

  Output format: "list" (default), "data.frame", or "text"

## Value

Trim statistics in requested format

## Examples

``` r
if (FALSE) { # \dontrun{
# After trimming
model <- trim_model(model)

# Get summary
summary <- get_trim_summary(model)
summary_df <- get_trim_summary(model, format = "data.frame")
cat(get_trim_summary(model, format = "text"))
} # }
```
