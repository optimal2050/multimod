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
m <- trim_model(example_models$energyRt$multimod, verbose = FALSE)

summary <- get_trim_summary(m)
summary_df <- get_trim_summary(m, format = "data.frame")
cat(get_trim_summary(m, format = "text"))
#> Model Trimming Summary
#> ======================
#> sets: 6/27 trimmed (22.2%), 21 remaining
#> parameters: 1/218 trimmed (0.5%), 217 remaining
#> mappings: 160/324 trimmed (49.4%), 164 remaining
#> variables: 10/93 trimmed (10.8%), 83 remaining
#> equations: 103/153 trimmed (67.3%), 50 remaining
```
