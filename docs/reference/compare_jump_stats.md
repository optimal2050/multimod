# Compare JuMP model constraint statistics between two model directories

Reads constraint statistics CSV files from two JuMP model solver
directories and compares constraint counts and non-zero counts. Useful
for validating model generation changes.

## Usage

``` r
compare_jump_stats(
  model_dir1,
  model_dir2,
  save_comparison = TRUE,
  verbose = TRUE
)
```

## Arguments

- model_dir1:

  Path to first model's solver/jump directory (e.g.,
  "tmp/model_v1/solvers/jump")

- model_dir2:

  Path to second model's solver/jump directory (e.g.,
  "tmp/model_v2/solvers/jump")

- save_comparison:

  Logical; if TRUE, saves merged comparison to CSV in model_dir1's
  parent directory

- verbose:

  Logical; if TRUE, prints detailed comparison results

## Value

A list with components:

- merged:

  Data frame with merged statistics from both models

- constraint_mismatches:

  Data frame of equations with different constraint counts

- nnz_differences:

  Data frame of equations with different non-zero counts

- summary:

  Named list with total counts and match status

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare two JuMP model versions
comp <- compare_jump_stats(
  "tmp/model_v1/solvers/jump",
  "tmp/model_v2/solvers/jump",
  verbose = TRUE
)

# Check if models match
if (comp$summary$all_match) {
  message("Models are identical!")
}
} # }
```
