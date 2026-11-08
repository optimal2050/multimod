# Compare JuMP model constraint statistics between two model directories

Reads constraint statistics CSV files from two JuMP model solver
directories and compares constraint counts and non-zero counts. Useful
for validating model generation changes.

## Usage

``` r
compare_jump_stats(model_dir1, model_dir2, file = NULL, verbose = TRUE)
```

## Arguments

- model_dir1, model_dir2:

  The `solvers/jump` directories of the two models (holding
  `constraint_stats.csv`, written by the generated JuMP code), or the
  model workspaces above them.

- file:

  Optional path of a CSV file to write the merged comparison to. `NULL`
  (default) writes nothing.

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
# the statistics files of two JuMP runs; solve_jump() writes them
d1 <- file.path(tempfile("jump1-"), "solvers", "jump")
d2 <- file.path(tempfile("jump2-"), "solvers", "jump")
dir.create(d1, recursive = TRUE); dir.create(d2, recursive = TRUE)
write.csv(data.frame(name = c("eqDemand", "eqCap"), count = c(4, 6),
                     nnz_total = c(8, 12)),
          file.path(d1, "constraint_stats.csv"), row.names = FALSE)
write.csv(data.frame(name = c("eqDemand", "eqCap"), count = c(4, 6),
                     nnz_total = c(8, 14)),
          file.path(d2, "constraint_stats.csv"), row.names = FALSE)

comp <- compare_jump_stats(d1, d2, verbose = FALSE)
comp$nnz_differences
#>    name nnz_total_1 nnz_total_2 nnz_diff
#> 1 eqCap          12          14        2
comp$summary$all_match
#> [1] FALSE
```
