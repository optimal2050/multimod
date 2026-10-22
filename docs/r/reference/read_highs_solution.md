# Parse a HiGHS solution file

Reads the raw (machine) format written by `Highs.writeSolution(path, 0)`
/ `highs_write_solution(..., style = 0)`. The pretty style (`1`) is a
human table and is not parsed here.

## Usage

``` r
read_highs_solution(file, require_optimal = TRUE)
```

## Arguments

- file:

  Path to the solution file; `.gz` is read transparently, matching how
  large models are transferred.

- require_optimal:

  Logical; error unless the model status is optimal. Set `FALSE` to
  inspect an infeasible or unbounded result.

## Value

A list with `status`, `objective`, and `columns` / `rows` (`data.table`s
of `name`, `primal`, `dual`). `dual` is `NA` when the file carries no
dual section.
