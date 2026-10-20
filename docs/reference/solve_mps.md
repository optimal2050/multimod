# Solve a written MPS file locally and emit a HiGHS solution file

The fully local counterpart of the cloud solve leg: reads an MPS file
written by
[`write_mps()`](https://optimal2050.github.io/multimod/reference/write_mps.md),
solves it in-process with HiGHS, and writes a raw-style solution file
that
[`read_highs_solution()`](https://optimal2050.github.io/multimod/reference/read_highs_solution.md)
/
[`read_mps_solution()`](https://optimal2050.github.io/multimod/reference/read_mps_solution.md)
consume unchanged. Positional column/row names (`c0`, `r0`, ...) are
reconstructed from the solver dimensions, matching what
[`write_mps()`](https://optimal2050.github.io/multimod/reference/write_mps.md)
emits; files with other naming conventions decode correctly only through
their own index tables.

## Usage

``` r
solve_mps(
  file,
  sol_file = NULL,
  control = list(),
  require_optimal = TRUE,
  verbose = FALSE
)
```

## Arguments

- file:

  Path to the MPS file (uncompressed; the HiGHS reader does not accept
  `.gz`).

- sol_file:

  Path for the solution file; default `file` with a `.sol` extension.

- control:

  Optional list of HiGHS options (e.g.
  `list(solver = "ipm", run_crossover = "on")`).

- require_optimal:

  Logical; error unless the run ends optimal.

- verbose:

  Logical; report progress.

## Value

A list with `status_message`, `objective`, `sol_file` and `run_time`
(seconds).
