# cuOpt watchdog settings

Thresholds for stopping a cuOpt (PDLP) solve that diverges or stalls.
The watchdog reads `cuopt_cli`'s iteration log every `poll` seconds and
stops the solve when

- **runaway**: the primal or dual objective stays beyond `runaway` in
  absolute value for `runaway_n` consecutive log lines (cuOpt logs every
  1,000 iterations), or

- **stall**: over the last `stall_window` seconds, or the last
  `stall_iter` iterations, none of the primal residual, the dual
  residual and the gap improved on its best value before that window by
  a factor `stall_factor`.

## Usage

``` r
cuopt_watchdog(
  poll = 30,
  runaway = 1e+20,
  runaway_n = 5,
  stall_window = 1800,
  stall_iter = 5e+05,
  stall_factor = 2
)
```

## Arguments

- poll:

  seconds between log reads.

- runaway, runaway_n:

  the runaway rule.

- stall_window, stall_iter, stall_factor:

  the stall rule.

## Value

a `cuopt_watchdog` list.

## Details

A rule whose threshold is `NA` is off. Pass `FALSE` as
`control$watchdog` (or the session option) to turn the watchdog off; it
also stays off when the 'processx' package is not installed.

Precedence: `control$watchdog` of the solve, then
`getOption("multimod.cuopt_watchdog")`, then `cuopt_watchdog()`.

## Examples

``` r
cuopt_watchdog()
#> <cuopt_watchdog>
#>   poll          30
#>   runaway       1e+20
#>   runaway_n     5
#>   stall_window  1800
#>   stall_iter    5e+05
#>   stall_factor  2
# a longer stall window for a slow but healthy model
w <- cuopt_watchdog(stall_window = 3600)
# control$watchdog <- w   (or FALSE to turn it off)
```
