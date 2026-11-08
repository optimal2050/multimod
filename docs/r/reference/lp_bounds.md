# Implied bounds of every variable

Propagates the row and column bounds of an LP to a fixed point and
returns the LP with the implied interval of each column, `lo_impl` and
`up_impl`, on its column index. The interval contains every feasible
value of the variable; it is tight only where a single row decides it.

## Usage

``` r
lp_bounds(lp, objective = NULL, max_pass = 50L, tol = 1e-09, verbose = FALSE)
```

## Arguments

- lp:

  An LP from
  [`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.md)
  or
  [`lp_presolve()`](https://optimal2050.github.io/multimod/r/reference/lp_presolve.md).

- objective:

  A known feasible objective value, or `NULL`. When given, the objective
  row `sum c_j x_j <= objective` (`>=` for a maximisation) takes part,
  which bounds variables that rows alone leave open.

- max_pass:

  Passes over the rows; propagation stops earlier when no bound moves by
  more than `tol`.

- tol:

  Minimal improvement of a bound that counts as a change.

- verbose:

  Report each pass.

## Value

The LP with `lo_impl`, `up_impl` on `col_index` and a `bounds` element:
`passes`, `tightened` (columns whose interval shrank), `fixed`
(zero-width intervals), `open` (an infinite end left) and `infeasible`
(a row whose activity range misses its bounds, by index).

## Examples

``` r
lp <- model_to_lp(example_models$toy)
b <- lp_bounds(lp)
b$bounds[c("tightened", "fixed", "open")]
#> $tightened
#> [1] 1
#> 
#> $fixed
#> [1] 0
#> 
#> $open
#> [1] 13
#> 
head(b$col_index[, c("symbol", "lo", "up", "lo_impl", "up_impl")])
#>    symbol    lo    up lo_impl up_impl
#>    <char> <num> <num>   <num>   <num>
#> 1:   vCap     0   Inf       0     Inf
#> 2:   vCap     0   Inf       0     Inf
#> 3:   vCap     0   Inf       0     Inf
#> 4:   vCap     0   Inf       0     Inf
#> 5:   vCap     0   Inf       0     Inf
#> 6:   vCap     0   Inf       0     Inf
```
