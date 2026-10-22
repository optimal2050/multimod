# Decide whether to show a progress bar while trimming

Heuristic used by
[`trim_model()`](https://optimal2050.github.io/multimod/r/reference/trim_model.md)
with `.progress = "auto"`: returns `TRUE` for models large enough that
trimming takes noticeable time. Trimming removes "dead branches" by
marking unused sets, parameters, mappings, variables, and equations with
`$trimmed = TRUE`; code generators (e.g., write_jump, write_gmpl) skip
elements marked as trimmed.

## Usage

``` r
should_enable_progress_trim(
  model,
  element_threshold = 100,
  row_threshold = 50000
)
```

## Arguments

- model:

  A multimod model object

- element_threshold:

  Integer. Progress is enabled when the number of sets, parameters,
  mappings, variables and equations exceeds this value (default: 100)

- row_threshold:

  Integer. Progress is enabled when the total number of parameter data
  rows exceeds this value (default: 50000)

## Value

Logical; `TRUE` when a `progressr` progress bar should be shown while
trimming

## Details

Trimming process:

1.  Mark sets, parameters, and mappings with empty data

2.  Mark variables with empty domains

3.  Mark equations with empty domains or all variables trimmed

4.  Untrim any elements still needed by non-trimmed equations

5.  Final pass: mark variables not appearing in any non-trimmed equation

Unlike folding (which reduces dimensions), trimming removes entire
unused elements. Original data is preserved on disk but marked for
exclusion from solver code generation.
