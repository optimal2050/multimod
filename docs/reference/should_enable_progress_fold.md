# Decide whether to show a progress bar while folding

Decide whether to show a progress bar while folding

## Usage

``` r
should_enable_progress_fold(
  model,
  fold_spec,
  param_threshold = 30,
  row_threshold = 50000
)
```

## Arguments

- model:

  A multimod model object.

- fold_spec:

  The fold specification.

- param_threshold:

  Parameter count above which to enable progress.

- row_threshold:

  Total data rows above which to enable progress.

## Value

Logical.
