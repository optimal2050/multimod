# Trim a model to the elements its data actually uses

Marks sets, parameters and mappings with no data, then the variables and
equations whose domains are empty as a consequence, so that renderers
skip them. The model keeps every element;
[`untrim_model()`](https://optimal2050.github.io/multimod/reference/untrim_model.md)
restores the marks and
[`get_trim_summary()`](https://optimal2050.github.io/multimod/reference/get_trim_summary.md)
reports what was trimmed.

## Usage

``` r
trim_model(model, strategy = "unused", verbose = TRUE, .progress = "auto")
```

## Arguments

- model:

  A `mm_model` with data attached.

- strategy:

  `"unused"` (the default) trims only elements that cannot take part in
  any equation; `"aggressive"` also trims elements referenced only by
  trimmed equations.

- verbose:

  Print progress messages.

- .progress:

  `"auto"` shows a progress bar (via the progressr package) for large
  models only; `TRUE`/`FALSE` force it on or off.

## Value

The model with trim marks set;
[`is_trimmed()`](https://optimal2050.github.io/multimod/reference/is_trimmed.md)
is `TRUE` for it.

## See also

[`untrim_model()`](https://optimal2050.github.io/multimod/reference/untrim_model.md),
[`get_trim_summary()`](https://optimal2050.github.io/multimod/reference/get_trim_summary.md),
[`fold_model()`](https://optimal2050.github.io/multimod/reference/fold_model.md)

## Examples

``` r
# \donttest{
trimmed <- trim_model(example_models$energyRt$multimod, verbose = FALSE)
is_trimmed(trimmed)
#> [1] TRUE
# }
```
