# Trim unused elements from a model

Removes "dead branches" from a model by marking unused sets, parameters,
mappings, variables, and equations with `$trimmed = TRUE`. Code
generators (e.g., write_jump, write_gmpl) will skip elements marked as
trimmed.

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

- strategy:

  Character. Trimming strategy:

  - "unused": Remove empty data and dependent variables/equations
    (default)

  - "aggressive": Also perform graph-based analysis (future)

- verbose:

  Logical. Print progress messages (default: TRUE)

## Value

Model with `$trimmed = TRUE` flag on unused elements

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

## Examples

``` r
if (FALSE) { # \dontrun{
# Typical workflow
model <- fold_model(model)  # Reduce dimensions first
model <- trim_model(model)  # Then remove dead branches
write_jump(model, "model.jl")  # Trimmed elements excluded

# Check what was trimmed
summary <- get_trim_summary(model)
print(summary, format = "text")
} # }
```
