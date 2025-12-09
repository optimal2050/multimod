# Generate data.jl file that loads all data upfront (energyRt.jl compatible)

Generate data.jl file that loads all data upfront (energyRt.jl
compatible)

## Usage

``` r
.generate_data_jl(
  model_dir,
  solver_dir,
  model = NULL,
  use_folded = FALSE,
  shifts_needed = NULL,
  export_data = FALSE,
  data_mode = c("external", "embedded")
)
```
