# Write JuMP solution values to disk

Helper function to save solution values from Julia model. Called from
within Julia code after solving.

## Usage

``` r
write_jump_solution_code()
```

## Arguments

- model:

  JuMP model object (from Julia)

- output_dir:

  Directory to save solutions

- format:

  Data format ("arrow" or "csv")

- run_name:

  Name for this solution run (default: timestamp)

## Details

This is typically called from within the generated model.jl file. It
saves variable values in the same directory structure as input data.

## Examples
