# Julia source of the JuMP solution writer

Returns the Julia code of the
`save_solution(model, output_dir, data_format)` helper that is embedded
in the generated `model.jl` and called after solving.

## Usage

``` r
write_jump_solution_code()
```

## Value

A character string with Julia source code.

## Details

The helper is called from within the generated model.jl file. It saves
variable values in the same directory structure as input data.

## Examples

``` r
# Julia source of the save_solution() helper embedded in generated model.jl
cat(write_jump_solution_code())
#> 
#> # Function to save solution values
#> function save_solution(model, output_dir, data_format)
#>     using Dates
#>     using DataFrames
#>     using CSV
#>     using Arrow
#>     
#>     if !isdir(output_dir)
#>         mkpath(output_dir)
#>     end
#>     
#>     println("\nSaving solution to: ", output_dir)
#>     
#>     # Determine file extension
#>     ext = data_format == "csv" ? ".csv" : ".arrow"
#>     
#>     # Save each variable
#>     for (var_name, var_obj) in object_dict(model)
#>         if !(var_obj isa AbstractArray{VariableRef})
#>             continue
#>         end
#>         
#>         # Get variable values
#>         var_vals = value.(var_obj)
#>         
#>         # Convert to DataFrame based on dimensions
#>         if var_obj isa DenseAxisArray
#>             # Get indices
#>             indices = axes(var_obj)
#>             n_dims = length(indices)
#>             
#>             if n_dims == 0
#>                 # Scalar variable
#>                 df = DataFrame(value = [var_vals])
#>             else
#>                 # Multi-dimensional variable
#>                 idx_names = [Symbol("dim$i") for i in 1:n_dims]
#>                 rows = []
#>                 
#>                 for idx in Iterators.product(indices...)
#>                     val = var_vals[idx...]
#>                     if abs(val) > 1e-10  # Only save non-zero values
#>                         push!(rows, (idx..., val))
#>                     end
#>                 end
#>                 
#>                 if !isempty(rows)
#>                     df = DataFrame(rows, vcat(idx_names, :value))
#>                 else
#>                     df = DataFrame()
#>                 end
#>             end
#>         else
#>             # Default handling
#>             df = DataFrame(value = [var_vals])
#>         end
#>         
#>         # Save to file
#>         if !isempty(df)
#>             var_dir = joinpath(output_dir, string(var_name))
#>             mkpath(var_dir)
#>             out_file = joinpath(var_dir, "data" * ext)
#>             
#>             if ext == ".csv"
#>                 CSV.write(out_file, df)
#>             else
#>                 Arrow.write(out_file, df)
#>             end
#>             
#>             println("  Saved ", string(var_name), ": ", nrow(df), " values")
#>         end
#>     end
#>     
#>     println("Solution saved successfully\n")
#> end
#> 
#> # Save solution after solving
#> if termination_status(model) == MOI.OPTIMAL || 
#>    termination_status(model) == MOI.LOCALLY_SOLVED
#>     
#>     run_name = Dates.format(now(), "yyyy-mm-dd_HHMMSS")
#>     solution_dir = joinpath(@__DIR__, "..", "..", "solutions", run_name)
#>     save_solution(model, solution_dir, DATA_FORMAT)
#> end
```
