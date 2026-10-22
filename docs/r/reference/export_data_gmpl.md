# Generate GMPL code to export all sets and parameters to CSV files

Creates GMPL printf statements that export all sets and parameters in a
model to CSV files in a data_export/ directory using standard CSV format
(comma-separated with headers). This code can be injected into a GMPL
model file before the solve statement.

## Usage

``` r
export_data_gmpl(
  model,
  export_dir = "data_export",
  drop_default_values = FALSE
)
```

## Arguments

- model:

  A model object with \$sets and \$parameters

- export_dir:

  Directory name for exports (default: "data_export")

- drop_default_values:

  logical; if TRUE, exclude values that match the default (default:
  FALSE)

## Value

Character string with GMPL export statements

## Examples

``` r
m <- example_models$energyRt$multimod
export_code <- export_data_gmpl(m)
cat(substr(export_code, 1, 400))
#> # ========== DATA EXPORT ==========
#> printf "Exporting model data to data_export/...\n";
#> 
#> # Export sets
#> printf "comm\n" > "data_export/comm.csv";
#> for {i in comm} { printf "%s\n", i >> "data_export/comm.csv"; }
#> printf "region\n" > "data_export/region.csv";
#> for {i in region} { printf "%s\n", i >> "data_export/region.csv"; }
#> printf "year\n" > "data_export/year.csv";
#> for {i in year} { printf "%s\n", i 
```
