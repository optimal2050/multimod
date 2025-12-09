# Load OSeMOSYS data from CSV directory

Load OSeMOSYS data from CSV directory

## Usage

``` r
load_osemosys_from_csv(
  model,
  csv_dir,
  sets_subdir = "sets",
  params_subdir = "parameters",
  verbose = TRUE
)
```

## Arguments

- model:

  Multimod model

- csv_dir:

  Directory containing CSV files

- sets_subdir:

  Subdirectory with set files

- params_subdir:

  Subdirectory with parameter files

- verbose:

  Logical

## Value

Modified model with data loaded
