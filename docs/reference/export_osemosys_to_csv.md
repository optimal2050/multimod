# Export OSeMOSYS data to CSV files

Export OSeMOSYS data to CSV files

## Usage

``` r
export_osemosys_to_csv(
  osemosys_data,
  output_dir,
  sets_subdir = "sets",
  params_subdir = "parameters",
  verbose = TRUE
)
```

## Arguments

- osemosys_data:

  List returned by read_osemosys_dat()

- output_dir:

  Directory to save CSV files

- sets_subdir:

  Subdirectory for sets (default: "sets")

- params_subdir:

  Subdirectory for parameters (default: "parameters")

- verbose:

  Logical. Print progress?

## Value

Character vector of created files
