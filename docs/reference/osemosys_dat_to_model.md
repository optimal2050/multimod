# Convert OSeMOSYS .dat to CSV and import into model

Convert OSeMOSYS .dat to CSV and import into model

## Usage

``` r
osemosys_dat_to_model(
  model,
  dat_file,
  csv_dir = NULL,
  name_mapping = NULL,
  verbose = TRUE
)
```

## Arguments

- model:

  Multimod model

- dat_file:

  Path to OSeMOSYS .dat file

- csv_dir:

  Directory to save CSV files (optional)

- name_mapping:

  Optional name mapping

- verbose:

  Logical

## Value

Modified model with data attached

## Details

Convenience function that:

1.  Reads OSeMOSYS .dat file

2.  Optionally exports to CSV

3.  Imports data into model
