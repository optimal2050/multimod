# Import OSeMOSYS data into multimod model

Import OSeMOSYS data into multimod model

## Usage

``` r
import_osemosys_data(model, dat_file, name_mapping = NULL, verbose = TRUE)
```

## Arguments

- model:

  Multimod model (from read_gams)

- dat_file:

  Path to OSeMOSYS .dat file

- name_mapping:

  Optional named list mapping OSeMOSYS names to multimod names

- verbose:

  Logical. Print progress?

## Value

Modified model with data attached

## Details

Reads OSeMOSYS .dat file and attaches data to corresponding model
elements. For sets, populates set\$data. For parameters, attaches
parameter\$data.

Name mapping example:

    name_mapping <- list(
      TECHNOLOGY = "tech",
      REGION = "region",
      YEAR = "year"
    )
