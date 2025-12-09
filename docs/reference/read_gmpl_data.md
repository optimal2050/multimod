# Read GMPL data file and parse its contents

Parses a .dat file to extract sets, mappings, and parameters with their
values. Returns a structured list with metadata about each element.

## Usage

``` r
read_gmpl_data(file)
```

## Arguments

- file:

  Path to .dat file, or character vector containing data file lines

## Value

List with elements: sets, mappings, parameters. Each element contains
name, type ("set", "mapping", "parameter"), dims, data, and default
value.
