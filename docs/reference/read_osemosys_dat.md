# Read OSeMOSYS data files

Functions to import OSeMOSYS data in various formats (Pyomo .dat, AMPL
.dat, etc.) and convert to multimod format. Parse OSeMOSYS .dat file
(Pyomo/AMPL format)

## Usage

``` r
read_osemosys_dat(dat_file, verbose = TRUE)
```

## Arguments

- dat_file:

  Path to .dat file

- verbose:

  Logical. Print progress messages?

## Value

List with sets and parameters as data frames

## Details

Parses OSeMOSYS data files in Pyomo/AMPL format. Handles:

- Set declarations: `set SETNAME := value1 value2 ... ;`

- Parameters with dimensions: `param ParamName := key1 key2 value ;`

- Multi-line continuations

- Comments starting with \#
