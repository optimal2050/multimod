# Export Model Source Files

Export model source code from package datasets (osemosys_source or
energyRt_source) to files in a specified directory. This is useful for
testing parsers, running models, or inspecting model code.

## Usage

``` r
export_model_source(
  dataset = c("osemosys", "energyrt"),
  dir,
  format = "all",
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  Name of the dataset to export. Either "osemosys" or "energyrt".

- dir:

  Directory path where files will be written. Will be created if it
  doesn't exist.

- format:

  For energyrt dataset, which format to export: "gams", "gmpl", "jump",
  "pyomo", or "all" (default). Ignored for osemosys dataset.

- overwrite:

  Logical. If TRUE, overwrite existing files. Default is FALSE.

- verbose:

  Logical. If TRUE, print progress messages. Default is TRUE.

## Value

Invisible list of file paths that were created.

## Details

### OSeMOSYS Export

For the OSeMOSYS dataset, exports:

- `osemosys.txt`: Model code

- `utopia.txt`: Data file

### energyRt Export

For the energyRt dataset, exports files based on the `format` argument:

- **GAMS** (`format = "gams"`):

  - `model.gms`: Model code with all includes resolved

  - `data.gms`: Data assignments with all includes resolved

  - `README.txt`: Model metadata and information

- **GMPL** (`format = "gmpl"`):

  - `model.mod`: Model declarations

  - `data.dat`: Data statements

  - `README.txt`: Model metadata and information

- **JuMP** (`format = "jump"`):

  - `model.jl`: JuMP model code with includes resolved

  - `data.jl`: Pure Julia data code (no RData dependency)

  - `README.txt`: Model metadata and information

- **Pyomo** (`format = "pyomo"`):

  - `model.py`: Pyomo model code

  - `data.py`: Python data code with exec() resolved

  - `README.txt`: Model metadata and information

- **All** (`format = "all"`):

  - Exports all formats to subdirectories: gams/, gmpl/, jump/, pyomo/

## Examples

``` r
if (FALSE) { # \dontrun{
# Export OSeMOSYS to temporary directory
tmp_dir <- tempdir()
export_model_source("osemosys", file.path(tmp_dir, "osemosys"))

# Export energyRt GMPL format
export_model_source("energyrt", file.path(tmp_dir, "energyrt"),
                    format = "gmpl")

# Export all energyRt formats
export_model_source("energyrt", file.path(tmp_dir, "energyrt_all"),
                    format = "all")

# Use exported files
osemosys_dir <- file.path(tmp_dir, "osemosys")
model <- read_gmpl(file.path(osemosys_dir, "osemosys.txt"))
} # }
```
