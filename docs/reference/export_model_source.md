# Export Model Source Files

Export model source code from the bundled `example_models` dataset to
files in a specified directory. This is useful for testing parsers,
running models, or inspecting model code.

## Usage

``` r
export_model_source(
  dataset = c("energyrt"),
  dir,
  format = "all",
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  Name of the dataset to export. Currently only "energyrt".

- dir:

  Directory path where files will be written. Will be created if it
  doesn't exist.

- format:

  Which format to export: "gams", "gmpl", "jump", "pyomo", or "all"
  (default).

- overwrite:

  Logical. If TRUE, overwrite existing files. Default is FALSE.

- verbose:

  Logical. If TRUE, print progress messages. Default is TRUE.

## Value

Invisible list of file paths that were created.

## Details

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
tmp_dir <- tempdir()

# Export energyRt GMPL format
export_model_source("energyrt", file.path(tmp_dir, "energyrt"),
                    format = "gmpl")

# Export all energyRt formats
export_model_source("energyrt", file.path(tmp_dir, "energyrt_all"),
                    format = "all")

# Use exported files
gmpl_dir <- file.path(tmp_dir, "energyrt")
model <- read_gmpl(file.path(gmpl_dir, "model.mod"),
                   file.path(gmpl_dir, "data.dat"))
} # }
```
