# Import GMPL Data to Model

Read a GMPL data file and populate a multimod model's sets and
parameters. This is more efficient for large models as it directly
populates the model and can save to CSV/arrow format.

## Usage

``` r
import_gmpl_data(
  model,
  gmpl_data,
  model_path = NULL,
  format = c("memory", "csv", "arrow"),
  verbose = FALSE
)
```

## Arguments

- model:

  A multimod model object

- gmpl_data:

  Path to GMPL data file, character vector of data file contents, or a
  list from read_gmpl_data()

- model_path:

  Optional path to save parameter data as CSV files (default: model's
  base_path)

- format:

  Data storage format: "memory" (default), "csv", or "arrow"

- verbose:

  Logical. If TRUE, print progress (default FALSE)

## Value

Updated model object with populated sets and parameters

## Examples

``` r
# Read the model structure
model <- read_gmpl(c(
  "set REGION;",
  "param Demand{r in REGION};",
  "var x{r in REGION} >= 0;",
  "minimize cost: sum{r in REGION} x[r];",
  "s.t. meet{r in REGION}: x[r] >= Demand[r];",
  "end;"
), as_multimod = TRUE)

# Import data given as text
data_text <- c(
  "data;",
  "set REGION := north south;",
  "param Demand := north 10 south 5;",
  "end;"
)
model <- import_gmpl_data(model, gmpl_data = data_text)
get_data(model, "Demand")
#>   REGION value
#> 1  north    10
#> 2  south     5

# Or import pre-parsed data, saving it as CSV in a model directory
data <- read_gmpl_data(data_text)
model <- import_gmpl_data(model, gmpl_data = data,
                          model_path = tempfile("gmpl-data-"),
                          format = "csv")
```
