#' energyRt Model Source Code (Utopia Test Case)
#'
#' @description
#' Source code for the energyRt Utopia test case model in multiple optimization 
#' modeling languages (GAMS, GMPL, JuMP/Julia, Pyomo). This dataset is used for
#' testing and validating multimod's parser and conversion functions.
#'
#' The energyRt package is an R-based energy systems modeling framework that can
#' generate optimization models in multiple formats. This dataset contains the
#' generated model files from the BASE_UTOPIA scenario, which is a standard test
#' case for energy modeling systems.
#'
#' @format A list with 5 elements:
#' \describe{
#'   \item{gams}{List with model and data components in GAMS format
#'     \itemize{
#'       \item \code{model}: Character vector containing the complete GAMS model
#'             code with all include files resolved
#'       \item \code{data}: Character vector containing the GAMS data statements
#'             with all include files resolved
#'     }
#'   }
#'   \item{gmpl}{List with model and data components in GMPL/MathProg format
#'     \itemize{
#'       \item \code{model}: Character vector containing the GMPL model code
#'       \item \code{data}: Character vector containing the GMPL data statements
#'     }
#'   }
#'   \item{jump}{List with model and data components in JuMP/Julia format
#'     \itemize{
#'       \item \code{model}: Character vector containing the JuMP model code
#'             with all include files resolved
#'       \item \code{data}: Character vector containing Julia data definitions
#'     }
#'   }
#'   \item{pyomo}{List with model and data components in Pyomo/Python format
#'     \itemize{
#'       \item \code{model}: Character vector containing the Pyomo model code
#'       \item \code{data}: Character vector containing Python data definitions
#'     }
#'   }
#'   \item{metadata}{List containing information about the source
#'     \itemize{
#'       \item \code{source}: Package name ("energyRt package")
#'       \item \code{scenario}: Scenario name ("BASE_UTOPIA")
#'       \item \code{repository}: GitHub repository URL
#'       \item \code{license}: License type ("MIT")
#'       \item \code{date_imported}: Date when data was imported
#'       \item \code{note}: Additional information about the dataset
#'     }
#'   }
#' }
#'
#' @details
#' ## Model Structure
#' 
#' Each format contains a complete, self-contained model with all include files
#' resolved and combined into single model and data files. Include directives
#' are replaced with comments showing the original file structure.
#' 
#' ## GAMS Format
#' 
#' The GAMS model includes:
#' - Set declarations and mappings
#' - Parameter definitions
#' - Variable declarations
#' - Equation definitions
#' - Model and solve statements
#' - Output directives
#' 
#' The data file contains all data assignments from the input/ directory.
#' 
#' ## GMPL Format
#' 
#' The GMPL/MathProg format is compatible with GLPK and follows the AMPL syntax.
#' The model file (.mod) contains declarations and the data file (.dat) contains
#' parameter values and set memberships.
#' 
#' ## JuMP Format
#' 
#' The JuMP format is Julia-based and uses the JuMP.jl optimization modeling
#' language. The model includes variable declarations, constraint definitions,
#' and objective function specification.
#' 
#' ## Pyomo Format
#' 
#' The Pyomo format is Python-based and uses the Pyomo optimization modeling
#' framework. It follows an object-oriented structure with ConcreteModel or
#' AbstractModel classes.
#' 
#' ## Utopia Test Case
#' 
#' The Utopia model is a simple energy system with:
#' - Multiple energy carriers (electricity, fossil fuels, renewables)
#' - Supply technologies and resources
#' - Conversion technologies
#' - Storage options
#' - Demand profiles
#' - Multiple time slices
#' - Investment and operational decisions
#' 
#' This makes it an ideal test case for validating optimization model parsers
#' and converters.
#'
#' @section Usage:
#' 
#' To load the dataset:
#' ```r
#' data(energyRt_source)
#' ```
#' 
#' To extract model code for a specific format:
#' ```r
#' # Get GAMS model
#' gams_model <- energyRt_source$gams$model
#' gams_data <- energyRt_source$gams$data
#' 
#' # Write to temporary files for testing
#' tmp_model <- tempfile(fileext = ".gms")
#' tmp_data <- tempfile(fileext = ".gms")
#' writeLines(gams_model, tmp_model)
#' writeLines(gams_data, tmp_data)
#' 
#' # Parse with multimod
#' # model <- read_gams(tmp_model)
#' ```
#' 
#' To compare formats:
#' ```r
#' # Compare line counts
#' sapply(energyRt_source[1:4], function(x) sapply(x, length))
#' 
#' # Check metadata
#' str(energyRt_source$metadata)
#' ```
#'
#' @section Deprecation Notice:
#' This dataset is deprecated. Please use \code{\link{example_models}} instead:
#' \preformatted{
#' data(example_models)
#' energyRt_gams <- example_models$energyRt$gams$model
#' energyRt_gmpl <- example_models$energyRt$gmpl$model
#' energyRt_model <- example_models$energyRt$multimod
#' }
#'
#' @examples
#' \dontrun{
#' # Load the dataset (DEPRECATED - use multimod_examples instead)
#' data(energyRt_source)
#' 
#' # Inspect structure
#' names(energyRt_source)
#' str(energyRt_source, max.level = 2)
#' 
#' # Get line counts for each format
#' lapply(energyRt_source[1:4], function(fmt) {
#'   list(
#'     model_lines = length(fmt$model),
#'     data_lines = length(fmt$data)
#'   )
#' })
#' 
#' # RECOMMENDED: Use new unified dataset
#' data(example_models)
#' model_gams <- read_gams(
#'   file_or_text = example_models$energyRt$gams$model,
#'   include = FALSE
#' )
#' model_gmpl <- read_gmpl(
#'   model_file = example_models$energyRt$gmpl$model,
#'   data_file = example_models$energyRt$gmpl$data
#' )
#' 
#' # OLD WAY (still works but deprecated):
#' # Parse GAMS model directly from memory (no temp files needed!)
#' model_gams <- read_gams(
#'   file_or_text = energyRt_source$gams$model,
#'   include = FALSE  # Already expanded
#' )
#' 
#' # Parse GMPL model directly from memory
#' model_gmpl <- read_gmpl(
#'   model_file = energyRt_source$gmpl$model,
#'   data_file = energyRt_source$gmpl$data
#' )
#' 
#' # View first few lines
#' head(energyRt_source$gams$model, 20)
#' 
#' # Write to files if needed for external tools
#' tmp_dir <- tempdir()
#' writeLines(energyRt_source$gmpl$model, file.path(tmp_dir, "utopia.mod"))
#' writeLines(energyRt_source$gmpl$data, file.path(tmp_dir, "utopia.dat"))
#' 
#' # Compare GAMS and GMPL versions
#' gams_sets <- grep("^sets$|^set ", energyRt_source$gams$model, 
#'                   ignore.case = TRUE, value = TRUE)
#' gmpl_sets <- grep("^set ", energyRt_source$gmpl$model, 
#'                   ignore.case = TRUE, value = TRUE)
#' 
#' cat("GAMS sets found:", length(gams_sets), "\n")
#' cat("GMPL sets found:", length(gmpl_sets), "\n")
#' }
#'
#' @source
#' Generated from energyRt package BASE_UTOPIA scenario
#' \url{https://github.com/energyRt/energyRt}
#' 
#' @section License:
#' The energyRt package and this derived dataset are licensed under the GNU Affero 
#' General Public License v3.0 (AGPL-3.0). This is a copyleft license that requires
#' derivative works to also be open source under AGPL-3.0 or compatible license.
#' 
#' Note: multimod itself is licensed under MIT, but this included energyRt data
#' is AGPL-3.0. When using this dataset, you must comply with AGPL-3.0 terms.
#' 
#' @seealso
#' \itemize{
#'   \item \code{\link{osemosys_source}} for OSeMOSYS GMPL source code
#'   \item \code{\link{read_gams}} for parsing GAMS models
#'   \item \code{\link{read_gmpl}} for parsing GMPL models
#'   \item \code{\link{read_jump}} for parsing JuMP models
#' }
#' 
#' @references
#' energyRt package: \url{https://github.com/energyRt/energyRt}
#' 
#' @keywords datasets
"energyRt_source"
