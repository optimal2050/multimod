#' Example Models in Multiple Formats
#'
#' @description
#' A comprehensive collection of example energy system models in multiple optimization
#' modeling languages (GAMS, GMPL, JuMP/Julia), along with their parsed multimod 
#' representations. Includes both energyRt and OSeMOSYS test cases for testing 
#' and validating multimod's parser and conversion functions.
#'
#' @format A list with 2 main elements:
#' \describe{
#'   \item{energyRt}{energyRt Utopia test case with components:
#'     \describe{
#'       \item{gams}{List with \code{model} and \code{data} as character vectors (GAMS format)}
#'       \item{gmpl}{List with \code{model} and \code{data} as character vectors (GMPL format)}
#'       \item{jump}{List with \code{model} and \code{data} as character vectors (JuMP/Julia format)}
#'       \item{multimod}{Parsed multimod_model object with populated data}
#'       \item{metadata}{List with source, scenario, repository, license, date_imported, note}
#'     }
#'   }
#'   \item{OSeMOSYS}{OSeMOSYS Utopia test case with components:
#'     \describe{
#'       \item{gmpl}{List with \code{model} and \code{data} as character vectors (GMPL format)}
#'       \item{multimod}{Parsed multimod_model object with populated data}
#'       \item{metadata}{List with source, repository, license, model_file, data_file, version, date_imported, note}
#'     }
#'   }
#' }
#'
#' @details
#' ## Structure
#' 
#' Each example model contains:
#' - **Source code** in one or more formats (character vectors, ready to parse)
#' - **Parsed multimod object** (already converted, ready to use)
#' - **Metadata** about the source, license, and provenance
#' 
#' All source code character vectors can be used directly with `read_gams()`, 
#' `read_gmpl()`, etc. without writing temporary files.
#' 
#' ## energyRt Test Case
#' 
#' The energyRt package is an R-based energy systems modeling framework. The 
#' BASE_UTOPIA scenario is a simple 2-region, 3-timeslice test case used for
#' validation. Available in GAMS, GMPL, and JuMP formats.
#' 
#' **License**: GNU Affero General Public License v3.0 (AGPL-3.0)
#' 
#' ## OSeMOSYS Test Case
#' 
#' OSeMOSYS (Open Source energy MOdeling SYStem) is a full-fledged systems
#' optimization model for long-run energy planning. The Utopia test case is
#' a simple energy system used for testing and validation. Available in GMPL format.
#' 
#' **License**: Apache License 2.0
#' 
#' @section Usage:
#' 
#' ```r
#' # Load dataset
#' data(example_models)
#' 
#' # Access energyRt model
#' energyRt_gams <- example_models$energyRt$gams$model
#' energyRt_model <- example_models$energyRt$multimod
#' 
#' # Access OSeMOSYS model
#' osemosys_gmpl <- example_models$OSeMOSYS$gmpl$model
#' osemosys_model <- example_models$OSeMOSYS$multimod
#' ```
#'
#' @source
#' - energyRt: \url{https://github.com/energyRt/energyRt}
#' - OSeMOSYS: \url{https://github.com/OSeMOSYS/OSeMOSYS_GNU_MathProg}
#' 
#' @seealso
#' \itemize{
#'   \item \code{\link{read_gams}} for parsing GAMS models
#'   \item \code{\link{read_gmpl}} for parsing GMPL models
#'   \item \code{\link{read_jump}} for parsing JuMP models
#'   \item \code{\link{as_multimod}} for converting model structures to multimod
#' }
#' 
#' @references
#' energyRt package: \url{https://github.com/energyRt/energyRt}
#' 
#' Howells, M., Rogner, H., Strachan, N., Heaps, C., Huntington, H., Kypreos, S.,
#' Hughes, A., Silveira, S., DeCarolis, J., Bazillian, M., & Roehrl, A. (2011).
#' OSeMOSYS: The Open Source Energy Modeling System: An introduction to its ethos,
#' structure and development. Energy Policy, 39(10), 5850–5870.
#' \doi{10.1016/j.enpol.2011.06.033}
#' 
#' @examples
#' # Load examples
#' data(example_models)
#' 
#' # Inspect structure
#' names(example_models)
#' names(example_models$energyRt)
#' names(example_models$OSeMOSYS)
#' 
#' # Check metadata
#' example_models$energyRt$metadata
#' example_models$OSeMOSYS$metadata
#' 
#' # Use pre-parsed multimod models
#' summary(example_models$energyRt$multimod)
#' summary(example_models$OSeMOSYS$multimod)
#' 
#' # Parse from source (no temp files needed!)
#' \dontrun{
#' # Parse energyRt GAMS source
#' model_gams <- read_gams(
#'   file_or_text = example_models$energyRt$gams$model,
#'   include = FALSE
#' )
#' 
#' # Parse energyRt GMPL source
#' model_gmpl <- read_gmpl(
#'   model_file = example_models$energyRt$gmpl$model,
#'   data_file = example_models$energyRt$gmpl$data
#' )
#' 
#' # Parse OSeMOSYS GMPL source
#' model_osemosys <- read_gmpl(
#'   model_file = example_models$OSeMOSYS$gmpl$model,
#'   data_file = example_models$OSeMOSYS$gmpl$data
#' )
#' 
#' # Compare line counts
#' sapply(example_models$energyRt[c("gams", "gmpl", "jump")], 
#'        function(x) list(model = length(x$model), data = length(x$data)))
#' 
#' # Export to different formats
#' write_gmpl(example_models$energyRt$multimod, "tmp/energyrt")
#' write_jump(example_models$OSeMOSYS$multimod, "tmp/osemosys")
#' write_pyomo(example_models$energyRt$multimod, "tmp/energyrt_pyomo")
#' }
#' 
#' @keywords datasets
"example_models"
