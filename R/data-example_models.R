#' Example Model in Multiple Formats
#'
#' @description
#' An example energy system model in several optimization modeling languages
#' (GAMS, GMPL, JuMP/Julia), along with its parsed multimod representation.
#' Used for testing and validating multimod's parsers and conversion functions.
#'
#' @format A list with 1 element:
#' \describe{
#'   \item{energyRt}{energyRt Utopia test case with components:
#'     \describe{
#'       \item{gams}{List with \code{model}: energyRt's GAMS template source
#'         (\code{gams/energyRt.gms}) as a character vector, ready for
#'         \code{read_gams(include = FALSE)}}
#'       \item{multimod}{Parsed multimod model object with energyRt data attached}
#'       \item{metadata}{List with source, scenario, repository, license,
#'         energyRt_version, gams_source, date_imported, note}
#'     }
#'   }
#' }
#'
#' @details
#' ## Structure
#'
#' The example contains:
#' - **Source code** in several formats (character vectors, ready to parse)
#' - **Parsed multimod object** (already converted, ready to use)
#' - **Metadata** about the source, license, and provenance
#'
#' All source code character vectors can be used directly with `read_gams()`,
#' `read_gmpl()`, etc. without writing temporary files.
#'
#' ## energyRt Test Case
#'
#' The energyRt package is an R-based energy systems modeling framework. The
#' UTOPIA R7 kit is a small 7-region, multi-timeslice test case used for
#' validation.
#'
#' The model *structure* is parsed from energyRt's GAMS template with
#' \code{read_gams(include = FALSE)}; GAMS is the source of truth because the
#' GMPL/JuMP/Pyomo templates are still evolving. The *data* is attached from an
#' interpolated energyRt scenario. See \code{data-raw/build_energyRt_fixture.R}.
#'
#' **License**: GNU Affero General Public License v3.0 (AGPL-3.0). This applies
#' to the bundled model source, not to multimod itself. See `LICENSE.note`.
#'
#' @section Usage:
#'
#' ```r
#' data(example_models)
#'
#' energyRt_gams  <- example_models$energyRt$gams$model
#' energyRt_model <- example_models$energyRt$multimod
#' ```
#'
#' @source
#' - energyRt: \url{https://github.com/energyRt/energyRt}
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{read_gams}} for parsing GAMS models
#'   \item \code{\link{read_gmpl}} for parsing GMPL models
#'   \item \code{\link{as_multimod}} for converting model structures to multimod
#' }
#'
#' @references
#' energyRt package: \url{https://github.com/energyRt/energyRt}
#'
#' @examples
#' # Load examples
#' data(example_models)
#'
#' # Inspect structure
#' names(example_models)
#' names(example_models$energyRt)
#'
#' # Check metadata
#' example_models$energyRt$metadata
#'
#' # Use the pre-parsed multimod model
#' summary(example_models$energyRt$multimod)
#'
#' \dontrun{
#' # Parse from source (no temp files needed)
#' model_gams <- read_gams(
#'   file_or_text = example_models$energyRt$gams$model,
#'   include = FALSE
#' )
#'
#' # Export to different formats
#' write_gmpl(example_models$energyRt$multimod, "tmp/energyrt")
#' write_pyomo(example_models$energyRt$multimod, "tmp/energyrt_pyomo")
#' }
#'
#' @keywords datasets
"example_models"
