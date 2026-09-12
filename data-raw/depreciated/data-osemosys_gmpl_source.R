#' OSeMOSYS GMPL Source Code
#'
#' Source code for the OSeMOSYS energy system model in GMPL (GNU MathProg Language)
#' format, including the Utopia test case data. Used for testing and validating
#' multimod's GMPL parser and model conversion capabilities.
#'
#' @format A list with 3 elements:
#' \describe{
#'   \item{model}{Character vector containing the OSeMOSYS model code (osemosys.txt).
#'     Each element is one line of the GMPL model file.}
#'   \item{data}{Character vector containing the Utopia test case data (utopia.txt).
#'     Each element is one line of the GMPL data file.}
#'   \item{metadata}{List containing:
#'     \describe{
#'       \item{source}{Source repository name}
#'       \item{repository}{GitHub repository URL}
#'       \item{license}{License type (Apache-2.0)}
#'       \item{model_file}{Relative path to model file in source repository}
#'       \item{data_file}{Relative path to data file in source repository}
#'       \item{version}{OSeMOSYS version (if available)}
#'       \item{date_imported}{Date when data was imported}
#'       \item{note}{Description of the dataset}
#'     }
#'   }
#' }
#'
#' @source OSeMOSYS GNU MathProg repository
#'   \url{https://github.com/OSeMOSYS/OSeMOSYS_GNU_MathProg}
#'
#' @details
#' OSeMOSYS (Open Source energy MOdeling SYStem) is a full-fledged systems
#' optimization model for long-run energy planning. It has been employed to
#' develop energy systems models from the scale of the globe, continents, countries,
#' regions, and villages. OSeMOSYS is extremely flexible and can be applied to
#' very different energy systems and research questions.
#'
#' The Utopia test case is a simple energy system with 2 regions, 3 time slices,
#' and several technologies, used for testing and validation purposes.
#'
#' This dataset is included under the Apache 2.0 License. See LICENSE.note in
#' the package documentation for full license text.
#'
#' @section License:
#' OSeMOSYS is distributed under the Apache License, Version 2.0.
#' See \url{http://www.apache.org/licenses/LICENSE-2.0} for details.
#'
#' @section Citation:
#' If you use this dataset in your research, please cite:
#'
#' Howells, M., Rogner, H., Strachan, N., Heaps, C., Huntington, H., Kypreos, S.,
#' Hughes, A., Silveira, S., DeCarolis, J., Bazillian, M., & Roehrl, A. (2011).
#' OSeMOSYS: The Open Source Energy Modeling System: An introduction to its ethos,
#' structure and development. Energy Policy, 39(10), 5850–5870.
#' \doi{10.1016/j.enpol.2011.06.033}
#'
#' @references
#' \itemize{
#'   \item OSeMOSYS Website: \url{http://www.osemosys.org/}
#'   \item GitHub Repository: \url{https://github.com/OSeMOSYS/OSeMOSYS_GNU_MathProg}
#'   \item Documentation: \url{https://osemosys.readthedocs.io/}
#' }
#'
#' @section Deprecation Notice:
#' This dataset is deprecated. Please use \code{\link{example_models}} instead:
#' \preformatted{
#' data(example_models)
#' osemosys_model <- example_models$OSeMOSYS$gmpl$model
#' osemosys_data <- example_models$OSeMOSYS$gmpl$data
#' }
#'
#' @examples
#' # Load OSeMOSYS source (DEPRECATED - use multimod_examples instead)
#' data(osemosys_source)
#'
#' # View metadata
#' str(osemosys_source$metadata)
#'
#' # Count lines
#' length(osemosys_source$gmpl$model)  # Model lines
#' length(osemosys_source$gmpl$data)   # Data lines
#'
#' # Parse directly from memory (no temp files needed!)
#' model <- read_gmpl(
#'   model_file = osemosys_source$gmpl$model,
#'   data_file = osemosys_source$gmpl$data
#' )
#'
#' # View model summary
#' summary(model)
#'
#' # Write to temporary files only if needed for external tools
#' \dontrun{
#' model_file <- tempfile(fileext = ".mod")
#' data_file <- tempfile(fileext = ".dat")
#'
#' writeLines(osemosys_source$gmpl$model, model_file)
#' writeLines(osemosys_source$gmpl$data, data_file)
#'
#' # Use with external solvers
#' system2("glpsol", args = c("-m", model_file, "-d", data_file))
#' }
#'
"osemosys_source"
