#' UTOPIA Test Model (DEPRECATED)
#'
#' @description
#' **This dataset is deprecated.** Please use \code{\link{example_models}} instead:
#' \preformatted{
#' data(example_models)
#' utopia_multimod <- example_models$energyRt$multimod
#' }
#'
#' A complete UTOPIA energy system model imported from energyRt with all data
#' loaded in memory. This dataset is ready for testing and examples without
#' requiring external GAMS files or data loading.
#'
#' @format A multimod model object (list) with the following components:
#' \describe{
#'   \item{sets}{List of 13 sets defining model dimensions (comm, region, year, slice, etc.)}
#'   \item{parameters}{List of 151 parameters with data pre-loaded in memory}
#'   \item{equations}{List of 108 equations defining model constraints}
#'   \item{variables}{List of model decision variables}
#'   \item{mappings}{List of 234 mapping relationships between sets}
#'   \item{metadata}{Model metadata including:
#'     \itemize{
#'       \item \code{name}: "UTOPIA"
#'       \item \code{description}: Model description
#'       \item \code{source}: "energyRt package BASE_UTOPIA scenario"
#'       \item \code{created}: Creation timestamp
#'       \item \code{parameters_with_data}: Number of parameters with data (typically ~94)
#'       \item \code{total_parameters}: Total number of parameters (151)
#'       \item \code{sets_populated}: Number of sets with members (typically 11)
#'       \item \code{total_sets}: Total number of sets (13)
#'     }
#'   }
#' }
#'
#' @details
#' The UTOPIA model represents a simple energy system with:
#' \itemize{
#'   \item 14 commodities (electricity, coal, gas, etc.)
#'   \item 7 regions
#'   \item 3 time periods (years)
#'   \item 17 time slices
#'   \item 6 technologies
#'   \item 1 storage technology
#'   \item 8 trade routes
#' }
#'
#' All parameter data is stored in memory (\code{inMemory = TRUE}), making it
#' fast to load and suitable for unit tests, examples, and vignettes.
#'
#' @section Deprecation Notice:
#' This standalone dataset is deprecated. Use the unified \code{\link{example_models}}
#' dataset instead, which includes both energyRt and OSeMOSYS examples:
#' \preformatted{
#' # New recommended way
#' data(example_models)
#' utopia_multimod <- example_models$energyRt$multimod
#' 
#' # Or access other formats
#' gams_code <- example_models$energyRt$gams$model
#' gmpl_code <- example_models$energyRt$gmpl$model
#' }
#'
#' @section Usage:
#' ```
#' # DEPRECATED
#' data(utopia_multimod)
#' 
#' # RECOMMENDED
#' data(example_models)
#' utopia_multimod <- example_models$energyRt$multimod
#' 
#' # Ready to use immediately
#' write_gmpl(utopia_multimod, file = "utopia.mod")
#' 
#' # Test folding
#' fold_spec <- create_fold_spec(utopia_multimod, 
#'                                fold_dims = list(slice = list(tech = "mTechSlice")))
#' model_folded <- fold_model(utopia_multimod, fold_spec = fold_spec)
#' ```
#'
#' @source
#' Created from energyRt package BASE_UTOPIA scenario using
#' \code{data-raw/DATASET.R}
#'
#' @seealso \code{\link{example_models}} for the current unified dataset
#'
#' @examples
#' \dontrun{
#' # DEPRECATED - Load the model directly
#' data(utopia_multimod)
#' 
#' # RECOMMENDED - Load from unified examples
#' data(example_models)
#' utopia_multimod <- example_models$energyRt$multimod
#' 
#' # Inspect model structure
#' length(utopia_multimod$sets)          # 13 sets
#' length(utopia_multimod$parameters)    # 151 parameters
#' length(utopia_multimod$equations)     # 108 equations
#' 
#' # Check metadata
#' utopia_multimod$metadata$name         # "UTOPIA"
#' utopia_multimod$metadata$parameters_with_data  # ~94
#' 
#' # Export to GMPL
#' write_gmpl(utopia_multimod, file = "utopia.mod", include_solve = TRUE)
#' 
#' # Test dimension folding
#' fold_spec <- create_fold_spec(
#'   utopia_multimod,
#'   fold_dims = list(slice = list(tech = "mTechSlice"))
#' )
#' model_folded <- fold_model(utopia_multimod, fold_spec = fold_spec)
#' }
"utopia_multimod"
