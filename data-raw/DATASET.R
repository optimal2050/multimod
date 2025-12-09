## Unified Example Models Dataset
##
## This script combines example models from energyRt and OSeMOSYS into a single
## unified dataset called 'example_models'. 
##
## Structure:
##   example_models$energyRt
##     $gams, $gmpl, $jump     - Source code in various formats
##     $multimod               - Parsed multimod object
##     $metadata               - Source information
##
##   example_models$OSeMOSYS
##     $gmpl                   - Source code in GMPL format
##     $multimod               - Parsed multimod object  
##     $metadata               - Source information
##
## Old datasets moved to data-raw/depreciated/ for reference.

# Load updated read_gmpl function
devtools::load_all(".", quiet = TRUE)

(load("data-raw/energyRt_demo.RData"))
(load("data-raw/energyRt_source.RData"))
energyRt_source <- list(
  gams = energyRt_source$gams,
  gmpl = energyRt_source$gmpl,
  jump = energyRt_source$jump,
  multimod = energyRt_demo,
  metadata = energyRt_source$metadata
)


(load("data-raw/osemosys_source.RData"))
mm_osemosys <- multimod::read_gmpl(
  model_file = osemosys_source$gmpl$model,
  data_file = osemosys_source$gmpl$data)

osemosys_source <- list(
  gmpl = osemosys_source$gmpl,
  multimod = mm_osemosys,
  metadata = osemosys_source
)

example_models <- list(
  energyRt = energyRt_source,
  OSeMOSYS = osemosys_source
)

usethis::use_data(example_models, overwrite = TRUE)


