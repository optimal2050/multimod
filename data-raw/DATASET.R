## Example Models Dataset
##
## Builds `example_models` from the CURRENT energyRt sources:
##   structure : energyRt/gams/energyRt.gms   (GAMS is the source of truth;
##               the GMPL/JuMP/Pyomo templates are still being edited)
##   data      : energyRt::interpolate_model() on the UTOPIA R7 kit
##
## Structure:
##   example_models$energyRt
##     $gams      - list(model = <character>) GAMS template source
##     $multimod  - parsed multimod model with energyRt data attached
##     $metadata  - provenance
##
## The OSeMOSYS workstream was moved to drafts/ (2026-09-08).
## Older datasets are in data-raw/depreciated/ for reference.
##
## Usage:  pkgload::load_all("."); source("data-raw/DATASET.R")

devtools::load_all(".", quiet = TRUE)
source("data-raw/build_energyRt_fixture.R")

KIT <- "R7"

m <- build_energyRt_fixture(KIT)

energyRt_entry <- list(
  gams = list(model = readLines(ENERGYRT_GMS, warn = FALSE)),
  multimod = m,
  metadata = list(
    source           = "energyRt UTOPIA kit",
    scenario         = paste0(KIT, " / utopia_seasons / base horizon"),
    repository       = "https://github.com/optimal2050/energyRt",
    license          = "AGPL-3.0",
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    gams_source      = "gams/energyRt.gms",
    date_imported    = Sys.Date(),
    note             = paste(
      "Model structure parsed from energyRt's GAMS template with",
      "read_gams(include = FALSE); data attached from an interpolated",
      "energyRt scenario. See data-raw/build_energyRt_fixture.R."
    )
  )
)

example_models <- list(energyRt = energyRt_entry)

usethis::use_data(example_models, overwrite = TRUE)
