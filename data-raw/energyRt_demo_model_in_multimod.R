
index_aliases_energyRt <- c(
  comm    = "c",  # commodity
  region  = "r",  # region
  year    = "y",  # year
  slice   = "t",  # time slice
  sup     = "u",  # supply
  dem     = "d",  # demand
  tech    = "h",  # technology
  stg     = "s",  # storage
  trade   = "z",  # interregional trade
  expp    = "x",  # export to ROW
  imp     = "m",  # import from ROW
  weather = "w",  # weather
  # process = "p",  # process
  # aux     = "a",  # auxiliary indicator (e.g. flags, switches)
  # input   = "i",  # input flows to process
  # output  = "o",  # output flows from process
  group   = "g",  # group of related commodities or tags
  # shorts for aliases
  techp   = "hp",
  regionp = "rp",
  region2 = "r2",
  src     = "rs",
  dst     = "rd",
  yearp   = "yp",
  yeare   = "ye",
  yearn   = "yn",
  year2   = "y2",
  slicep  = "tp",
  slicepp = "tpp",
  slice2  = "t2",
  groupp  = "gp",
  commp   = "cp",
  acomm   = "ca",
  comme   = "ce",
  supp    = "up"
)

# if (F) { # old version
#   # temporary file for testing and example
#   library(here)
#   devtools::load_all()
#   gams_file <- here("tmp/energyRt.gms")
#   model_info <- read_gams(gams_file, include = FALSE)
#
#   symbols <- build_symbols_list(model_info)
#   mmod <- as_multimod(model_info)
#   example_model <- list(
#     name = "energyRt.gms",
#     model_info = model_info,
#     multimod = mmod,
#     short_aliases = index_aliases_energyRt
#   )
#   usethis::use_data(example_model, overwrite = TRUE)
# }

## Script to create UTOPIA test data for multimod package
## This creates a complete model with structure and data for testing

# library(energyRt)
devtools::load_all()

cat("=== Creating UTOPIA test data ===\n\n")

# 1. Read GAMS model structure
cat("Step 1: Reading GAMS model structure...\n")
gams_file <- 'dev/scenarios/BASE_UTOPIA/script/gams_cbc/energyRt.gms'
model_struct <- read_gams(gams_file)
cat("\n=== Extract domains from comments (BEFORE as_multimod) ===\n")
model_struct <- en_extract_domains_from_comments(model_struct, verbose = FALSE)
model_struct <- populate_defvals_from_energyrt(model_struct)
model_struct$objectives[[1]]
cat("  ✓ Model structure read\n\n")

# model_struct$objectives
# um <- as_multimod(model_struct)
# um

# 2. Convert to multimod
cat("Step 2: Converting to multimod...\n")
um <- as_multimod(model_struct)
um$objectives[[1]]
um$index_aliases
cat("  ✓ Converted to multimod\n")
cat(sprintf("  Sets: %d\n", length(um$sets)))
cat(sprintf("  Aliases: %d\n", length(um$aliases)))
cat(sprintf("  Parameters: %d\n", length(um$parameters)))
cat(sprintf("  Equations: %d\n", length(um$equations)))
cat(sprintf("  Variables: %d\n\n", length(um$variables)))

# 3. Load and import energyRt scenario data
cat("Step 3: Importing energyRt scenario data...\n")
load('dev/scenarios/BASE_UTOPIA/scen.RData')
um$base_path <- 'dev/scenarios/BASE_UTOPIA'
um <- import_energyRt_data(um, scen, inMemory = TRUE)
cat("  ✓ Data imported\n\n")

# add index aliases
um <- add_index_aliases(um, index_aliases = index_aliases_energyRt, overwrite = T)
um$index_aliases
validate(um, verbose = T)

# 4. Count populated parameters
cat("Step 4: Checking data population...\n")
n_params <- length(um$parameters)
n_with_data <- sum(sapply(um$parameters, function(p) {
  !is.null(p$data) && nrow(p$data) > 0
}))
cat(sprintf("  Parameters with data: %d / %d\n", n_with_data, n_params))

# Count set members
set_sizes <- sapply(um$sets, function(s) {
  if (!is.null(s$members)) length(s$members) else 0
})
cat(sprintf("  Set members populated: %d sets\n", sum(set_sizes > 0)))
cat("\n")

# 5. Add metadata
cat("Step 5: Adding metadata...\n")
um$metadata <- list(
  name = "DEMO",
  description = "DEMO model from energyRt with full data",
  source = "energyRt package BASE_UTOPIA scenario",
  created = Sys.time(),
  parameters_with_data = n_with_data,
  total_parameters = n_params,
  sets_populated = sum(set_sizes > 0),
  total_sets = length(um$sets)
)
cat("  ✓ Metadata added\n\n")

# 6. Save as package data
cat("Step 6: Saving to package data...\n")
energyRt_demo <- um # formally _utopia_multimod_
save(energyRt_demo, file = "data-raw/energyRt_demo.RData")
# usethis::use_data(energyRt_demo, overwrite = TRUE)
cat("  ✓ Saved as data/energyRt_demo.rda\n\n")

cat("=== Summary ===\n")
cat(sprintf("Model name: %s\n", energyRt_demo$metadata$name))
cat(sprintf("Sets: %d (%d populated)\n",
            energyRt_demo$metadata$total_sets,
            energyRt_demo$metadata$sets_populated))
cat(sprintf("Parameters: %d (%d with data)\n",
            energyRt_demo$metadata$total_parameters,
            energyRt_demo$metadata$parameters_with_data))
cat(sprintf("Equations: %d\n", length(energyRt_demo$equations)))
cat(sprintf("Variables: %d\n", length(energyRt_demo$variables)))
cat("\n✓ UTOPIA test data created successfully!\n")
cat("\nUsage:\n")
cat("  data(energyRt_demo)\n")
cat("  # Model is ready with structure and data\n")

cat("=== Creating OSeMOSYS-UTOPIA test data ===\n\n")
