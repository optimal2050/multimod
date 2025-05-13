## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

if (F) {
  # temporary file for testing and example
  library(here)
  devtools::load_all()
  gams_file <- here("tmp/energyRt.gms")
  model_info <- read_gams_model_structure(gams_file, include = FALSE)
  symbols <- build_symbols_list(model_info)
  mmod <- as_multimod(model_info)
  example_model <- list(
    name = "energyRt.gms",
    model_info = model_info,
    multimod = mmod
  )
  usethis::use_data(example_model, overwrite = TRUE)
}
