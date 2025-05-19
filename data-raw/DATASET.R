## code to prepare `DATASET` dataset goes here

# usethis::use_data(DATASET, overwrite = TRUE)

short_aliases_for_sets <- list(
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
  process = "p",  # process
  aux     = "a",  # auxiliary indicator (e.g. flags, switches)
  input   = "i",  # input flows to process
  output  = "o",  # output flows from process
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
    multimod = mmod,
    short_aliases = short_aliases_for_sets
  )
  usethis::use_data(example_model, overwrite = TRUE)
}
