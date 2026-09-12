#' Short index names for the energyRt model
#'
#' Maps each energyRt set (and set alias) to a short iterator name used when
#' rendering equations, e.g. `tech` -> `h`, `timeslice` -> `t`. Consumed by
#' [add_index_aliases()] when building the bundled `example_models` fixture.
#'
#' Names must match the sets and aliases declared in energyRt's GAMS model.
#' As of energyRt 0.89.5 those are: comm, region, year, timeslice, sup, dem,
#' tech, stg, trade, expp, imp, group, weather; plus the alias families
#' tech/techp, region/regionp/src/dst/region2, year/yearp/yeare/yearn/year2,
#' timeslice/timeslicep/timeslicepp/timeslice2, group/groupp,
#' comm/commp/acomm/comme, sup/supp.
#'
#' @keywords internal
#' @noRd
index_aliases_energyRt <- c(
  # base sets
  comm        = "c",   # commodity
  region      = "r",   # region
  year        = "y",   # year
  timeslice   = "t",   # time slice
  sup         = "u",   # supply
  dem         = "d",   # demand
  tech        = "h",   # technology
  stg         = "s",   # storage
  trade       = "z",   # interregional trade
  expp        = "x",   # export to RoW
  imp         = "m",   # import from RoW
  group       = "g",   # group of related commodities or tags
  weather     = "w",   # weather
  # set aliases
  techp       = "hp",
  regionp     = "rp",
  region2     = "r2",
  src         = "rs",
  dst         = "rd",
  yearp       = "yp",
  yeare       = "ye",
  yearn       = "yn",
  year2       = "y2",
  timeslicep  = "tp",
  timeslicepp = "tpp",
  timeslice2  = "t2",
  groupp      = "gp",
  commp       = "cp",
  acomm       = "ca",
  comme       = "ce",
  supp        = "up"
)
