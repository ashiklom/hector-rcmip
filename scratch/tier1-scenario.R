library(hector)
library(tidyverse)
library(hector.rcmip)

all_long <- rcmip_inputs()

tier1 <- tribble(
  ~Scenario, ~driver,
  "piControl", "concentrations",
  "esm-piControl","emissions",
  "1pctCO2","concentrations",
  "1pctCO2-4xext","concentrations",
  "abrupt-4xCO2","concentrations",
  "abrupt-2xCO2","concentrations",
  "abrupt-0p5xCO2","concentrations",
  "historical","concentrations",
  "ssp119","concentrations",
  "ssp585","concentrations",
  "esm-hist","concentrations",
  "esm-ssp119","concentrations",
  "esm-ssp585","concentrations"
)

tier1_inputs <- all_long %>%
  inner_join(tier1, "Scenario")

set_variable <- function(core, input_data, varname, hector_name, hector_unit) {
  invar <- dplyr::filter(input_data, Variable == !!varname)
  invar <- dplyr::arrange(invar, year)
  stopifnot(nrow(invar) > 0)
  year <- invar$year
  unit <- unique(invar$Unit)
  stopifnot(length(unit) == 1)
  value <- udunits2::ud.convert(invar$value, unit, hector_unit)
  # HACK: Assume initial value in input applies from start date to t0. Prevents
  # interpolation errors.
  if (startdate(core) < min(year)) {
    iyear <- seq(startdate(core), min(year) - 1)
    year <- c(iyear, year)
    value <- c(rep(value[1], length(iyear)), value)
  }
  hector::setvar(core, year, hector_name, value, hector_unit)
  invisible(core)
}

scenario <- "piControl"
input <- tier1_inputs

run_concentration_scenario <- function(input, scenario, basedon = "rcp45", vars = NULL) {
  basefile <- system.file("input", paste0("hector_", basedon, ".ini"),
                          package = "hector")
  stopifnot(file.exists(basefile))
  input_sub <- dplyr::filter(input, Scenario == !!scenario)
  stopifnot(nrow(input_sub) > 0)
  dates <- sort(unique(input_sub$year))
  hc <- hector::newcore(basefile, suppresslogging = TRUE, name = scenario)
  # Run to the starting point
  # TODO: This is a hack to prevent interpolation issues. Need a better way of
  # dealing with model start dates
  ## run(hc, min(dates))
  hc <- set_variable(hc, input_sub, "Atmospheric Concentrations|CO2", "Ca_constrain", "ppm")
  hc <- set_variable(hc, input_sub, "Atmospheric Concentrations|CH4", "CH4", "ppb")
  hc <- set_variable(hc, input_sub, "Atmospheric Concentrations|N2O", "N2O", "ppb")
  hc <- set_variable(hc, input_sub, "Atmospheric Concentrations|F-Gases|HFC|HFC125",
                     "HFC125_concentration", "ppt")
  hc <- set_variable(hc, input_sub, "Atmospheric Concentrations|F-Gases|HFC|HFC134a",
                     "HFC134a_concentration", "ppt")
  hc <- set_variable(hc, input_sub, "Atmospheric Concentrations|F-Gases|HFC|HFC143a",
                     "HFC143a_concentration", "ppt")
  # TODO: More halocarbons
  run(hc, max(dates))
  result <- tibble::as_tibble(hector::fetchvars(
    hc, dates, vars, scenario
  ))
  return(result)
}

all_long %>%
  semi_join(tier1, "Scenario") %>%
  filter(Scenario == Scenario[[1]]) %>%
  distinct(Scenario, Variable) %>%
  count(Variable) %>%
  print(n = Inf)

all_long %>%
  filter(Scenario %in% tier1, grepl("Emissions", Variable)) %>%

inputs <- all_long %>%
  filter(Scenario == !!scenario)

inputs %>%
  distinct(Variable) %>%
  print(n = Inf)
