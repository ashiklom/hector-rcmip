library(hector.rcmip)
library(tidyverse)

rcmip <- rcmip_inputs()
model <- rcmip$Model[1]
scenario <- rcmip$Scenario[1]
basedon <- "rcp60"

create_scenario <- function(model, scenario, basedon = "rcp45") {

  inputs <- rcmip_inputs() %>%
    filter(model == Model, scenario == Scenario)
  stopifnot(nrow(inputs) > 0)

  base_ini <- system.file("input", paste0("hector_", basedon, ".ini"),
                          package = "hector") %>%
    hectortools::read_ini()

  # Fossil fuel emissions
  ffi_in <- inputs %>%
    filter(Variable == "Emissions|CO2|Fossil and Industrial")
  if (nrow(ffi_in) > 0) {
    base_ini$simpleNbox$ffi_emissions <- ffi_in %>%
      # TODO: More accurate molar mass
      mutate(ffi_emissions = udunits2::ud.convert(value / 44, "Mt", "Pg")) %>%
      select(date = year, ffi_emissions)
  } else {
    message("Missing FFI emissions. Using defaults from: ", basedon)
  }

  # LUC emissions
  luc_in <- inputs %>%
    filter(Variable == "Emissions|CO2|AFOLU")
  if (nrow(luc_in) > 0) {
    base_ini$simpleNbox$luc_emissions <- luc_in %>%
      # TODO: More accurate molar mass
      mutate(luc_emissions = udunits2::ud.convert(value / 44, "Mt", "Pg")) %>%
      select(date = year, luc_emissions)
  } else {
    message("Missing LUC emissions. Using defaults from: ", basedon)
  }
 
  # Albedo change forcing
  alb_in <- inputs %>%
    filter(Variable == "Radiative Forcing|Anthropogenic|Albedo Change")
  if (nrow(alb_in) > 0) {
    base_ini$simpleNbox$Ftalbedo <- alb_in %>%
      select(date = year, Ftalbedo = value)
  } else {
    message("Missing Ftalbedo. Using defaults from: ", basedon)
  }

  udunits2::ud.convert(1, "Mt", "Gg")

  # SO2 emissions
  so2_in <- inputs %>%
    filter(Variable == "Emissions|Sulfur")
  if (nrow(so2_in) > 0) {
    base_ini$
    base_ini$so2$SO2_emissions <- so2_in %>%
      mutate(SO2_emissions = udunits2::ud.convert(value, "Mt", "Gg")) %>%
      select(date = year, SO2_emissions)
  } else {
    message("Missing SO2 emissions. Using defaults from: ", basedon)
  }

  # Volcanic forcing
  rf_vol <- inputs %>%
    filter(Variable == "Radiative Forcing|Natural|Volcanic")
  if (nrow(rf_vol) > 0) {
    base_ini$so2$SV <- rf_vol %>%
      select(date = year, SV = value)
  } else {
    message("Missing SV. Using defaults from: ", basedon)
  }

  # CH4_emissions
  ch4_in <- inputs %>%
    filter(Variable == "Emissions|CH4")
  if (nrow(ch4_in) > 0) {
    base_ini$CH4$CH4_emissions <- ch4_in %>%
      select(date = year, CH4_emissions = value)
  } else {
    message("Missing CH4 emissions. Using defaults from: ", basedon)
  }

  # NOX_emissions
  nox_in <- inputs %>%
    filter(Variable == "NOX_emissions")
  if (nrow(nox_in) > 0) {
    base_ini$OH$NOX_emissions <- nox_in %>%
      # TODO: Figure out NOX conversion factor
      mutate(NOX_emissions = value * 1) %>%
      select(date = year, NOX_emissions)
  } else {
    message("Missing Ftalbedo. Using defaults from: ", basedon)
  }

}
