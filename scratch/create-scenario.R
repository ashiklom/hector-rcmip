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

  co2c <- biogas::molMass("CO2") / biogas::molMass("C")

  # Fossil fuel emissions
  ffi_in <- inputs %>%
    filter(Variable == "Emissions|CO2|Fossil and Industrial")
  if (nrow(ffi_in) > 0) {
    base_ini$simpleNbox$ffi_emissions <- ffi_in %>%
      mutate(ffi_emissions = udunits2::ud.convert(value / co2c, "Mt", "Pg")) %>%
      select(date = year, ffi_emissions)
  } else {
    message("Missing FFI emissions. Using defaults from: ", basedon)
  }

  # LUC emissions
  luc_in <- inputs %>%
    filter(Variable == "Emissions|CO2|AFOLU")
  if (nrow(luc_in) > 0) {
    base_ini$simpleNbox$luc_emissions <- luc_in %>%
      mutate(luc_emissions = udunits2::ud.convert(value / co2c, "Mt", "Pg")) %>%
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
  # TODO: Figure out NOX conversion factor
  nox_mass <- (biogas::molMass("NO2") + biogas::molMass("NO")) / 2
  nox2n <- nox_mass / biogas::molMass("N")
  nox_in <- inputs %>%
    filter(Variable == "Emissions|NOx")
  if (nrow(nox_in) > 0) {
    base_ini$OH$NOX_emissions <- nox_in %>%
      mutate(NOX_emissions = value / nox2n) %>%
      select(date = year, NOX_emissions)
    base_ini$ozone$NOX_emissions <- base_ini$OH$NOX_emissions
  } else {
    message("Missing NOX emissions. Using defaults from: ", basedon)
  }
  
  # CO_emissions
  co_in <- inputs %>%
    filter(Variable == "CO_emissions")
  if (nrow(co_in) > 0) {
    base_ini$OH$CO_emissions <- co_in %>%
      select(date = year, CO_emissions = value)
    base_ini$ozone$CO_emissions <- base_ini$OH$CO_emissions
  } else {
    message("Missing CO emissions. Using defaults from: ", basedon)
  }

  # NMVOC_emissions
  nmvoc_in <- inputs %>%
    filter(Variable == "Emissions|VOC")
  if (nrow(nmvoc_in) > 0) {
    base_ini$OH$NMVOC_emissions <- nmvoc_in %>%
      select(date = year, NMVOC_emissions = value)
    base_ini$ozone$NMVOC_emissions <- base_ini$OH$NMVOC_emissions
  } else {
    message("Missing NMVOC emissions. Using defaults from: ", basedon)
  }

  n2on <- biogas::molMass("N2O") / biogas::molMass("N")
  # N2O_emissions
  n2o_in <- inputs %>%
    filter(Variable == "Emissions|N2O")
  if (nrow(n2o_in) > 0) {
    base_ini$N2O$N2O_emissions <- n2o_in %>%
      mutate(N2O_emissions = udunits2::ud.convert(value / n2on, "1000 t", "Tg")) %>%
      select(date = year, N2O_emissions)
  } else {
    message("Missing N2O emissions. Using defaults from: ", basedon)
  }

  udunits2::ud.convert(1, "1000 t", "kg")

  # BC emissions
  bc_in <- inputs %>%
    filter(Variable == "Emissions|BC")
  if (nrow(bc_in) > 0) {
    base_ini$bc$BC_emissions <- bc_in %>%
      mutate(BC_emissions = udunits2::ud.convert(value, "Mt", "Tg")) %>%
      select(date = year, BC_emissions)
  } else {
    message("Missing BC emissions. Using defaults from: ", basedon)
  }

  # OC emissions
  oc_in <- inputs %>%
    filter(Variable == "Emissions|OC")
  if (nrow(oc_in) > 0) {
    base_ini$oc$OC_emissions <- oc_in %>%
      mutate(OC_emissions = udunits2::ud.convert(value, "Mt", "Tg")) %>%
      select(date = year, OC_emissions)
  } else {
    message("Missing OC emissions. Using defaults from: ", basedon)
  }

}
