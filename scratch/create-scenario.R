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

  missing_inputs <- c()

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
    missing_inputs <- c(missing_inputs, "ffi_emissions")
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
    missing_inputs <- c(missing_inputs, "luc_emissions")
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
    missing_inputs <- c(missing_inputs, "SO2_emissions")
  }

  # Volcanic forcing
  rf_vol <- inputs %>%
    filter(Variable == "Radiative Forcing|Natural|Volcanic")
  if (nrow(rf_vol) > 0) {
    base_ini$so2$SV <- rf_vol %>%
      select(date = year, SV = value)
  } else {
    message("Missing SV. Using defaults from: ", basedon)
    missing_inputs <- c(missing_inputs, "SV")
  }

  # CH4_emissions
  ch4_in <- inputs %>%
    filter(Variable == "Emissions|CH4")
  if (nrow(ch4_in) > 0) {
    base_ini$CH4$CH4_emissions <- ch4_in %>%
      select(date = year, CH4_emissions = value)
  } else {
    message("Missing CH4 emissions. Using defaults from: ", basedon)
    missing_inputs <- c(missing_inputs, "CH4_emissions")
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
    missing_inputs <- c(missing_inputs, "NOX_emissions")
  }

  # CO_emissions
  co_in <- inputs %>%
    filter(Variable == "Emissions|CO")
  if (nrow(co_in) > 0) {
    base_ini$OH$CO_emissions <- co_in %>%
      select(date = year, CO_emissions = value)
    base_ini$ozone$CO_emissions <- base_ini$OH$CO_emissions
  } else {
    message("Missing CO emissions. Using defaults from: ", basedon)
    missing_inputs <- c(missing_inputs, "CO_emissions")
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
    missing_inputs <- c(missing_inputs, "NMVOC_emissions")
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
    missing_inputs <- c(missing_inputs, "N2O_emissions")
  }

  # BC emissions
  bc_in <- inputs %>%
    filter(Variable == "Emissions|BC")
  if (nrow(bc_in) > 0) {
    base_ini$bc$BC_emissions <- bc_in %>%
      mutate(BC_emissions = udunits2::ud.convert(value, "Mt", "Tg")) %>%
      select(date = year, BC_emissions)
  } else {
    message("Missing BC emissions. Using defaults from: ", basedon)
    missing_inputs <- c(missing_inputs, "BC_emissions")
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
    missing_inputs <- c(missing_inputs, "OC_emissions")
  }

  list(ini = base_ini, missing_inputs = missing_inputs)

}

scen2base <- tibble::tribble(
  ~Scenario, ~basedon,
  "1pctCO2", "rcp45",
  "1pctCO2-4xext", "rcp45",
  "abrupt-0p5xCO2", "rcp45",
  "abrupt-2xCO2", "rcp45",
  "abrupt-4xCO2", "rcp45",
  "esm-pi-cdr-pulse", "rcp45",
  "esm-pi-CO2pulse", "rcp45",
  "esm-piControl", "rcp45",
  "historical-cmip5", "rcp45",
  "historical-cmip6", "rcp45",
  "piControl", "rcp45",
  "rcp26", "rcp26",
  "rcp45", "rcp45",
  "rcp60", "rcp60",
  "rcp85", "rcp85",
  "ssp119", "rcp26",
  "ssp126", "rcp26",
  "ssp245", "rcp45",
  "ssp370", "rcp45",
  "ssp370-lowNTCF", "rcp45",
  "ssp434", "rcp45",
  "ssp460", "rcp60",
  "ssp534-over", "rcp45",
  "ssp585", "rcp85"
)

rcmip_ini <- rcmip_mods %>%
  left_join(scen2base, "Scenario") %>%
  mutate(dat = pmap(list(Model, Scenario, basedon), create_scenario))

bymissing <- rcmip_ini %>%
  mutate(
    missing = map(dat, "missing_inputs"),
    nmissing = map_dbl(missing, length),
    miss_chr = map_chr(missing, paste, collapse = "|")
  ) %>%
  arrange(nmissing)

bymissing %>%
  select() %>%
  print(n = Inf)

bymissing %>%
  filter(nmissing > 0) %>%
  slice(1) %>%
  pull(missing)
