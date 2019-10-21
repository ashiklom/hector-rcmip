library(hector)
library(tidyverse)
library(udunits2)

dates <- 1850:2300
area_ocean <- ud.convert(3.619e8, "km2", "m2")

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
hc <- newcore(rcp45, name = "myscenario")
invisible(run(hc))

hget <- function(variable) {
  fetchvars(hc, 1850:2300, variable) %>%
    tibble::as_tibble()
}

results_tier1 <- list(
  "Atmospheric Concentrations|CH4" = hget(ATMOSPHERIC_CH4()),
  "Atmospheric Concentrations|CO2" = hget(ATMOSPHERIC_CO2()),
  # TODO: Calculate as (ffi_emissions + luc_emissions) + ATMOSPHERIC_CO2[0] - ATMOSPHERIC_CO2
  "Carbon Sequestration" = NULL,
  "Heat Uptake" = NULL,
  "Heat Uptake|Ocean" = hget(HEAT_FLUX()) %>%
    mutate(
      value = ud.convert(value * area_ocean, "W", "ZJ/year"),
      units = "ZJ/year"
    ),
  # Reference period is TODO ????
  "Surface Air Temperature Change" = hget(GLOBAL_TEMP()),
  # Reference period is TODO ????
  "Surface Ocean Temperature Change" = hget(OCEAN_SURFACE_TEMP()),
  # TODO: How to get to effective radiative forcing?
  # TODO: Cumsum of `ffi_emissions` and `luc_emissions`? Or calculate from atmospheric CO2?
  "Cumulative Emissions|CO2" = NULL,
  "Effective Radiative Forcing" = NULL,
  # TODO: Sum of RFs from various components?
  "Effective Radiative Forcing|Anthropogenic" = NULL,
  # TODO: RF_BC + ???
  "Effective Radiative Forcing|Anthropogenic|Aerosols" = NULL,
  "Effective Radiative Forcing|Anthropogenic|CO2" = hget(RF_CO2()),
  # TODO: A bunch of emissions terms in here: BC, CH4, CO, CO2, N2O, NH3, NOx, OC, Sulfur, VOC
  "Radiative Forcing" = hget(RF_TOTAL()),
  # TODO: Sum all RF components? Or RF_TOTAL - RF_VOL?
  "Radiative Forcing|Anthropogenic" = NULL,
  # TODO: RF_BC + ???
  "Radiative Forcing|Anthropogenic|Aerosols" = NULL,
  # TODO: Is this right?
  "Radiative Forcing|Anthropogenic|CO2" = hget(RF_CO2())
)

# TODO: Tier 2 results:
# - Atmospheric concentrations of gases
# - (Cumulative) Net land -> atmosphere fluxes of CH4, CO2
# - Carbon pools (atmosphere, plant, detritus, soil)
# - Net primary productivity
# - CO2 airborne fraction
# - Effective climate sensitivity and feedback
# - Heat uptake by source
# - Ocean heat content (by depth)
# - TCRE (surface air temperature / cumulative CO2 emissions)
# - Surface air ocean blended temperature change
# - Cumulative emissions of LUC, FFI, Other
# - (Effective) radiative forcings by species
# - Emissions of everything else...

results_df <- bind_rows(results_tier1, .id = "rcmip_variable") %>%
  as_tibble()

if (interactive()) {
  ggplot(results_df) +
    aes(x = year, y = value) +
    geom_line() +
    facet_wrap(vars(variable), scales = "free_y")
}

# This is close to the final form RCMIP wants
results_wide <- results_df %>%
  pivot_wider(names_from = year, values_from = value)
