library(hector.rcmip)
library(tidyverse)

scenario <- "piControl"
picontrol <- run_scenario(scenario)

results <- fetchvars2(picontrol, c(
  "CH4",
  "Ca",
  "ffi_emissions", "luc_emissions",
  "heatflux",
  "Tgav", "Tgav_ocean_ST",
  "Ftot",
  "FCO2",
  "BC_emissions",
  "CH4_emissions",
  "CO_emissions",
  "N2O_emissions",
  "NOX_emissions",
  "OC_emissions",
  "SO2_emissions",
  "NMVOC_emissions"
))

results_wide <- results %>%
  select(-units) %>%
  pivot_wider(names_from = "variable", values_from = "value")

results_wide2 <- results_wide %>%
  arrange(year) %>%
  mutate(
    `Heat Uptake|Ocean` = udunits2::ud.convert(
      # W m-2 --> W (ocean area)
      # Constants derived from `temperature_component.hpp`
      heatflux * 5100656e8 * (1 - 0.29), "W", "ZJ year-1"
    ),
    `Emissions|CO2` = ffi_emissions + luc_emissions,
    `Cumulative Emissions|CO2` = udunits2::ud.convert(
      cumsum(`Emissions|CO2`), "1/12.01 Pg", "1/44.01 Mt"
    ),
    # CCS = Cum. emissions - Atm. CO2
    # ... = -Atm. CO2 + Cum. emissions
    `Carbon Sequestration` = ((Ca - Ca[2]) * -2.13) %>%
      udunits2::ud.convert("1/12.01 Pg", "1/44.01 Mt") %>%
      `+`(`Cumulative Emissions|CO2`)
  ) %>%
  select(
    scenario = scenario,
    year = year,
    `Atmospheric Concentrations|CH4` = CH4,
    `Atmospheric Concentrations|CO2` = Ca,
    `Carbon Sequestration`,
    # `Heat uptake`
    `Heat Uptake|Ocean`,
    `Surface Air Temperature Change` = Tgav,
    `Ocean Air Temperature Change` = Tgav_ocean_ST,
    `Cumulative Emissions|CO2`,
    # Effective radiative forcings...
    `Radiative Forcing` = Ftot,
    ## `Radiative Forcing|Anthropogenic`
    ## `Radiative Forcing|Anthropogenic|Aerosols`
    `Radiative Forcing|Anthropogenic|CO2` = FCO2
  )

results_long <- results_wide2 %>%
  pivot_longer(-c(scenario, year),
               names_to = "variable",
               values_to = "value")


ggplot(results_long) +
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y")

hector::RF_CO2()

out <- list()

out[["Atmospheric Concentrations|CH4"]] <-
  fetchvars2(picontrol, "CH4")

out[["Atmospheric Concentrations|CO2"]] <-
  fetchvars2(picontrol, "Ca")

out[["Carbon Sequestration"]] <-

out[["Heat Uptake"]] <- NULL #TODO

out[["Heat Uptake|Ocean"]] <- fetchvars2(picontrol, "heatflux")

out[["Surface Air Temperature Change"]] <- fetchvars2(picontrol, "Tgav")
out[["Surface Ocean Temperature Change"]] <- fetchvars2(picontrol, "Tgav_ocean_ST")

emiss <- fetchvars2(picontrol, c("ffi_emissions", "luc_emissions"))
emiss_wide <- emiss %>%
  pivot_wider(names_from = "variable", values_from = "value")
totemiss <- emiss_wide %>%
  mutate(
    total_emissions = ffi_emissions + luc_emissions,
    cumulative_emissions = cumsum(total_emissions)
  )

out[["Cumulative Emissions|CO2"]] <- totemiss %>%
  select(scenario, year, units, value = cumulative_emissions)

out[["Effective Radiative Forcing"]] <-
