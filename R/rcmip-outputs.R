#' Retrieve results with RCMIP variable names and units
#'
#' @param core Hector core (as returned by [run_scenario()])
#' @return
#' @author Alexey Shiklomanov
#' @export
rcmip_outputs <- function(core) {
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
    dplyr::select(-units) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "value")

  results_wide2 <- results_wide %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
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
    dplyr::select(
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
    tidyr::pivot_longer(-c(scenario, year),
                        names_to = "variable",
                        values_to = "value")
  results_long
}
