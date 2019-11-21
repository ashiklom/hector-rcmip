#' Retrieve results with RCMIP variable names and units
#'
#' @param core Hector core (as returned by [run_scenario()])
#' @param ... Additional arguments to [fetchvars2()]
#' @return
#' @author Alexey Shiklomanov
#' @export
rcmip_outputs <- function(core, ...) {
  results <- fetchvars2(core, c(
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
    "NMVOC_emissions",
    hector::LAND_CFLUX(),
    hector::OCEAN_CFLUX(),
    # Anthro. RF is total - volcanic
    hector::RF_VOL(),
    # Anthro. aerosols
    hector::RF_BC(),
    hector::RF_OC(),
    hector::RF_SO2()
  ), ...)

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
      `Cumulative Emissions|CO2` = ud_convert2(
        cumsum(`Emissions|CO2`), "1/12.01 Pg", "1/44.01 Mt"
      ),
      `Carbon Sequestration` = ud_convert2(
        atm_land_flux + atm_ocean_flux,
        "Pg [C]", "Mt [CO2]"
      ),
      `Radiative Forcing|Anthropogenic` = Ftot - Fvol,
      `Radiative Forcing|Anthropogenic|Aerosols` = FBC + FOC + FSO2,
      `Emissions|N2O` = tryCatch(
        ud_convert2(
          N2O_emissions,
          "Tg [N] year-1", "Mt [N2O] year-1"
        ),
        error = function(e) NULL
      ),
      `Emissions|NOx` = ud_convert2(
        NOX_emissions,
        "Tg [N] year-1", "Mt [NO2] year-1"
      ),
      `Emissions|Sulfur` = ud_convert2(
        SO2_emissions,
        "Gg [S] year-1", "Mt [SO2] year-1"
      )
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
      # Emissions...
      `Emissions|BC` = BC_emissions,
      `Emissions|CH4` = dplyr::one_of("CH4_emissions"),
      `Emissions|CO` = CO_emissions,
      `Emissions|CO2`,
      `Emissions|N2O` = dplyr::one_of("Emissions|N2O"),
      ## `Emissions|NH3`, -- Not in Hector
      `Emissions|NOx`,
      `Emissions|OC` = OC_emissions,
      `Emissions|Sulfur`,
      `Emissions|VOC` = NMVOC_emissions,
      # Radiative forcings...
      `Radiative Forcing` = Ftot,
      `Radiative Forcing|Anthropogenic`,
      `Radiative Forcing|Anthropogenic|Aerosols`,
      `Radiative Forcing|Anthropogenic|CO2` = FCO2
    )

  results_long <- results_wide2 %>%
    tidyr::pivot_longer(-c(scenario, year),
                        names_to = "variable",
                        values_to = "value")
  results_long
}
