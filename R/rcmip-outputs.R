#' Retrieve results with RCMIP variable names and units
#'
#' @param outfile Hector output file
#' @param result `data.frame` of results. If not `NULL`, use this instead of
#'   reading from `outfile`.
#' @param ... Additional arguments to [fetchvars2()]
#' @return
#' @author Alexey Shiklomanov
#' @export
rcmip_outputs <- function(outfile, result = NULL, ...) {
  vvars <- c(
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
  )
  if (is.null(result)) {
    colspec <- vroom::cols(
      scenario = "c", year = "d",
      variable = "c", value = "d", units = "c",
      scenario = "c", cmip6_model = "c"
    )
    results <- vroom::vroom(outfile, col_types = colspec) %>%
      dplyr::filter(variable %in% vvars)
  } else {
    results <- result %>%
      dplyr::filter(variable %in% vvars)
  }

  results_wide <- results %>%
    dplyr::select(-dplyr::one_of("units")) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "value")

  results_wide2 <- results_wide %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
      `Heat Uptake|Ocean` = udunits2::ud.convert(
        # W m-2 --> W (ocean area)
        # Constants derived from `temperature_component.hpp`
        heatflux * 5100656e8 * (1 - 0.29), "W", "ZJ year-1"
      ),
      `Emissions|CO2` = ud_convert2(
        ffi_emissions + luc_emissions,
        "Pg [C] year-1", "Mt [CO2] year-1"
      ),
      `Cumulative Emissions|CO2` = cumsum(`Emissions|CO2`),
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
      dplyr::one_of(c("scenario", "cmip6_model", "year", "stat")),
      `Atmospheric Concentrations|CH4` = CH4,
      `Atmospheric Concentrations|CO2` = Ca,
      `Carbon Sequestration`,
      # `Heat uptake`
      `Heat Uptake|Ocean`,
      `Surface Air Temperature Change` = Tgav,
      `Surface Ocean Temperature Change` = Tgav_ocean_ST,
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

  first_pivot <- which(colnames(results_wide2) == "Atmospheric Concentrations|CH4")
  pivot_cols <- colnames(results_wide2)[first_pivot:ncol(results_wide2)]

  results_long <- results_wide2 %>%
    tidyr::pivot_longer(
      pivot_cols,
      names_to = "variable",
      values_to = "value"
    )
  results_long
}
