#' Run scenario with parameter uncertainty
#'
#' @inheritParams run_scenario
#' @param n Number of ensembles to run
#' @return Nested `data.frame` of outputs
#' @author Alexey Shiklomanov
#' @export
run_probability <- function(scenario, n = 1000, ...) {

  params <- readr::read_csv(here::here(
    "data-raw", "brick-posteriors", "emissions_17k_posteriorSamples.csv"
  ), col_types = readr::cols(.default = "d"))

  params_sub <- dplyr::sample_n(params, size = n, replace = FALSE)

  pb <- progress::progress_bar$new(total = n)
  results <- params_sub %>%
    dplyr::mutate(
      results = furrr::future_pmap(list(
        pS = S.temperature,
        pdiff = diff.temperature,
        palpha = alpha.temperature
      ), run_with_param,
      scenario = scenario, .pb = pb, ...)
    )
}

#' Run Hector with specific parameter values
#'
#' @param scenario Hector scenario
#' @param pS Equilibrium climate sensitivity (`S`)
#' @param pdiff Heat diffusivity (`diff`)
#' @param palpha Aerosol scaling factor (`alpha`)
#' @param include_params Logical. If `TRUE` (default), include parameter values
#'   in output object
#' @param .pb Optional [progress::progress_bar()] bar object. If `NULL`, ignore.
#' @inheritParams rcmip_outputs
#' @return `data.frame` of results
#' @author Alexey Shiklomanov
#' @export
run_with_param <- function(scenario, pS, pdiff, palpha,
                           dates = 1750:2100,
                           include_params = TRUE,
                           .pb = NULL, ...) {

  if (!is.null(.pb)) .pb$tick()

  ini <- hectortools::read_ini(rcmip_ini())
  if (grepl("piControl", scenario)) {
    # For preindustrial control runs, fix natural N2O emissions to a constant
    # value. 11 TgN here is the Hector preindustrial default.
    ini$N2O$N2O_natural_emissions <- subset(ini$N2O$N2O_natural_emissions, date == date[1])
  }

  core <- hectortools::newcore_ini(
    ini,
    suppresslogging = TRUE,
    name = paste0(scenario, "-p")
  )
  maxdate <- set_scenario(core, scenario = scenario)
  dates <- seq(min(dates), min(maxdate, max(dates)))
  hector::setvar(
    core, NA,
    hector::ECS(), pS,
    hector::getunits(hector::ECS())
  )
  hector::setvar(
    core, NA,
    hector::DIFFUSIVITY(), pdiff,
    hector::getunits(hector::DIFFUSIVITY())
  )
  hector::setvar(
    core, NA,
    hector::VOLCANIC_SCALE(), palpha,
    hector::getunits(hector::VOLCANIC_SCALE())
  )
  hector::run(core, maxdate)
  out <- rcmip_outputs(core, dates = dates, ...)
  if (include_params) {
    out <- dplyr::mutate(
      out,
      param_ecs = pS,
      param_diffusivity = pdiff,
      param_volscl = palpha
    )
  }
  out
}
