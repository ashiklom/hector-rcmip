#' Run Hector with specific parameter values
#'
#' @param scenario Hector scenario
#' @param pS Equilibrium climate sensitivity (`S`)
#' @param pdiff Heat diffusivity (`diff`)
#' @param palpha Aerosol scaling factor (`alpha`)
#' @param dates
#' @param include_params Logical. If `TRUE` (default), include parameter values
#'   in output object
#' @param .pb Optional [progress::progress_bar()] bar object. If `NULL`, ignore.
#' @param isamp Optional sample index
#' @param ... Additional columns to add to output
#' @inheritParams rcmip_outputs
#' @return `data.frame` of results
#' @author Alexey Shiklomanov
#' @export
run_with_param <- function(scenario, pS, pdiff, palpha,
                           dates = 1750:2100,
                           include_params = TRUE,
                           .pb = NULL,
                           isamp = NULL, ...) {

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
  if (is.null(isamp)) isamp <- uuid::UUIDgenerate()
  outfile <- file.path(
    "output", "zz-raw-output",
    "probability",
    scenario,
    paste0(isamp, ".csv")
  )
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  if (include_params) {
    write_output(core, outfile, param_ecs = pS,
                 param_diffusivity = pdiff,
                 param_volscl = palpha,
                 isamp = isamp,
                 ...)
  } else {
    write_output(core, outfile, isamp = isamp, ...)
  }
}
