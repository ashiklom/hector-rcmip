#' Set Hector variable to RCMIP data
#'
#' @param core Hector core object
#' @param input_data `data.frame` of RCMIP inputs for a specific scenario.
#' @param varname Variable name, as it appears
#' @param hector_vars RCMIP to Hector variable conversion table
#' @param default Default value, in RCMIP units. If `NULL` (default), don't set
#'   a default and throw an error if data are missing.
#' @return `core`, invisibly
#' @author Alexey Shiklomanov
#' @export
set_variable <- function(core, input_data,
                         varname = unique(input_data[["Variable"]]),
                         hector_vars,
                         default = NULL) {
  stopifnot(
    "Variable" %in% colnames(input_data),
    "year" %in% colnames(input_data),
    "value" %in% colnames(input_data),
    length(varname) == 1
  )
  rundates <- seq(hector::startdate(core), hector::enddate(core))
  varconv <- dplyr::filter(hector_vars, rcmip_variable == !!varname)
  unit <- varconv$rcmip_udunits
  hector_unit <- varconv$hector_udunits
  hector_name <- varconv$hector_variable
  stopifnot(nrow(varconv) == 1)
  invar <- input_data %>%
    dplyr::filter(Variable == !!varname,
                  year > 1745)
  invar <- dplyr::arrange(invar, year)
  if (nrow(invar) > 0) {
    year <- invar$year
    value <- invar$value
  } else {
    if (is.null(default)) {
      stop("No data for ", varname, " and no default provided.")
    }
    year <- rundates
    value <- default
  }
  value <- udunits2::ud.convert(value, unit, hector_unit)
  hector::setvar(core, year, hector_name, value, varconv$hector_unit)
  invisible(core)
}
