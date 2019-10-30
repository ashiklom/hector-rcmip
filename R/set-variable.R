#' Set Hector variable to RCMIP data
#'
#' @param core Hector core object
#' @param input_data `data.frame` of RCMIP inputs for a specific scenario.
#' @param varname RCMIP variable name. Defaults to unique `Variable` in
#'   `input_data`.
#' @param hector_vars RCMIP to Hector variable conversion table
#' @param interpolate (Logical) If `TRUE` (default), interpolate incomplete time
#'   series using [stats::approxfun()]
#' @return `core`, invisibly
#' @author Alexey Shiklomanov
#' @export
set_variable <- function(core, input_data,
                         varname = NULL,
                         hector_vars = rcmip2hector_df(),
                         interpolate = TRUE) {
  if (!(nrow(input_data) > 0)) {
    warning("Empty input data. Returning core unmodified.")
    return(core)
  }
  stopifnot(
    "Variable" %in% colnames(input_data),
    "year" %in% colnames(input_data),
    "value" %in% colnames(input_data)
  )
  if (is.null(varname)) varname <- unique(input_data[["Variable"]])
  stopifnot(length(unique(input_data[["Variable"]])) == 1)
  rundates <- seq(hector::startdate(core), hector::enddate(core))
  varconv <- dplyr::filter(hector_vars, rcmip_variable == !!varname)
  stopifnot(nrow(varconv) == 1)
  unit <- varconv$rcmip_udunits
  hector_unit <- varconv$hector_udunits
  hector_name <- varconv$hector_variable
  invar <- input_data %>%
    dplyr::filter(Variable == !!varname, year > 1745) %>%
    dplyr::arrange(year)
  if (interpolate) invar <- interpolate_var(invar)
  year <- invar$year
  value <- udunits2::ud.convert(invar$value, unit, hector_unit)
  hector::setvar(core, year, hector_name, value, varconv$hector_unit)
  invisible(core)
}

interpolate_var <- function(dat) {
  yrs <- sort(dat$year)
  if (any(diff(yrs) > 1)) {
    dat_l <- as.list(dat)
    dat_l$year <- seq(min(yrs), max(yrs))
    dat_l$value <- approxfun(yrs, dat$value)(dat_l$year)
    others <- !(names(dat_l) %in% c("year", "value"))
    dat_l[others] <- lapply(dat_l[others], unique)
    dat <- tibble::tibble(!!!dat_l)
  }
  dat
}
