#' Thin wrapper around [hector::fetchvars()] that automatically grabs start and
#' end dates
#'
#' @inherit hector::fetchvars params return
#' @param dates
#' @param ... Additional arguments to [hector::fetchvars()]
#' @author Alexey Shiklomanov
#' @export
fetchvars2 <- function(core, vars = NULL, dates = NULL, ...) {
  if (is.null(dates)) {
    dates <- seq(hector::startdate(core), hector::enddate(core))
  }
  hector::fetchvars(core, dates = dates, vars = vars, ...)
}
