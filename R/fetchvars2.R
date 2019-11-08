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
  if (!is.null(vars)) {
    purrr::map_dfr(
      vars,
      purrr::possibly(hector::fetchvars, NULL, quiet = FALSE),
      core = core,
      dates = dates,
      ...
    ) %>%
      tibble::as_tibble()
  } else {
    tibble::as_tibble(hector::fetchvars(
      core, dates = dates, vars = vars, ...
    ))
  }
}
