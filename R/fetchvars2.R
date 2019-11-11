#' Thin wrapper around [hector::fetchvars()] that automatically grabs start and
#' end dates
#'
#' @inherit hector::fetchvars params return
#' @inherit purrr::possibly params
#' @param quiet
#' @param ... Additional arguments to [hector::fetchvars()]
#' @author Alexey Shiklomanov
#' @export
fetchvars2 <- function(core, vars = NULL, dates = NULL, quiet = FALSE, ...) {
  if (is.null(dates)) {
    dates <- seq(hector::startdate(core), hector::enddate(core))
  }
  if (!is.null(vars)) {
    purrr::map_dfr(
      vars,
      purrr::possibly(hector::fetchvars, NULL, quiet = quiet),
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
