#' Create Hector RCMIP core
#'
#' @inherit hector::newcore return params
#' @param ... Additional arguments to [hector::newcore()]
#' @author Alexey Shiklomanov
rcmip_core <- function(name = "rcmip-default", suppresslogging = TRUE, ...) {
  f <- system.file("rcmip-default.ini", package = "hector.rcmip")
  hector::newcore(f, suppresslogging = suppresslogging, name = name, ...)
}
