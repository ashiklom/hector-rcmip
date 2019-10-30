#' Create Hector RCMIP core
#'
#' @inherit hector::newcore return params
#' @param ... Additional arguments to [hector::newcore()]
#' @author Alexey Shiklomanov
#' @export
rcmip_core <- function(name = "rcmip-default", suppresslogging = TRUE, ...) {
  f <- rcmip_ini()
  hector::newcore(f, suppresslogging = suppresslogging, name = name, ...)
}

#' RCMIP default INI file path
#'
#' @return Full path to RCMIP default INI file
#' @author Alexey Shiklomanov
#' @export
rcmip_ini <- function() {
  system.file("rcmip-default.ini", package = "hector.rcmip")
}
