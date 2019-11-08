#' Hector calibrated CMIP6 parameters
#'
#' @return
#' @author Alexey Shiklomanov
#' @export
cmip6_params <- function() {
  f <- here::here("data-raw", "cmip6_paramter_fits.csv")
  ctypes <- readr::cols(.default = "d", model = "c")
  readr::read_csv(f, col_types = ctypes) %>%
    dplyr::select(model, dplyr::everything())
}
