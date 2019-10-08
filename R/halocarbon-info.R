#' Complete halocarbon dataframe
#'
#' @return `data.frame` of halocarbon info
#' @author Alexey Shiklomanov
#' @export
halocarbon_data <- function() {
  readr::read_csv(system.file("ipcc-ar5-ch8-rf.csv", package = "hector.rcmip"),
                  col_types = "ccddd") %>%
    dplyr::mutate(molarmass = biogas::molMass(formula))
}

#' Information about a specific halocarbon
#'
#' @param halocarbon
#' @return `data.frame` with a single row containing halocarbon info
#' @author Alexey Shiklomanov
#' @export
halocarbon_info <- function(halocarbon) {
  halocarbon_data() %>%
    dplyr::filter(description == !!halocarbon)
}
