#' Long RCMIP inputs data.frame
#'
#' @inheritParams generate_rcmip_inputs
#' @return
#' @author Alexey Shiklomanov
#' @importFrom magrittr %>%
#' @export
rcmip_inputs <- function(targetfile = NULL) {
  if (is.null(targetfile)) {
      targetfile <- here::here("inst", "rcmip-inputs.fst")
  }
  stopifnot(file.exists(targetfile))
  fst::read_fst(targetfile) %>%
    tibble::as_tibble()
}

#' Generate RCMIP input files
#' @param targetfile
#' @return `targetfile`, invisibly
#' @export
generate_rcmip_inputs <- function(targetfile = NULL) {
  if (is.null(targetfile)) {
    targetfile <- here::here("inst", "rcmip-inputs.fst")
  }
  stopifnot(
    requireNamespace("dplyr", quietly = TRUE),
    requireNamespace("tidyr", quietly = TRUE),
    requireNamespace("readr", quietly = TRUE)
  )
  protocol_version <- "3-1-0"
  conc_long <- here::here(
    "data-raw",
    sprintf("rcmip-concentrations-annual-means-v%s.csv", protocol_version)
  ) %>%
    readr::read_csv() %>%
    tidyr::pivot_longer(
      dplyr::matches("[[:digit:]]{4}"),
      names_to = "year",
      values_to = "value",
      names_ptypes = list(year = numeric())
    )

  emiss_long <- here::here(
    "data-raw",
    sprintf("rcmip-emissions-annual-means-v%s.csv", protocol_version)
  ) %>%
    readr::read_csv() %>%
    tidyr::pivot_longer(
      dplyr::matches("[[:digit:]]{4}"),
      names_to = "year",
      values_to = "value",
      names_ptypes = list(year = numeric())
    )

  rf_long <- here::here(
    "data-raw",
    sprintf("rcmip-radiative-forcing-annual-means-v%s.csv", protocol_version)
  ) %>%
    readr::read_csv() %>%
    tidyr::pivot_longer(
      dplyr::matches("[[:digit:]]{4}"),
      names_to = "year",
      values_to = "value",
      names_ptypes = list(year = numeric())
    )

  rcmip_inputs <- dplyr::bind_rows(conc_long, emiss_long, rf_long)

  rcmip_inputs %>%
    dplyr::filter(!is.na(value)) %>%
    # Assuming Hector can only do global
    dplyr::filter(Region == "World") %>%
    fst::write_fst(targetfile)
  invisible(targetfile)
}
