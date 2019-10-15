#' Long RCMIP inputs data.frame
#'
#' @return
#' @author Alexey Shiklomanov
#' @importFrom magrittr %>%
#' @export
rcmip_inputs <- function() {
  fst::read_fst(system.file("rcmip-inputs.fst", package = "hector.rcmip")) %>%
    tibble::as_tibble()
}
