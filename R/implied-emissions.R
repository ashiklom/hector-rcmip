#' Calculate implied emissions from concentration data
#'
#' @param concdata (data.frame) Input concentration data
#' @param component (character) Name of component
#' @return
#' @author Alexey Shiklomanov
#' @export
implied_emissions <- function(concdata, component) {
  stopifnot("year" %in% colnames(concdata))
  comp_data <- halocarbon_info(component)

  dat <- concdata %>%
    dplyr::filter(grepl(paste0("\\|", component, "$"), Variable)) %>%
    dplyr::group_by(Model, Scenario, Region, Activity_Id, Mip_Era) %>%
    dplyr::arrange(year, .by_group = TRUE)

}
