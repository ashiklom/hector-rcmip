#' Quick plot of Hector results
#'
#' @param results Hector results, as produced by [hector::fetchvars()], or a
#'   Hector core, in which case default variables will be read in.
#' @return `ggplot2` plot object
#' @author Alexey Shiklomanov
#' @export
rplot <- function(results) {
  if (inherits(results, "hcore")) {
    results <- fetchvars2(results)
  }
  ggplot2::ggplot(results) +
    ggplot2::aes(x = year, y = value, color = scenario) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(variable),
                        scales = "free_y") +
    ggplot2::theme_bw()
}
