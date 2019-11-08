#' Chemical symbol-aware unit conversion
#'
#' @param x
#' @param from
#' @param to
#' @return
#' @author Alexey Shiklomanov
#' @export
ud_convert2 <- function(x, from, to) {
  udunits2::ud.convert(x, parse_chem(from), parse_chem(to))
}

parse_chem <- function(unit) {
  unit2 <- unit
  rx <- "\\[.*?\\]"
  m <- regexpr(rx, unit2)
  while (m != -1) {
    regmatches(unit2, m) <- sprintf(
      "(1/%f)",
      get_molmass(regmatches(unit2, m))
    )
    m <- regexpr(rx, unit2)
  }
  unit2
}

get_molmass <- function(s) {
  biogas::molMass(gsub("\\[|\\]", "", s))
}
