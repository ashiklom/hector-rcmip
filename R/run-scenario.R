#' Run an RCMIP Scenario using Hector
#'
#' @param input_data RCMIP input data
#' @param scenario Name of scenario
#' @return
#' @author Alexey Shiklomanov
#' @export
run_scenario <- function(input_data, scenario) {

  hector_vars <- rcmip2hector_df()

  input_sub <- input_data %>%
    dplyr::filter(Scenario == !!scenario)
  stopifnot(nrow(input_sub))

  dates <- sort(unique(input_sub$year))
  # HACK: Start at the first value provided by RCMIP, but no earlier than 1765
  # (because I have no Hector defaults before then)
  minyear <- max(min(dates), 1765)
  # HACK: Same as above for the last year
  maxyear <- min(max(dates), 2300)

  basefile <- system.file("input", paste0("hector_", basedon, ".ini"),
                          package = "hector")
  stopifnot(file.exists(basefile))
  ini <- hectortools::read_ini(basefile)
  ini$core$startDate <- minyear
  ini$core$endDate <- maxyear
  # HACK: This should really be `minyear`, but that doesn't work for some reason
  ini$forcing$baseyear <- minyear + 1

  hc <- hectortools::newcore_ini(ini, suppresslogging = TRUE, name = scenario)
  rundates <- seq(minyear, maxyear)

  # CO2
  ffi <- subset_hector_var(input_data, "ffi_emissions")
  luc <- subset_hector_var(input_sub, "luc_emissions")
  co2 <- subset_hector_var(input_sub, "Ca_constrain")
  if (nrow(ffi) && nrow(luc)) {
    # Use FFI and LUC emissions
    hc <- set_variable(hc, ffi)
    hc <- set_variable(hc, luc)
  } else if (nrow(co2)) {
    # Use CO2 concentrations
    hc <- set_variable(hc, co2)
  } else {
    warning("Scenario ", scenario, " has no CO2 data.")
  }

  # CH4
  emit <- subset_hector_var(input_data, "CH4_emissions")
  conc <- subset_hector_var(input_data, "CH4")
  if (nrow(emit)) {
    hc <- set_variable(hc, emit)
  } else if (nrow(conc)) {
    hc <- set_variable(hc, conc)
  }

  # OH and ozone
  nox_emit <- subset_hector_var(input_data, "NOX_emissions")
  co_emit <- subset_hector_var(input_data, "CO_emissions")
  voc_emit <- subset_hector_var(input_data, "NMVOC_emissions")
  if (nrow(nox_emit) && nrow(co_emit) && nrow(voc_emit)) {
    # Only set these if all three are present
    hc <- set_variable(hc, nox_emit)
    hc <- set_variable(hc, co_emit)
    hc <- set_variable(hc, voc_emit)
  } else {
    # HACK: Set to zero (is this correct?)
    setvar(hc, rundates, "NOX_emissions", 0, "Tg N year-1")
    setvar(hc, rundates, "CO_emissions", 0, "Tg CO year-1")
    setvar(hc, rundates, "NMVOC_emissions", 0, "Tg NMVOC year-1")
  }

  # N2O
  emit <- subset_hector_var(input_data, "N2O_emissions")
  conc <- subset_hector_var(input_data, "N2O")
  if (nrow(emit)) {
    hc <- set_variable(hc, emit)
    # Also set natural emissions. These values are the Hector defaults (linear
    # interpolation), but set manually to avoid issues with dates.
    n2o_natural_emit <- approxfun(c(1765, 2000, 2300), c(11, 8, 5))(rundates)
    setvar(hc, rundates, "N2O_natural_emissions", n2o_natural_emit)
  }

  # Variables that can be handled naively
  # NOTE: All of these will assume a default value of zero
  naive_vars <- c(
    "Ftalbedo", "SO2_emissions", "SV",
    "BC_emissions", "OC_emissions"
  )
  for (v in naive_vars) {
    dat <- subset_hector_var(input_data, v)
    if (nrow(dat) == 0) {
      warning("Scenario ", scenario, "has no data for ", v, ".")
    }
    hc <- set_variable(hc, dat, default = 0)
  }

  # Same logic for halocarbons.
  # HACK: For now, only use the ones already defined.
  halocarbons <- rcmip2hector_df() %>%
    dplyr::filter(grepl("_halocarbon", hector_component)) %>%
    dplyr::transmute(halocarbon = gsub("_halocarbon", "", hector_component)) %>%
    dplyr::distinct(halocarbon)

  halocarbon_dict <- input_data %>%
    dplyr::distinct(Variable) %>%
    fuzzyjoin::regex_inner_join(halocarbons, c("Variable" = "halocarbon")) %>%
    dplyr::mutate(
      datatype = dplyr::case_when(
        grepl("^Atmospheric Concentrations", Variable) ~ "concentration",
        grepl("^Emissions", Variable) ~ "emissions",
        TRUE ~ "UNKNOWN"
      ),
      hector_variable = paste(halocarbon, datatype, sep = "_")
    )

  for (i in seq_len(nrow(halocarbon_dict))) {
    irow <- halocarbon_dict[i,]
    ivar <- haloca
  }

  tryCatch(
    invisible(run(hc)),
    error = function(e) {
      msg <- paste("Scenario", scenario, "failed with the following error:\n",
                   conditionMessage(e))
      stop(msg)
    }
  )

  tibble::as_tibble(hector::fetchvars(
    hc, dates, vars, scenario = scenario
  ))

}

rcmip2hector_df <- function() {
  readr::read_csv(system.file(
    "variable-conversion.csv",
    package = "hector.rcmip"
  ), col_types = readr::cols(.default = "c"))
}

subset_hector_var <- function(input_data, hector_var) {
  hector_sub <- rcmip2hector_df() %>%
    dplyr::filter(hector_variable == !!hector_var)
  stopifnot(nrow(hector_sub) > 0)
  result <- input_data %>%
    dplyr::semi_join(hector_sub, c("Variable" = "rcmip_variable"))
  if(length(unique(result$Variable)) != 1) {
    stop("Multiple matching variables found for ", hector_var)
  }
  result
}
