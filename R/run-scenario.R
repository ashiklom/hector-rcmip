#' Run an RCMIP Scenario using Hector
#'
#' @param scenario Name of scenario
#' @param cmip6_model CMIP6 model parameter set to use. If `NULL` (default), use
#'   Hector defaults.
#' @param outfile Output file. Default is `output/zz-raw-output/single-run/<scenario>-<model>.csv`
#' @param ... Additional arguments to [set_variable()]
#' @return Hector core object at the run end date
#' @author Alexey Shiklomanov
#' @export
run_scenario <- function(scenario, cmip6_model = NULL, outfile = NULL, ...) {

  if (is.null(outfile)) {
    modeltag <- if (is.null(cmip6_model)) "default" else cmip6_model
    outfile <- file.path(
      "output", "zz-raw-output", "single-run",
      paste0(scenario, "-", modeltag, ".csv")
    )
    dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  }

  basefile <- rcmip_ini()
  ini <- hectortools::read_ini(basefile)
  if (grepl("piControl", scenario)) {
    # For preindustrial control runs, fix natural N2O emissions to a constant
    # value. 11 TgN here is the Hector preindustrial default.
    ini$N2O$N2O_natural_emissions <-
      subset(ini$N2O$N2O_natural_emissions, date == date[1])
  }

  hc <- hectortools::newcore_ini(ini, suppresslogging = TRUE, name = scenario)

  maxyear <- set_scenario(hc, scenario = scenario, ...)

  if (!is.null(cmip6_model) && cmip6_model != "default") {
    params <- cmip6_params() %>%
      dplyr::filter(model == !!cmip6_model)
    if (!nrow(params)) {
      stop("CMIP6 model ", cmip6_model, " not found.")
    }
    purrr::walk(
      c("S", "diff", "alpha", "volscl"),
      ~hector::setvar(hc, NA, .x, params[[.x]],
                      hector::getunits(.x))
    )
  }

  tryCatch(
    invisible(hector::run(hc, maxyear)),
    error = function(e) {
      stop(
        "Running scenario ", scenario,
        " failed with the following error:\n",
        conditionMessage(e)
      )
    }
  )

  write_output(
    hc, outfile,
    rcmip_scenario = scenario,
    cmip6_model = cmip6_model
  )

}

write_output <- function(core, outfile, ...) {
  mut <- rlang::list2(...)
  # year+1 here because can't access year 1 for `LAND_CFLUX` (and maybe others)
  alldates <- seq(hector::startdate(core) + 1, hector::enddate(core))
  allvars <- hector::fetchvars_all(core, dates = alldates)
  # These variables might not be in fetchvars_all
  rplusvars <- c("NOX_emissions", "CO_emissions", "NMVOC_emissions", "Tgav_ocean_ST")
  rplus <- hector::fetchvars(core, dates = alldates, rplusvars) %>%
    # Remove any variables that might have been added to fetchvars_all. This
    # ensures there are no duplicates.
    dplyr::anti_join(allvars, "variable")
  result <- allvars %>%
    dplyr::bind_rows(rplus) %>%
    dplyr::mutate(!!!mut)

  # Write result to file
  data.table::fwrite(result, outfile)
  invisible(outfile)
}

#' Add scenario data to a Hector core
#'
#' @param hc Hector core (see [hector::newcore()])
#' @inheritParams run_scenario
#' @return Hector core (invisibly)
#' @author Alexey Shiklomanov
#' @export
set_scenario <- function(hc, scenario, ...) {

  hector_vars <- rcmip2hector_df()

  hector_minyear <- 1745
  hector_maxyear <- 2100

  # Restrict inputs to the range of dates
  input_sub <- rcmip_inputs() %>%
    dplyr::filter(
      Scenario == !!scenario,
      year >= hector_minyear,
      year <= hector_maxyear
    )
  stopifnot(nrow(input_sub) > 0)

  minyear <- max(min(input_sub$year), hector_minyear)
  maxyear <- min(max(input_sub$year), hector_maxyear)
  rundates <- seq(minyear, maxyear)

  # Special case scenarios. These require inputs to be interpolated back to start date.
  abrupt_scenarios <- sprintf("abrupt-%sCO2", c("0p5x", "2x", "4x"))
  interp_scenarios <- c(
    "piControl", "1pctCO2", "1pctCO2-4xext",
    abrupt_scenarios
  )

  if (scenario %in% interp_scenarios) {
    # HACK: Need to extend the time series to get this to work properly
    input_wide <- input_sub %>%
      dplyr::select(Variable, year, value) %>%
      tidyr::pivot_wider(names_from = "Variable", values_from = "value") %>%
      tidyr::complete(year = seq(hector_minyear, hector_maxyear)) %>%
      dplyr::mutate_if(
        is.double,
        ~approxfun(year, .x, rule = 2)(year)
      )
    if (scenario %in% abrupt_scenarios) {
      c0 <- hector::fetchvars(hc, hector::startdate(hc), "Ca")[["value"]]
      input_wide <- input_wide %>%
        dplyr::mutate(
          `Atmospheric Concentrations|CO2` = dplyr::case_when(
            year < min(input_sub$year) ~ c0,
            TRUE ~ `Atmospheric Concentrations|CO2`
          )
        )
    }
    input_sub <- input_wide %>%
      tidyr::pivot_longer(-year, names_to = "Variable", values_to = "value")
  }

  # CO2
  ffi <- subset_hector_var(input_sub, "ffi_emissions")
  luc <- subset_hector_var(input_sub, "luc_emissions")
  co2 <- subset_hector_var(input_sub, "CO2_constrain")

  if (nrow(ffi) && nrow(luc)) {
    # Use FFI and LUC emissions
    hc <- set_variable(hc, ffi, ...)
    hc <- set_variable(hc, luc, ...)
  } else if (nrow(co2)) {
    # Use CO2 concentrations
    hc <- set_variable(hc, co2, ...)
    if (min(co2$year) <= hector_minyear) {
      # Also set the pre-industrial value
      hector::setvar(hc, NA, hector::PREINDUSTRIAL_CO2(),
                     co2$value[co2$year == hector_minyear], "ppm")
    }
    maxco2 <- 3500
    if (any(co2$value > maxco2)) {
      maxyear <- co2 %>%
        dplyr::filter(value < maxco2) %>%
        dplyr::pull(year) %>%
        max()
      warning(
        "Some CO2 concentrations over ", maxco2, " ppm. ",
        "Truncating to year ", maxyear, "."
      )
    }
  } else {
    warning("Scenario ", scenario, " has no CO2 data.")
  }

  # CH4
  emit <- subset_hector_var(input_sub, "CH4_emissions")
  conc <- subset_hector_var(input_sub, "CH4_constrain")
  if (nrow(emit)) {
    hc <- set_variable(hc, emit, ...)
  } else if (nrow(conc)) {
    hc <- set_variable(hc, conc, ...)
  }

  # OH and ozone
  nox_emit <- subset_hector_var(input_sub, "NOX_emissions")
  co_emit <- subset_hector_var(input_sub, "CO_emissions")
  voc_emit <- subset_hector_var(input_sub, "NMVOC_emissions")
  if (nrow(nox_emit) && nrow(co_emit) && nrow(voc_emit)) {
    # Only set these if all three are present
    hc <- set_variable(hc, nox_emit, ...)
    hc <- set_variable(hc, co_emit, ...)
    hc <- set_variable(hc, voc_emit, ...)
  }

  # N2O
  emit <- subset_hector_var(input_sub, "N2O_emissions")
  conc <- subset_hector_var(input_sub, "N2O_constrain")
  if (nrow(emit)) {
    hc <- set_variable(hc, emit, ...)
  } else if (nrow(conc)) {
    hc <- set_variable(hc, conc, ...)
  }

  # Variables that can be handled naively
  # NOTE: All of these will assume a default value of zero
  naive_vars <- c(
    "Ftalbedo", "SO2_emissions", "SV",
    "BC_emissions", "OC_emissions"
  )
  for (v in naive_vars) {
    dat <- subset_hector_var(input_sub, v)
    if (nrow(dat) > 0) {
      tryCatch(
        hc <- set_variable(hc, dat, ...),
        error = function(e) {
          stop("Hit the following error on variable ", v, ":\n",
               conditionMessage(e))
        }
      )
    } else {
      warning("Scenario ", scenario, " has no data for ", v, ". ",
              "Using default value.")
    }
  }

  # Same logic for halocarbons.
  # HACK: For now, only use the ones already defined.
  halocarbons <- hector_vars %>%
    dplyr::filter(grepl("_halocarbon", hector_component)) %>%
    dplyr::transmute(halocarbon = gsub("_halocarbon", "", hector_component),
                     halocarbon_rxp = paste0(halocarbon, "$")) %>%
    dplyr::distinct(halocarbon, halocarbon_rxp)

  halocarbon_dict <- input_sub %>%
    dplyr::distinct(Variable) %>%
    fuzzyjoin::regex_inner_join(halocarbons, c("Variable" = "halocarbon_rxp")) %>%
    dplyr::mutate(
      datatype = dplyr::case_when(
        grepl("^Atmospheric Concentrations", Variable) ~ "concentration",
        grepl("^Emissions", Variable) ~ "emissions",
        TRUE ~ "UNKNOWN"
      ) %>% factor(c("emissions", "concentration", "UNKNOWN")),
      hector_variable = paste(halocarbon, datatype, sep = "_")
    )

  # Prefer emissions, then concentration, then UNKNOWN
  halocarbon_dict <- halocarbon_dict %>%
    dplyr::group_by(halocarbon) %>%
    dplyr::arrange(datatype, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  for (i in seq_len(nrow(halocarbon_dict))) {
    irow <- halocarbon_dict[i,]
    i_rcmip_var <- irow[["Variable"]]
    i_hector_var <- irow[["hector_variable"]]
    indat <- input_sub %>%
      dplyr::filter(Variable == !!i_rcmip_var)
    tryCatch(
      hc <- set_variable(hc, indat, ...),
      error = function(e) {
        stop(
          "Error setting Hector variable ", i_hector_var,
          " / RCMIP variable ", i_rcmip_var, "."
        )
      }
    )
  }

  invisible(maxyear)

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
  if (length(unique(result$Variable)) > 1) {
    stop("Multiple matching variables found for ", hector_var)
  }
  result
}
