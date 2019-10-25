# devtools::install()
library(hector)
library(tidyverse)
library(hector.rcmip)
library(hectortools)

dir.create("figures", showWarnings = FALSE)

all_long <- rcmip_inputs()

tier1 <- tribble(
  ~Scenario, ~driver,
  "piControl", "concentrations",
  "esm-piControl","emissions",
  "1pctCO2","concentrations",
  "1pctCO2-4xext","concentrations",
  "abrupt-4xCO2","concentrations",
  "abrupt-2xCO2","concentrations",
  "abrupt-0p5xCO2","concentrations",
  "historical","concentrations",
  "ssp119","concentrations",
  "ssp585","concentrations",
  "esm-hist","concentrations",
  "esm-ssp119","concentrations",
  "esm-ssp585","concentrations"
)

tier1_inputs <- all_long %>%
  inner_join(tier1, "Scenario")

set_variable <- function(core, input_data, varname) {
  varconv <- readr::read_csv(system.file(
    "variable-conversion.csv",
    package = "hector.rcmip"
  ), col_types = readr::cols(.default = "c")) %>%
    dplyr::filter(rcmip_variable == !!varname)
  stopifnot(nrow(varconv) == 1)
  invar <- input_data %>%
    dplyr::filter(Variable == !!varname,
                  year > 1745)
  invar <- dplyr::arrange(invar, year)
  stopifnot(nrow(invar) > 0)
  year <- invar$year
  unit <- varconv$rcmip_udunits
  hector_unit <- varconv$hector_udunits
  hector_name <- varconv$hector_variable
  value <- udunits2::ud.convert(invar$value, unit, hector_unit)
  hector::setvar(core, year, hector_name, value, varconv$hector_unit)
  invisible(core)
}

run_concentration_scenario <- function(input, scenario, basedon = "rcp45", vars = NULL) {
  input_sub <- dplyr::filter(input, Scenario == !!scenario)
  stopifnot(nrow(input_sub) > 0)
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
  input_vars <- input_sub %>%
    dplyr::distinct(Variable) %>%
    dplyr::pull(Variable)
  for (v in input_vars) {
    hc <- tryCatch(
      set_variable(hc, input_sub, v),
      # Skip errors silently
      error = function(e) return(hc)
    )
  }
  invisible(run(hc))
  result <- tibble::as_tibble(hector::fetchvars(
    hc, dates, vars, scenario
  ))
  return(result)
}

tier1_results <- tier1_inputs %>%
  distinct(Model, Scenario) %>%
  mutate(
    result = map(Scenario, possibly(run_concentration_scenario, NULL),
                 input = tier1_inputs)
  )

tier1_results %>%
  unnest(result) %>%
  filter(year <= 2100) %>%
  ggplot() +
  aes(x = year, y = value, color = Scenario) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y") +
  scale_color_brewer(type = "qual") +
  theme_bw()

ggsave("figures/rcmip-tier1.png", width = 7, height = 7)
