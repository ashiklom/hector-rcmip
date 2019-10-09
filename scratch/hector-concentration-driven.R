library(hector)
library(tidyverse)
library(fs)
library(here)

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
dates <- 1850:2300
outvars <- c(GLOBAL_TEMP(), RF_TOTAL(), ATMOSPHERIC_CH4())
out <- list()

scenario <- "default"
hc45 <- newcore(rcp45, name = scenario)
invisible(run(hc45))
out[[scenario]] <- fetchvars(hc45, dates, outvars)

conc_file <- here("data-raw", "rcmip-concentrations-annual-means-v1-0-0.csv")
conc <- read_csv(conc_file) %>%
  # Hector only does global
  # Do this early to save calculations
  filter(Region == "World")

conc_long <- conc %>%
  pivot_longer(
    matches("[[:digit:]]"),
    names_to = "year",
    values_to = "concentration",
    names_ptypes = list(year = numeric())
  ) %>%
  filter(!is.na(concentration)) %>%
  mutate(Variable = str_remove(Variable, "^Atmospheric Concentrations\\|"))

# Grab the first scenario, as an example
dat <- conc_long %>%
  filter(Model == Model[1], Scenario == Scenario[1],
         Mip_Era == "CMIP5")

dat %>%
  distinct(Variable, Unit) %>%
  print(n = Inf)

scenario <- "rcmip"
hc <- newcore(rcp45, name = scenario)
hc_y0 <- startdate(hc)
varlist <- list(
  list(datvar = "CO2", hvar = "Ca_constrain", unit = "ppmv CO2"),
  list(datvar = "CH4", hvar = ATMOSPHERIC_CH4(), unit = "ppbv CH4"),
  list(datvar = "N2O", hvar = ATMOSPHERIC_N2O(), unit = "ppbv N2O"),
  list(datvar = "F-Gases|HFC|HFC125", hvar = "HFC125_concentration", unit = "pptv"),
  list(datvar = "F-Gases|HFC|HFC134a", hvar = "HFC134a_concentration", unit = "pptv"),
  list(datvar = "F-Gases|HFC|HFC143a", hvar = "HFC143a_concentration", unit = "pptv"),
  list(datvar = "F-Gases|HFC|HFC227ea", hvar = "HFC227ea_concentration", unit = "pptv"),
  list(datvar = "F-Gases|HFC|HFC23", hvar = "HFC23_concentration", unit = "pptv"),
  list(datvar = "F-Gases|HFC|HFC245fa", hvar = "HFC245fa_concentration", unit = "pptv"),
  list(datvar = "F-Gases|HFC|HFC32", hvar = "HFC32_concentration", unit = "pptv"),
  list(datvar = "F-Gases|HFC|HFC4310mee", hvar = "HFC4310_concentration", unit = "pptv"),
  list(datvar = "F-Gases|PFC|C2F6", hvar = "C2F6_concentration", unit = "pptv"),
  list(datvar = "F-Gases|PFC|CF4", hvar = "CF4_concentration", unit = "pptv"),
  list(datvar = "F-Gases|SF6", hvar = "SF6_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|CCl4", hvar = "CCl4_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|CFC|CFC11", hvar = "CFC11_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|CFC|CFC113", hvar = "CFC113_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|CFC|CFC114", hvar = "CFC114_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|CFC|CFC115", hvar = "CFC115_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|CFC|CFC12", hvar = "CFC12_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|CH3Br", hvar = "CH3Br_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|CH3CCl3", hvar = "CH3CCl3_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|CH3Cl", hvar = "CH3Cl_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|HCFC141b", hvar = "HCFC141b_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|HCFC142b", hvar = "HCFC142b_concentration", unit = "pptv"),
  ## list(datvar = "Montreal Gases|HCFC22", hvar = "HCFC22_concentration", unit = "pptv"),
  ## list(datvar = "Montreal Gases|Halon1202", hvar = "HCFC22_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|Halon1211", hvar = "halon1211_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|Halon1301", hvar = "halon1301_concentration", unit = "pptv"),
  list(datvar = "Montreal Gases|Halon2402", hvar = "halon2402_concentration", unit = "pptv")
)
for (vl in varlist) {
  d <- filter(dat, Variable == vl$datvar)
  stopifnot(nrow(d) > 0)
  y0 <- d$year[1]
  if (y0 > hc_y0) {
    # Because interpolation is disabled, assume the first few years of the
    # simulation have the same concentration as the first timestep
    c0 <- d$concentration[1]
    setvar(hc, seq(hc_y0, y0), vl$hvar, c0, vl$unit)
  }
  setvar(hc, d$year, vl$hvar, d$concentration, vl$unit)
}
invisible(reset(hc))
invisible(run(hc))
out[[scenario]] <- fetchvars(hc, dates, outvars)
out %>%
  bind_rows() %>%
  as_tibble() %>%
  filter(variable != "CH4") %>%
  ggplot() +
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y")
