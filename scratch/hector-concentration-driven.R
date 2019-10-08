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

ch4 <- filter(dat, Variable == "CH4")

scenario <- "ch4"
hc <- newcore(rcp45, name = scenario)
setvar(hc, ch4$year, ATMOSPHERIC_CH4(), ch4$concentration, "ppb CH4")
invisible(run(hc))
out[[scenario]] <- fetchvars(hc, dates, outvars)

out %>%
  bind_rows() %>%
  as_tibble() %>%
  ggplot() +
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y")
