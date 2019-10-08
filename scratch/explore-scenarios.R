library(tidyverse)
library(fs)
library(here)

hc <- hector::newcore(system.file("input", "hector_rcp45.ini", package = "hector"))

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

# CO2, pass directly
# CH4, special case
# N2O, special case

# Halocarbons
hcdat <- halocarbon_data()

conc_halocarbon <- conc_long %>%
  filter(grepl("Montreal Gases|F-Gases", Variable)) %>%
  mutate(gas = str_remove(Variable, ".*\\|"))
  ## left_join(halocarbon_data(), c("gas" = "description"))

halo_nested <- conc_halocarbon %>%
  select(-Variable) %>%
  group_by(Model, Scenario, Region, Activity_Id, Mip_Era,
           Unit, gas) %>%
  arrange(year, .by_group = TRUE) %>%
  nest(data = c(year, concentration)) %>%
  left_join(halocarbon_data(), c("gas" = "description"))

implied_emissions <- function(dat, lifetime, radeff, molarmass, H0 = 0) {
  stopifnot(c("year", "concentration") %in% names(dat))
  expfac <- exp(-1 / lifetime)
  dat %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
      dH = concentration - lag(concentration, default = concentration[1]) * expfac,
      dC = dH / (lifetime * (1 - expfac)),
      E = dC * 0.18 * molarmass
    )
}

halo_result <- halo_nested %>%
  mutate(data = pmap(list(data, lifetime, radeff, molarmass), implied_emissions)) %>%
  unnest(data)

ggplot(filter(halo_result, Region == "World")) +
  aes(x = year, y = E, color = interaction(Model, Scenario)) +
  geom_line() +
  facet_wrap(vars(gas), scales = "free")

halo_wide <- halo_result %>%
  ungroup() %>%
  select(Model:year, E) %>%
  mutate(gas = sprintf("%s_emissions", gas)) %>%
  pivot_wider(values_from = "E", names_from = "gas")

result_halocarbon <- conc_halocarbon %>%
  select(-Variable) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    expfac = lifetime
    dH = concentration - lag(concentration) * expfac,
    dC = dH / (tau * (1 - expfac)),
    E = dC * 0.18 * mm
  )


concdata <- conc_long

# Take HFC125 as an example
component <- "HFC125"
chemtab_sub <- chemtab %>%
  filter(description == !!species)
mm <- chemtab_sub$molarmass
tau <- chemtab_sub$lifetime
rho <- chemtab_sub$radeff
H0 <- 0
expfac <- exp(-1 / tau)
dat <- conc_long %>%
  filter(
    grepl(paste0("\\|", species, "$"), Variable),
    Model == Model[1],
    Scenario == Scenario[1],
    Region == "World"
  )

dat_out <- dat %>%
  arrange(year) %>%
  select(-c(Model:Variable), -Activity_Id) %>%
  mutate(
    ## dT = c(NA, diff(year))
  )

dat_out %>%
  filter(year > 2050)

dat_out

library(hector)
hc <- newcore(system.file("input", "hector_rcp45.ini", package = "hector"))
## setvar(hc, dat_out$year, "HFC125_emissions", dat_out$E, "kt/yr")
run(hc)
out <- fetchvars(hc, 1745:2300, hector::RF_HFC125())

ggplot(out) +
  aes(x = year, y = value) +
  geom_line()

emiss_hector <- system.file("input", "emissions", "RCP45_emissions.csv",
                            package = "hector") %>%
  read_csv(skip = 3)

ggplot(dat_out) +
  aes(x = year, y = E) +
  geom_line(aes(color = "RCMIP")) +
  geom_line(aes(x = Date, y = HFC125_emissions,
                color = "Hector"),
            data = emiss_hector)

# # Halocarbons
# HFC125, convert to emissions
# HFC134a, convert to emissions
# HFC143a, convert to emissions
# HFC227ea, convert to emissions
# HFC23, missing
# HFC245fa, convert to emissions
# HFC32, convert to emissions
# HFC4310mee, convert to emissions (drop the mee)
# C2F6, convert to emissions
# C6F14, missing
# CF4, convert to emissions
# SF6, convert to emissions
# CCl4, convert to emissions
# CFC11, convert to emissions
# CFC113, convert to emissions
# CFC114, convert to emissions
# CFC115, convert to emissions
# CFC12, convert to emissions
# CH3Br, convert to emissions
# CH3CCl3, conver to emissions
# CH3Cl, convert to emissions
# HCFC141b, missing
# HCFC142b, missing
# HCFC22, missing
# Halon1202, missing
# Halon1211, convert to emissions
# Halon1301, convert to emissions
# Halon2402, convert to emissions
# HFC152a, convert to emissions (?)
# HFC236fa, convert to emissions (?)
# HFC365mfc, missing
# NF3, missing
# C3F8, missing
# C4F10, convert to emissions (?)
# C5F12, missing
# C7F16, missing
# C8F18, missing
# cC4F8, missing
# SO2F2, missing
# CH2Cl2, missing
# CHCl3, missing
