library(tidyverse)
library(fs)
library(here)

conc_file <- here("data-raw", "rcmip-concentrations-annual-means-v1-0-0.csv")
conc <- read_csv(conc_file)

conc_long <- conc %>%
  pivot_longer(
    matches("[[:digit:]]"),
    names_to = "year",
    values_to = "value",
    names_ptypes = list(year = numeric())
  ) %>%
  filter(!is.na(value)) %>%
  mutate(Variable = str_remove(Variable, "^Atmospheric Concentrations\\|"))

conc_long %>%
  distinct(Variable) %>%
  pull()

# CO2, pass directly
# CH4, special case
# N2O, special case

chemtab <- read_csv(here("data-raw", "ipcc-ar5-ch8-rf.csv"),
                    col_types = "ccddd") %>%
  mutate(molarmass = biogas::molMass(formula))

# Take HFC125 as an example
species <- "HFC125"
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
    dH = value - lag(value) * expfac,
    dC = dH / (tau * (1 - expfac))
  )
dat_out %>%
  filter(year > 2200)
ggplot(dat_out) +
  aes(x = year, y = dH) +
  geom_line()


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
