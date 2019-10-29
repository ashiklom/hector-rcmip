library(tidyverse)
library(fs)
library(here)
requireNamespace("fst", quietly = TRUE)

## dir_ls(here("data-raw"))

conc_long <- here("data-raw", "rcmip-concentrations-annual-means-v3-0-0.csv") %>%
  read_csv() %>%
  pivot_longer(
    matches("[[:digit:]]{4}"),
    names_to = "year",
    values_to = "value",
    names_ptypes = list(year = numeric())
  )

emiss_long <- here("data-raw", "rcmip-emissions-annual-means-v3-0-0.csv") %>%
  read_csv() %>%
  pivot_longer(
    matches("[[:digit:]]{4}"),
    names_to = "year",
    values_to = "value",
    names_ptypes = list(year = numeric())
  )

rf_long <- here("data-raw", "rcmip-radiative-forcing-annual-means-v3-0-0.csv") %>%
  read_csv() %>%
  pivot_longer(
    matches("[[:digit:]]{4}"),
    names_to = "year",
    values_to = "value",
    names_ptypes = list(year = numeric())
  )

rcmip_inputs <- bind_rows(conc_long, emiss_long, rf_long)

rcmip_inputs %>%
  filter(!is.na(value)) %>%
  # Assuming Hector can only do global
  filter(Region == "World") %>%
  fst::write_fst(
    here("inst", "rcmip-inputs.fst"),
    compress = 100
  )
